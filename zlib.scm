;;; guile-zlib
;;; Copyright (C) 2013 David Thompson <dthompson2@worcester.edu>
;;;
;;; guile-zlib is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; guile-zlib is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; guile-zlib is a Guile wrapper for zlib.
;;
;;; Code:

(define-module (zlib)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 receive)
  #:export (compress
            uncompress
            adler32
            crc32))

(define libz (dynamic-link "libz"))

(define-syntax-rule (define-foreign name ret string-name args)
  (define name
    (pointer->procedure ret (dynamic-func string-name libz) args)))

;;
;; ZEXTERN int ZEXPORT uncompress OF((Bytef *dest, uLongf *destLen,
;;                                    const Bytef *source, uLong sourceLen));
;;
;; Decompresses the source buffer into the destination
;; buffer. sourceLen is the byte length of the source buffer. Upon
;; entry, destLen is the total size of the destination buffer, which
;; must be large enough to hold the entire uncompressed data. (The
;; size of the uncompressed data must have been saved previously by
;; the compressor and transmitted to the decompressor by some
;; mechanism outside the scope of this compression library.) Upon
;; exit, destLen is the actual size of the compressed buffer.
;;
;; uncompress returns Z_OK if success, Z_MEM_ERROR if there was not
;; enough memory, Z_BUF_ERROR if there was not enough room in the
;; output buffer, or Z_DATA_ERROR if the input data was corrupted or
;; incomplete. In the case where there is not enough room,
;; uncompress() will fill the output buffer with the uncompressed data
;; up to that point.
(define-foreign %uncompress
  int "uncompress" (list '* '* '* unsigned-long))

;;
;; ZEXTERN int ZEXPORT compress OF((Bytef *dest, uLongf *destLen,
;;                                  const Bytef *source, uLong sourceLen));
;;
;; Compresses the source buffer into the destination buffer. sourceLen
;; is the byte length of the source buffer. Upon entry, destLen is the
;; total size of the destination buffer, which must be at least the
;; value returned by compressBound(sourceLen). Upon exit, destLen is
;; the actual size of the compressed buffer.
;;
;; compress returns Z_OK if success, Z_MEM_ERROR if there was not
;; enough memory, Z_BUF_ERROR if there was not enough room in the
;; output buffer.
(define-foreign %compress
  int "compress" (list '* '* '* unsigned-long))

;;
;; ZEXTERN uLong ZEXPORT compressBound OF((uLong sourceLen));
;;
;; compressBound() returns an upper bound on the compressed size after
;; compress() or compress2() on sourceLen bytes. It would be used
;; before a compress() or compress2() call to allocate the destination
;; buffer.
(define-foreign %compress-bound
  unsigned-long "compressBound" (list unsigned-long))

;; Update a running Adler-32 checksum with the bytes buf[0..len-1] and
;; return the updated checksum.  If buf is Z_NULL, this function returns the
;; required initial value for the checksum.
;;
;;   An Adler-32 checksum is almost as reliable as a CRC32 but can be computed
;; much faster.
;;
;; Usage example:
;;
;;   uLong adler = adler32(0L, Z_NULL, 0);
;;
;;   while (read_buffer(buffer, length) != EOF) {
;;     adler = adler32(adler, buffer, length);
;;   }
;;   if (adler != original_adler) error();
(define-foreign %adler32
  unsigned-long "adler32" (list unsigned-long '* unsigned-int))

;; Update a running CRC-32 with the bytes buf[0..len-1] and return the
;; updated CRC-32.  If buf is Z_NULL, this function returns the required
;; initial value for the crc.  Pre- and post-conditioning (one's complement) is
;; performed within this function so it shouldn't be done by the application.
;;
;; Usage example:
;;
;;   uLong crc = crc32(0L, Z_NULL, 0);
;;
;;   while (read_buffer(buffer, length) != EOF) {
;;     crc = crc32(crc, buffer, length);
;;   }
;;   if (crc != original_crc) error();
(define-foreign %crc32
  unsigned-long "crc32" (list unsigned-long '* unsigned-int))

;; There is a bit of guesswork involved when creating the bytevectors
;; to store compressed/uncompressed data in. This procedure provides a
;; convenient way to copy the portion of a bytevector that was
;; actually used.
(define (bytevector-copy-region bv start end)
  (let* ((length (- end start))
         (new-bv (make-bytevector length)))
    (bytevector-copy! bv start new-bv 0 length)
    new-bv))

;; uncompress/compress take a bytevector that zlib writes the size of
;; the returned data to. This procedure saves me a few keystrokes when
;; fetching that value.
(define (buffer-length bv)
  (bytevector-u64-native-ref bv 0))

(define (uncompress bv)
  "Uncompresses bytevector and returns a bytevector containing
the uncompressed data. Returns #f on error."
  (define (try-uncompress length)
    (let* ((dest (make-bytevector (* (sizeof uint8) length)))
           (dest-length (make-bytevector (sizeof unsigned-long))))
      (bytevector-u64-native-set! dest-length 0 length)
      (values (%uncompress (bytevector->pointer dest)
                   (bytevector->pointer dest-length)
                   (bytevector->pointer bv)
                   length)
              (bytevector-copy-region dest 0 (buffer-length dest-length)))))

  ;; We don't know how much space we need to store the uncompressed
  ;; data. So, we make an initial guess and keep increasing buffer
  ;; size until it works.
  (define (step-buffer-length length)
    (inexact->exact (round (* length 1.5))))

  (let try-again ((tries 1)
                  (length (step-buffer-length (bytevector-length bv))))
    ;; Bail after so many failed attempts. This shouldn't happen, but
    ;; I don't like the idea of a potentially unbounded loop that
    ;; keeps allocating larger and larger chunks of memory.
    (if (> tries 10)
        (throw 'zlib-uncompress-error)
        (receive (ret-code uncompressed-data)
            (try-uncompress length)
          ;; return code -5 means that destination buffer was too small.
          ;; return code  0 means everything went OK.
          (cond ((= ret-code -5)
                 (try-again (1+ tries) (step-buffer-length length)))
                ((= ret-code 0)
                 uncompressed-data)
                (else
                 (throw 'zlib-uncompress-error)))))))

(define (compress bv)
  "Compresses bytevector and returns a bytevector containing the compressed data."
  (let* ((bv-length      (bytevector-length bv))
         (dest-length    (%compress-bound bv-length))
         (dest-bv        (make-bytevector dest-length))
         (dest-length-bv (make-bytevector (sizeof unsigned-long)))
         (ret-code       0))
    (bytevector-u64-native-set! dest-length-bv 0 dest-length)
    (set! ret-code
          (%compress (bytevector->pointer dest-bv)
                     (bytevector->pointer dest-length-bv)
                     (bytevector->pointer bv)
                     bv-length))
    (if (= ret-code 0)
        (bytevector-copy-region dest-bv 0
                                (buffer-length dest-length-bv))
        (throw 'zlib-compress-error))))

(define %default-adler32 (%adler32 0 %null-pointer 0))
(define %default-crc32   (%crc32   0 %null-pointer 0))

(define* (adler32 bv #:optional (value %default-adler32))
  "Computes adler32 checksum with optional starting value."
  (%adler32 value (bytevector->pointer bv) (bytevector-length bv)))

(define* (crc32 bv #:optional (value %default-crc32))
  "Computes crc32 checksum with optional starting value."
  (%crc32 value (bytevector->pointer bv) (bytevector-length bv)))
