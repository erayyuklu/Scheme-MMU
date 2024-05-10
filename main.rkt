; name surname
; student id
; compiling: yes
; complete: yes
#lang racket

(define (binary_to_decimal binary)
  (let loop ((binary-list (reverse (string->list binary))) (power 0) (decimal 0))
    (cond
      ((null? binary-list) decimal)
      ((char=? (car binary-list) #\1)
       (loop (cdr binary-list) (+ power 1) (+ decimal (expt 2 power))))
      ((char=? (car binary-list) #\0)
       (loop (cdr binary-list) (+ power 1) decimal))
      (else
       (error "Input is not a binary number")))))

(newline)
(display "************************3.1************************")  
(newline)
(display (binary_to_decimal "100101")) 
; Output: 37
(newline)

(display(binary_to_decimal "11111101000")) 
; Output: 2024
(newline)

(define (relocation_mapping args limit base)
  (define (map-address address)
    (let ((decimal-address (binary_to_decimal address)))
      (if (> decimal-address limit)
          -1
          (+ decimal-address base))))
  (map map-address args))

(newline)
(display "************************3.2************************")  
(newline)

(display(relocation_mapping '("000010100111" "010000110001" "100100111101" "100110010001" "101111011000") 3500 1200 ))
; Output: (1367 2273 3565 3649 4232)
(newline)

(display(relocation_mapping '("0010010110001000" "1011111000100111" "0101010100000101" "0101011101001111") 25000 400 ))
; Output: (10008 -1 22165 22751)
(newline)


(define (divide_address_space address page-size)
  (define m (string-length address)) ; Total number of bits in the logical address
  (define n (ceiling (/ (log (* page-size 1024)) (log 2)))) ; Number of bits for the page offset
  (define page-number-bits (- m n)) ; Number of bits for the page number

  (let* ((page-number (substring address 0 (inexact->exact page-number-bits))) ; Extract the page number
         (page-offset (substring address (inexact->exact page-number-bits) m))) ; Extract the page offset
    (list page-number page-offset)))

(newline)
(display "************************3.3************************")  
(newline)

(display (divide_address_space "11011011011000" 4)) ; Output: ("11" "011011011000")
(newline)
(display (divide_address_space "1111101010110000000000" 512)) ; Output: ("111" "1101010110000000000")
(newline)
(display (divide_address_space "10110111010010000011101110011011" 256)) ; Output: ("10110111010010" "000011101110011011")
(newline)

(define (page args page-table page-size)
  (define m (string-length (car args))) ; Total number of bits in the logical address
  (define n (ceiling (/ (log (* page-size 1024)) (log 2)))) ; Number of bits for the page offset
  (define page-number-bits (- m n)) ; Number of bits for the page number
  
  (define (split-address address)
    (let* ((page-number (substring address 0 (exact-round page-number-bits))) ; Extract the page number
           (page-offset (substring address (exact-round page-number-bits) m))) ; Extract the page offset
      (list page-number page-offset)))

  (define (lookup-frame page-number)
    (list-ref page-table (binary_to_decimal page-number)))

  (define (update-address address frame-number)
    (string-append frame-number (apply string-append address)))

  (map (lambda (arg)
         (let* ((address-parts (split-address arg)) ; Split the logical address
                (page-number (car address-parts))
                (page-offset (cdr address-parts))
                (frame-number (lookup-frame page-number))) ; Lookup frame number in the page table
           (update-address page-offset frame-number))) ; Replace page number with frame number
       args))

(newline)
(display "************************3.4************************")  
(newline)

; Example usage:
(display (page '("110010111011001" "000001111111010" "010001100000100" "101001011011101")
               '("100" "000" "010" "110" "011" "001" "111" "101") 4))
; Output: ("111010111011001" "100001111111010" "010001100000100" "001001011011101")
(newline)
(display (page '("01101000101111110") '("11" "00" "10" "01") 32))
; Output: ("00101000101111110")
(newline)


(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (power x n)
  (if (= n 0)
      1
      (* x (power x (- n 1)))))

(define (sin-terms x n)
  (if (= n 0)
      x
      (+(* (/ (power -1 n) (factorial (+ (* 2 n) 1) )) (power x (+ (* 2 n) 1))) (sin-terms x (- n 1)))))

(define (find_sin angle num-terms)
  (sin-terms (degrees->radians angle) num-terms))

(define (degrees->radians degrees)
  (* (/ degrees 180) pi))


(newline)
(display "************************3.5************************")  
(newline)
(display(find_sin 45 5)); output: 0.7071067811796194
(newline)
(display(find_sin 30 2)); output: 0.5000021325887924
(newline)




(define (myhash arg table-size)
  ; Calculate the sin value using the given functions
  (define sin-value
    (let* ((decimal-value (binary_to_decimal arg))
           (num-modulus (modulo decimal-value 5)))
      (find_sin decimal-value num-modulus)))

  ; Extract the first ten digits after the decimal point
 (define (extract-decimal-digits value)
  (let* ((decimal-str (number->string value))
         (decimal-parts (string-split decimal-str ".")))
    (if (>= (length decimal-parts) 2)
        (let ((fraction-part (cadr decimal-parts)))
          (if (>= (string-length fraction-part) 10)
                (string-take fraction-part 10)
                (string-append fraction-part (make-string (- 10 (string-length fraction-part)) #\0))))
        "0000000000"))) ;; Return default value if decimal parts are not present

  ; Calculate the hash value  
  (modulo (sum_decimal_digits (string->number (extract-decimal-digits sin-value))) table-size))
; Define string-take function manually
(define (string-take str n)
  (substring str 0 (min n (string-length str))))

(define (string-drop-right str n)
  (substring str 0 (- (string-length str) n)))

(define (string-take-right str n)
  (substring str (- (string-length str) n)))

(define (sum_decimal_digits n)
  (let loop ((num-str (number->string n)) (total 0))
    (if (string=? num-str "")
        total
        (loop (string-drop-right num-str 1)
              (+ total (string->number (string-take-right num-str 1)))))))


(newline)
(display "************************3.6************************")  
(newline)

(display (myhash "1101" 8)) ; Output: 3
(newline)
(display (myhash "0110101" 12)) ; Output: 11
(newline)
(newline)

