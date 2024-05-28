; eray yuklu
; 2021400273
; compiling: yes
; complete: yes
#lang racket

(provide (all-defined-out))

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

;(newline)
;(display "************************3.1************************")  
;(newline)
;(binary_to_decimal "100101"); Output: 37
;(binary_to_decimal "11111101000"); Output: 2024


(define (relocator args limit base)
  (define (map-address address)
    (let ((decimal-address (binary_to_decimal address)))
      (if (> decimal-address limit)
          -1
          (+ decimal-address base))))
  (map map-address args))

;(newline)
;(display "************************3.2************************")  
;(newline)
;(relocator '("000010100111" "010000110001" "100100111101" "100110010001" "101111011000") 3500 1200 )
; Output: (1367 2273 3565 3649 4232)
;(relocator '("0010010110001000" "1011111000100111" "0101010100000101" "0101011101001111") 25000 400 )
; Output: (10008 -1 22165 22751)



(define (divide_address_space num page_size)
  (define m (string-length num)) ; Total number of bits in the logical address
  (define n (ceiling (/ (log (* page_size 1024)) (log 2)))) ; Number of bits for the page offset
  (define page-number-bits (- m n)) ; Number of bits for the page number

  (let* ((page-number (substring num 0 (inexact->exact page-number-bits))) ; Extract the page number
         (page-offset (substring num (inexact->exact page-number-bits) m))) ; Extract the page offset
    (list page-number page-offset)))

;(newline)
;(display "************************3.3************************")  
;(newline)
;(divide_address_space "11011011011000" 4) ; Output: ("11" "011011011000")
;(divide_address_space "1111101010110000000000" 512) ; Output: ("111" "1101010110000000000")
;(divide_address_space "10110111010010000011101110011011" 256); Output: ("10110111010010" "000011101110011011")


(define (page args page_table page_size)
  (define m (string-length (car args))) ; Total number of bits in the logical address
  (define n (ceiling (/ (log (* page_size 1024)) (log 2)))) ; Number of bits for the page offset
  (define page-number-bits (- m n)) ; Number of bits for the page number
  
  (define (split-address address)
    (let* ((page-number (substring address 0 (exact-round page-number-bits))) ; Extract the page number
           (page-offset (substring address (exact-round page-number-bits) m))) ; Extract the page offset
      (list page-number page-offset)))

  (define (lookup-frame page-number)
    (list-ref page_table (binary_to_decimal page-number)))

  (define (update-address address frame-number)
    (string-append frame-number (apply string-append address)))

  (map (lambda (arg)
         (let* ((address-parts (split-address arg)) ; Split the logical address
                (page-number (car address-parts))
                (page-offset (cdr address-parts))
                (frame-number (lookup-frame page-number))) ; Lookup frame number in the page table
           (update-address page-offset frame-number))) ; Replace page number with frame number
       args))

;(newline)
;(display "************************3.4************************")  
;(newline)
; Example usage:
;(page '("110010111011001" "000001111111010" "010001100000100" "101001011011101")'("100" "000" "010" "110" "011" "001" "111" "101") 4)
; Output: ("111010111011001" "100001111111010" "010001100000100" "001001011011101")
;(page '("01101000101111110") '("11" "00" "10" "01") 32)
; Output: ("00101000101111110")



(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (power x n)
  (if (= n 0)
      1
      (* x (power x (- n 1)))))

(define (sin-terms x n)
  (define (sin-term k)
    (* (/ (power -1 k) (factorial (+ (* 2 k) 1))) (power x (+ (* 2 k) 1))))
  
  (define (sum-terms k)
    (if (= k (- n 1))
        (sin-term k)
        (+ (sin-term k) (sum-terms (+ k 1)))))
  
  (sum-terms 0))

(define (find_sin value num)
  (sin-terms (degrees->radians value) num))

(define (degrees->radians degrees)
  (* (/ degrees 180) pi))


;(newline)
;(display "************************3.5************************")  
;(newline)
;(find_sin 45 5); output: 0.7071067829368671
;(find_sin 30 2); output: 0.49967417939436376





(define (myhash arg table_size)
  ; Calculate the sin value using the given functions
  (define sin-value
    (let* ((decimal-value (binary_to_decimal arg))
           (num-modulus (modulo decimal-value 5)))
      (find_sin decimal-value (+ num-modulus 1))))

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
  (modulo (sum_decimal_digits (string->number (extract-decimal-digits sin-value))) table_size))
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


;(newline)
;(display "************************3.6************************")  
;(newline)
;(myhash "1101" 8) ; Output: 3
;(myhash "0110101" 12); Output: 11








(define (hashed_page arg table_size page_table page_size)
  ; Convert a list containing a single string element to just the string itself
  (define (convert-to-string lst)
    (if (and (list? lst) (= (length lst) 1) (string? (car lst)))
        (car lst)
        (error "Invalid input format")))
  
  ; Step 1: Calculate page number and offset
  (define num-off (divide_address_space arg page_size))
  
  ; Step 2: Compute hash index for the page number
  (define num (car num-off))
  (define index_num (myhash num table_size))
  
  ; Step 3: Find corresponding frame number based on hash index
  (define (find-frame page-table)
    (cond
      ((null? page-table) #f)  ; Base case: If page-table is empty, return #f
      ((string=? (caar page-table) num) (cdar page-table))  ; Return the frame number
      (else (find-frame (cdr page-table)))))  ; Recursively search in the rest of the page table
  
  ; Step 4: Look up frame number in the page table
  (define page-table-entry (find-frame (list-ref page_table index_num)))
  
  ; Step 5: If frame number found, concatenate with offset to get physical address
  (if page-table-entry
      (string-append (convert-to-string page-table-entry) (convert-to-string (cdr num-off)))
      (error "No matching frame number found in the page table for the given page number.")))


;(newline)
;(display "************************3.7************************")  
;(newline)
;(hashed_page "010010111111101" 3 '( ( ("01" "000") ) ( ("11" "010") ) ( ("10" "111")) ) 8)
;(hashed_page "0101111101011001" 5 '( ( ("1101" "010") ) ( ("0111" "111") ("0101" "000")) ( ("1100" "101") ) ( ("1001" "100") ) ( ("0110" "110") ("0010" "001") ) ) 4)




(define (split_addresses args size)
  (define (split-helper str)
    (if (<= (string-length str) size)
        (list (list str))
        (cons (list (substring str 0 size))
              (split-helper (substring str size)))))
  (split-helper args))



  

;(newline)
;(display "************************3.8************************")  
;(newline)
;(split_addresses "1110110101000000100100101011000101110011" 8)
;(split_addresses "10101110101111010010101011111101" 16)
;(split_addresses "011110001101" 4)



(define (split_addresses_for_mapping args size)
  (define (split-helper str)
    (if (<= (string-length str) size)
        (list str)
        (cons (substring str 0 size)
              (split-helper (substring str size)))))
  (split-helper args))

(define (map_addresses args table_size page_table page_size address_space_size)
  ; Step 1: Split the logical addresses into chunks
  (define list-of-addresses (split_addresses_for_mapping args address_space_size))
  
  ; Step 2: Map each logical address to its corresponding physical address using hashed page table
  (define (map-helper lst return-lst)
    (if (null? lst)
        return-lst
        (map-helper (cdr lst) 
                    (append return-lst (list (hashed_page (car lst) table_size page_table page_size))))))
  
  ; Call the map-helper function with the list of addresses and an empty list as the initial return value
  (map-helper list-of-addresses '()))





;(newline)
;(display "************************3.9************************")  
;(newline)
;(map_addresses "001010000011001011000010100000011001011101001010" 5 '( ( ("1101" "010") ) ( ("0111" "111") ("0101" "000") ) ( ("1100" "101") ) ( ("1001" "100") ) ( ("0110" "110") ("0010" "001") )) 4 16)
;(newline)




