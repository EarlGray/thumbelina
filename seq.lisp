(def (fold f val xs)
     (if xs (fold f (f val (car xs)) (cdr xs))
            val))

(def (map f xs)
     (if xs (cons (f (car xs)) (map f (cdr xs)))
            '()))

(def (filter f? xs)
     (if xs
        (if (f? (car xs)) 
            (cons (car xs) (filter f? (cdr xs)))
            (filter f? (cdr xs)))
        '()))

(def sum (fold (+) 0))
(def product (fold (*) 0))

(def zero? (eq 0))
(def len (fold (lambda (n _) (+ 1 n)) 0))

(def (and x y) (if x (if y t nil) nil))
(def (or x y) (if x t (if y t nil)))
(def (not x) (if x nil t))
