(def (fold f val xs)
    (if xs (fold f (f (car xs) val) (cdr xs))
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
