(def (abs x)
  (if (< x 0.0)  (- 0.0 x) x))

(def (average x y) (/ (+ x y) 2.0))
(def (sqr x) (* x x))

(def (sqrt x)
  (def (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (def (good-enough? guess x)
    (< (abs (- (sqr guess) x)) 0.000001))
  (def (improve guess x)
    (average guess (/ x guess)))
  (sqrt-iter 1.0 x))
    

(def (cube x) (* x (sqr x)))

(def (sine angle)
    (def (p x)
        (- (* 3.0 x) (* 4.0 (cube x))))
    (if (< (abs angle) 0.00001)
        angle
        (p (sine (/ angle 3.0)))))
