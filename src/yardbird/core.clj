(ns yardbird.core
  (:use [overtone.live]
        [yardbird.dmfetd]))

(def gs1 (load-solo :gs1))
(def first-chorus (first gs1))

(definst beep [note 60 vol 0.5]
  (let [freq (midicps note)
        src (sin-osc freq)
        env (env-gen (perc ) :action FREE)]
    (* vol src env)))

(let [t (+ 1000 (now))] 
  (doseq [[i n] (map-indexed vector (map note first-chorus))]
    (when n
      (at (+ t (* 100 i)) (beep (+ 36 n))))))
