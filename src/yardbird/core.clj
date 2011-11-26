;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns yardbird.core
  (:use [overtone.core]
        [overtone.inst synth drum]
        [yardbird.dmfetd]
        [yardbird.util]))

(defn note-player 
  "Starting at time t, play sequence of notes dt apart with inst."
  [inst t dt notes]
  (when notes
    (let [next-t (+ t dt)
          next-note (first notes)]
      (doseq [n (as-notes next-note)] 
        (when n 
          (at t (inst n))))
      (apply-at next-t #'note-player [inst next-t dt (next notes)]))))

(defn absolute-transpose 
  "Returns a function that transposes a note by off semi-tones"
  [off]
  (fn [n]
    (if n (+ n off))))

(defn diatonic-transpose 
  "Returns a function that transposes a note diatonically in the given
  key by off steps. For instance:
  
    ((diatonic-transpose :C 60) 1)
    ;=> 62   
    ; i.e. C goes up one whole step to D
    ((diatonic-transpose :C 60) -1)
    ;=> 59  
    ; i.e. C goes down one half-step to B

  Non-diatonic notes go to their nearest diatonic neighbor and then
  shift from there.
  "
  [key off]
             ;0   1  2  3  4  5  6  7  8  9  10  11
  (let [up   [2   1  2  1  1  2  1  2  1  2  1   1]
        down [-1 -1 -2 -1 -1 -2 -1 -2 -1 -2 -1  -2]
        key-off (NOTES key)
        adjust (fn [table note n]
                 (if (zero? n)
                   note
                   (recur table (+ note (table (mod note 12))) (dec n))))] 
    (fn [n]
      (if n
        (+ key-off 
           (adjust (if (pos? off) up down) 
                   (- n key-off) 
                   (Math/abs off)))))))

(defn one-of 
  "Returns a function that randomly applies one of the functions in
  transforms."
  [& transforms]
  (fn [n] ((choose transforms) n)))

(defn side-slip 
  "Returns a function that randomly shifts a note up or down by offset."
  ([] (side-slip 1))
  ([off] (one-of (absolute-transpose off) (absolute-transpose (- off)))))

(defn diatonic-side-slip
  "Returns a function that randomly shifts a note diatonically in key
  up or down by offset."
  ([key] (diatonic-side-slip key 1))
  ([key off] 
   (one-of 
     (diatonic-transpose key off) 
     (diatonic-transpose key (- off)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(boot-external-server)

(use 'overtone.inst.piano)
(defn p
  ([note] (p note 50))
  ([note vel] (piano note 1 (+ vel (rand-int 10)) 0.3 0.1 0.5)))

(definst beep [note 60 vol 0.5]
  (let [freq (midicps note)
        src (sin-osc freq)
        env (env-gen (perc ) :action FREE)]
    (* vol src env)))

(def gs1 (load-solo :gs1))
(def rrryb (cycle (map note [:C4 nil nil :C4 nil nil :C4 nil nil :D4 :E4 nil
            :E4 nil :D4 :E4 nil :F4 :G4 nil nil nil nil nil
            :C5 :C5 :C5 :G4 :G4 :G4 :E4 :E4 :E4 :c4 :C4 :C4
            :G4 nil :F4 :E4 nil :D4 :C4 nil nil nil nil nil])))
(def mhall (map note [:E4 :D4 :C4 :D4 :E4 :E4 :E4 nil
                      :D4 :D4 :D4 nil :E4 :G4 :G4 nil
                      :E4 :D4 :C4 :D4 :E4 :E4 :E4 :D4 
                      :D4 :E4 :D4 :C4 nil]))

(note-player beep (+ (now) 100) 200 (map (one-of (side-slip 1) identity) mhall))

(note-player p (+ (now) 100) 200 (interleave mhall (map (diatonic-transpose :C 2) mhall)))

(note-player p (+ (now) 100) 200 (take 64 rrryb))

(note-player p (+ (now) 100) 150 
             (let [no-nils (take 32 (filter identity (map note (first gs1))))]
               (interleave
                    (invert no-nils)
                    no-nils)))

(note-player beep (+ (now) 100) 200 (take 48
                                       (map (juxt identity (diatonic-transpose :C 2)(diatonic-transpose :C 4)) rrryb)))

(note-player p (+ (now) 100) 200 (take 48 (map vector (drop 12 rrryb) rrryb)))

(note-player p (+ (now) 100) 100 
             (take 128 (interleave rrryb (map (diatonic-transpose :C 2) rrryb))))

(note-player p (+ (now) 100) 300
             (interleave (scale :C4 :major) 
                         (map (diatonic-transpose :C 2) (scale :C4 :major))))
(stop)
(note-player p (+ (now) 100) 100 (map note (first gs1)))
(note-player p (+ (now) 100) 100 (map (absolute-transpose 12) (map note (second gs1))))
(note-player beep (+ (now) 100) 100 
             (map cons 
                  (->> (map note (second gs1))
                    (map (absolute-transpose 12)))
                  (->> (map note (first gs1)) 
                    (map (absolute-transpose 24))
                    (map (juxt identity (absolute-transpose 7) (absolute-transpose 14)))))) 

