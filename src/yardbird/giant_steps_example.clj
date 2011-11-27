;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns yardbird.giant-steps-example
  (:use [overtone.core]
        [overtone.inst synth drum piano]
        [yardbird.dmfetd]
        [yardbird.core]))

(boot-external-server)

; Make a bass-like instrument (aka beep)
(definst my-bass [note 60 vol 0.35]
  (let [freq (midicps note)
        src (sin-osc freq)
        env (env-gen (perc ) :action FREE)]
    (* vol src env)))

; Use MdaPiano ugen
(defn my-piano 
  ([note] (my-piano note 50))
  ([note vel] (piano note 1 (+ vel (rand-int 10)) 0.3 0.1 0.5)))

; Grab all the notes in the original Giant Steps solo
(def gs1 (mapcat #(map note %) (load-solo :gs1)))

; Roots of the chord changes, 2 beats each
(def gs1-changes 
  [:B4  :D4  :G4  :Bb4 :Eb4 :Eb4 :A4  :D4  
   :G4  :Bb4 :Eb4 :F#4 :B4  :B4  :F4  :Bb4 
   :Eb4 :Eb4 :A4  :D4  :G4  :G4  :C#4 :F#4 
   :B4  :B4  :F4  :Bb4 :Eb4 :Eb4 :C#4 :F#4])

; Build an infinite, dumb 1-5 bass line
(def gs1-bass-line
  (let [roots (->> gs1-changes
                (map note)
                (map (absolute-transpose -12)))   ; bassier
        fifths (map (absolute-transpose 7) roots) ; calculate fifths
        line   (interleave roots fifths)]         ; mix roots and fifths
    (cycle (stretch 2 line)))) ; stretch to match solo eighth notes

((note-player :dt 100) 
   ; Play the solo with the piano, harmonized in absolute fifths
   { :inst my-piano :notes (map (juxt identity (absolute-transpose 7)) gs1)}

   ; Play the bass-line
   { :inst my-bass  :notes gs1-bass-line })

(stop)

