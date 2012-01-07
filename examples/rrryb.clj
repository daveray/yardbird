;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Your boat, row it. Row it gently down the stream.
(do 
  (use '[overtone.core]
       '[overtone.inst piano])
  (boot-external-server)
  (use '[yardbird.core] :reload
       '[yardbird.giant-steps])) 

; Use MdaPiano ugen
(defn my-piano 
  ([note] (my-piano note 80))
  ([note vel] (piano note 1 (+ vel (rand-int 10)) 0.3 0.1 0.5)))

(def rrryb 
  (->> [:C4 nil nil :C4 nil nil :C4 nil nil :D4 :E4 nil
        :E4 nil :D4 :E4 nil :F4 :G4 nil nil nil nil nil
        :C5 :C5 :C5 :G4 :G4 :G4 :E4 :E4 :E4 :c4 :C4 :C4
        :G4 nil :F4 :E4 nil :D4 :C4 nil nil nil nil nil]
    (map note)
    cycle))

(def m (metronome 300))
(def p (note-player :metro m :inst my-piano :ts [12 8] :stop? false))

; Hit this to start new rounds. The :ts above will ensure they wait
; and start at the beginning of the measure
(p {:on 0} rrryb)
(p {:on 0} (map (diatonic-transpose :C :major 6) rrryb))

; Some controls
(m :bpm (* (m :bpm) 2))
(m :bpm (/ (m :bpm) 2))
(m :bpm (+ (m :bpm) 20))
(m :bpm (- (m :bpm) 20))
(stop)

