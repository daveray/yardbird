;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; http://darevay.com/overtone/minmay.mp3
(do 
  (use 'overtone.core)
  (boot-external-server)
  (use 'yardbird.core :reload))

; ubiquitous beep
(definst beep [note 60 vol 0.4]
  (let [freq (midicps note)
        src (sin-osc freq)
        env (env-gen (perc) :action FREE)]
    (* vol src env)))

; 3 nice notes
(def notes (map note [:F4 :G4 :A4]))

; it's hard to go wrong with a pentatonic
(defn trans [off] (diatonic-transpose :D :minor-pentatonic off))

(def m (metronome 250))
(def p (note-player :inst beep :metro m))

(p (->> (range 7) ; some numbers to drive things
     cycle
     (map #(map (one-of 
                  (fn [_])                  ; no note
                  (trans %)                 ; tranpose up
                  (trans (- %))             ; transpose down
                  (absolute-transpose -12)  ; transpose way down
                  (absolute-transpose 24))  ; transpose way up
                notes))))

; Some controls
(m :bpm (* (m :bpm) 2))
(m :bpm (/ (m :bpm) 2))
(m :bpm (+ (m :bpm) 20))
(m :bpm (- (m :bpm) 20))
(stop)

