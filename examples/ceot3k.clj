;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; A mysterious melody I can't get out of my head
(do 
  (use '[overtone.core])
  ;(boot-external-server)
  (use '[yardbird.core] 
       '[yardbird.sounds] :reload)) 

(def ceot3k (cycle (map note [:D5 :E5 :C5 :C4 :G4 :G4 :G4])))

(def m (metronome 200))
(def p (note-player :metro m 
                    :inst #(clarinet % :dur 1.0) 
                    :ts [7 4] 
                    :stop? false))

(recording-start "~/Desktop/ceot3k.wav")
(p ceot3k)
(p {:on 0} {:inst beep :notes (map (absolute-transpose -12) ceot3k)})
(p {:on 0} {:inst beep :notes (->> ceot3k
                                (map (absolute-transpose -12))
                                (map (diatonic-transpose :C :major 4))
                                (stretch 2))})
(p {:on 0.5} (map (diatonic-transpose :C :major 2) ceot3k))
(p {:on 0.25} (map (diatonic-transpose :C :major 6) ceot3k))
(p {:on 0.75} (map (diatonic-transpose :C :major 1) ceot3k))
(recording-stop)

; Some controls
(m :bpm (* (m :bpm) 2))
(m :bpm (/ (m :bpm) 2))
(m :bpm (+ (m :bpm) 20))
(m :bpm (- (m :bpm) 20))
(stop)

