;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns yardbird.util
  (:require [overtone.music.pitch :as pitch]))

(defn as-notes 
  "Make a sequence of notes."
  [n]
  (if (number? n) [n] n))

(defn midi-note-to-pitch 
  "Convert a midi number to pitch, e.g.
  
    (midi-note-to-pitch 60)
    ;=> :C4
  "
  [n]
  (if n
    (let [p (pitch/REVERSE-NOTES (mod n 12))
        off (dec (int (/ n 12)))]
      (keyword (str (name p) off)))))

