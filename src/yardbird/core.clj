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
        [yardbird.util]))

(defn next-measure [beat beats-per-measure]
  (let [m (mod beat beats-per-measure)]
    (if (> m 0)
      (+ beat (- beats-per-measure m))
      beat)))

(defn split-opts [[f & more :as args]]
  (if (map? f)
    [f more]
    [{} args]))

(defn note-player
  "Returns a function which plays one or more sequences of notes with the given
  instrument. Keyword args are:
  
    :inst The default instrument function. Must take a single midi note value.
    :metro The metronome to use. If omitted, an anonymous 120bmp metronome
           is created and used.
    :stop? If true, calls (stop) each time the the player function is called.
           This is a convenience for live-coding. Defaults to true.
    :ts    Time signature, e.g. [6 8], [4 4]. If the player is called multiple
           times, they'll all start on the first beat of the measure.

  The returned function takes an optional option map followed by one or more
  note seqs. The option map has the following options:
    
    :on The beat to start on. Defaults to 0.

  Example:
    
    ; Define a metronome
    (def m (metronome 200))

    ; Define the player
    (def play (note-player :inst my-piano :metro m))

    ; Now use it to play some note sequences in parallel
    (play [60 61 62] [64 65 66])
    ; or to start on the third beat of the measure ...
    (play {:on 3} [60 61 62] [64 65 66])
  "
  [& {:keys [inst metro stop? ts] 
      :or {metro (metronome 120) 
           stop? true
           ts [4 4]}}]
  (let [[beats _] ts
        player (fn play [beat note-seqs]
                (when (some :notes note-seqs)
                  (let [t (metro beat)]
                    (doseq [note-seq note-seqs]
                      (let [i (or (:inst note-seq) inst)]
                        (doseq [n (as-notes (first (:notes note-seq)))]
                          (when n
                            (at t (i n))))))
                    (apply-at 
                      (metro (inc beat)) 
                      play 
                      [(inc beat) (map #(update-in % [:notes] next) note-seqs)]))))]
    (fn [& args]
      (let [[{:keys [on] :or {on 0} :as opts} note-seqs] (split-opts args)] 
        (when stop? (stop))
        (player (+ (next-measure (metro) beats) on) 
                (map #(if-not (map? %) 
                        (hash-map :notes %) %) note-seqs))))))

(defn absolute-transpose 
  "Returns a function that transposes a note by off semi-tones"
  [off]
  (fn [n]
    (if n (+ n off))))

(defn- up-diatonic-transpose-table [scale]
  ; First fill a vector with offsets for each scale tone, i.e. a[i]
  ; is the delta between interval i and i+1.
  (let [a (first (reduce (fn [[acc i] d]
                            [(assoc acc i d) (+ i d)])
                         [(vec (repeat 12 0)) 0]
                         scale))
        
        ; Now fill in the zeros with the delta up to the next scale
        ; tone.
        b (first (reduce (fn [[acc n] i] 
                    (if (zero? (acc i)) 
                      [(assoc acc i (- n i)) n] [acc i])) 
                  [a (count a)] 
                  (range (dec (count a)) -1 -1)))]
    b))

(defn- down-diatonic-transpose-table [scale]
  (let [a (reverse (map - (up-diatonic-transpose-table (reverse scale))))]
    (vec (concat (drop 11 a) (take 11 a)))))

(defn diatonic-transpose 
  "Returns a function that transposes a note diatonically in the given
  key by off steps. For instance:
  
    ((diatonic-transpose :C :major 1) 60)
    ;=> 62   
    ; i.e. C goes up one whole step to D
    ((diatonic-transpose :C :major -1) 60)
    ;=> 59  
    ; i.e. C goes down one half-step to B

  Non-diatonic notes go to their nearest diatonic neighbor and then
  shift from there.

  See:
    (overtone.music.pitch/resolve-scale)
  "
  [key scale off]
  (let [scale   (resolve-scale scale)
        up      (up-diatonic-transpose-table scale)
        down    (down-diatonic-transpose-table scale)
        key-off (NOTES key)
        adjust  (fn [table note n]
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
  up or down by offset.
  
  See:
    (yardbird.core/diatonic-transpose)
  "
  ([key scale] (diatonic-side-slip key scale 1))
  ([key scale off] 
   (one-of 
     (diatonic-transpose key scale off) 
     (diatonic-transpose key scale (- off)))))

(defn absolute-invert 
  ([]
   (let [helper (atom nil)]
     (fn [n]
       (when n
         (swap! helper #(if-not % (absolute-invert n) %)) 
         (@helper n)))))
  ([pivot]
    (fn [n]
      (if n
        (- pivot (- n pivot))))))

(defn stretch 
  "Given a sequence of notes, returns a lazy 'stretched' version of the sequence 
  by inserting (- s 1) nils between each note."
  [s notes]
  (let [filler (repeat (dec s) nil)] 
    (mapcat
      #(cons % filler)
      notes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment 
(boot-external-server)

(use 'overtone.inst.piano)
(defn p
  ([note] (p note 50))
  ([note vel] (piano note 1 (+ vel (rand-int 10)) 0.3 0.1 0.5)))

(definst beep [note 60 vol 0.35]
  (let [freq (midicps note)
        src (sin-osc freq)
        env (env-gen (perc ) :action FREE)]
    (* vol src env)))

(def gs1 (mapcat #(map note %) (load-solo :gs1)))
(def gs1-changes 
  [:B4  :D4  :G4  :Bb4 :Eb4 :Eb4 :A4  :D4  
   :G4  :Bb4 :Eb4 :F#4 :B4  :B4  :F4  :Bb4 
   :Eb4 :Eb4 :A4  :D4  :G4  :G4  :C#4 :F#4 
   :B4  :B4  :F4  :Bb4 :Eb4 :Eb4 :C#4 :F#4])

(def gs1-bass-line
  (let [roots (->> gs1-changes
                (map note)
                (map (absolute-transpose -12)))
        fifths (map (absolute-transpose 7) roots)
        line   (interleave roots fifths)]
    (cycle (stretch 2 line)))) ; stretch to match solo eighth notes

((note-player :dt 100) 
   { :inst p    :notes (map (juxt identity (absolute-transpose 7)) gs1)}
   { :inst beep :notes gs1-bass-line })

(stop)

(def mhall (map note [:E4 :D4 :C4 :D4 :E4 :E4 :E4 nil
                      :D4 :D4 :D4 nil :E4 :G4 :G4 nil
                      :E4 :D4 :C4 :D4 :E4 :E4 :E4 :D4 
                      :D4 :E4 :D4 :C4 nil]))
(def pl (note-player :inst p :dt 100))
(pl (stretch 2 (take 24 ceotk))
    { :inst beep 
      :notes (interleave (repeat nil) (map (absolute-transpose -12) ceotk))})
(stop)
(pl (map (absolute-transpose 12) ceotk)
    (take 32 (map (diatonic-transpose :C :major 2) ceotk))
    (take 32 (map (absolute-transpose -24) (stretch 3 ceotk))))

; As 2 parallel seqs
(pl (map (absolute-invert) ceotk)
    (map (diatonic-transpose :C :major -2) ceotk))
; or one seq of 2-elemet vectors ...
(pl (map (juxt (absolute-invert) 
               (diatonic-transpose :C :major -2)) ceotk))

(pl (take 24 (interleave (map (diatonic-transpose :C :major 7) ceotk) ceotk)))
(pl (map (diatonic-transpose :C :major -2) ceotk)
    (map (absolute-transpose 24) ceotk))
(pl (map (one-of (side-slip 1) identity) mhall))

(pl (interleave mhall (map (diatonic-transpose :C :major 2) mhall)))


(note-player p (+ (now) 100) 100 (map note (first gs1)))
(note-player p (+ (now) 100) 100 (map (absolute-transpose 12) (map note (second gs1))))
(let [notes (map note (first gs1))] 
  (pl (interleave (map (absolute-transpose 12) notes)
      (map (absolute-transpose 19) notes))
      (stretch 2 (map (absolute-transpose 26) notes)))) 
)
