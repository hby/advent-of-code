(ns advent-of-code.2016.day19
  (:require [clojure.set :as cset])
  (:import (java.util ArrayList)))


(def input 3014387)

(declare dropping)

(defn skipping
  [order remain]
  (if (= 1 (count remain))
    (first remain)
    #(dropping
       (->> (drop-while (fn [x] (not (remain x))) order)
            (drop 1))
       remain)))

(defn dropping
  [order remain]
  (if (= 1 (count remain))
    (first remain)
    (let [[rem & r] (drop-while (fn [x] (not (remain x))) order)]
      #(skipping r (cset/difference remain #{rem})))))

(defn part1
  [n]
  (let [remain (set (range 1 (inc n)))
        order (cycle (range 1 (inc n)))]
       (trampoline skipping order remain)))

(comment
  (time (part1 input)))
  ;; Elapsed time: 17523.845465 msecs
  ;; => 1834471

;; August 2021
;; I was looking for good problems to use when teaching
;; a Clojure Crash Course and started scanning through these
;; old problems for something interesting.
;;
;; I noticed that I did part 1 of this problem but not part 2
;; and thought I'd should be able to quickly finish it.
;; As Bill Cosby's Noah would say, "Right."
;;
;; First thing I noticed was how much the way I think
;; in Clojure has changed in 4.5 years.
;;
;; I redid part1 as this

(defn step
  [s]
  (cond-> (map first (partition-all 2 s))
          (odd? (count s)) (#(cons (last %) (butlast %)))))

(defn part1-redux
  [[f & r :as s]]
  (if r
    (recur (step s))
    f))


(comment
  (time (part1-redux (range 1 (inc input)))))
  ;; "Elapsed time: 2533.875487 msecs"
  ;; => 1834471

;; Faster, too!

;; Now for part 2

;; My first thought was to just plow straight ahead and
;; simulate each step in the operation.
;; 0. Given a data structure where I can mark a position
;;    and be able to advance the position and remove the
;;    element at a position:
;; 1. Hold a position, p, and the count of elements, c.
;; 2. Compute offset = (int (Math/floor (/ c 2)))
;; 3. Remove element at p + offset
;; 4. Advance position and decrement count.
;; 5. Repeat at 2 with new p and c.

;; So, let's just stick with vectors and use subvec to slice
;; and dice it.

(defn offset
  "Delta from a reference location to the element to remove."
  [c]
  (int (Math/floor (/ c 2))))

(defn remove-and-shift-vec
  [v]
  (let [off (offset (count v))
        v' (into (subvec v 1 off) (subvec v (inc off)))]
    (assoc v' (count v') (first v))))

(defn part2-rasv
  [[f & r :as s]]
  (if r
    (recur (remove-and-shift-vec s))
    f))

(comment
  (part2-rasv (vec (range 1 (inc 5))))
  ;=> 2
  ;; works for the small test input

  ;; Assuming it is correct, how long will take to run?
  (time (part2-rasv (vec (range 1 (inc 1000)))))
  ;"Elapsed time: 227.860885 msecs"
  ;=> 271
  ;; Try 10 times the input
  (time (part2-rasv (vec (range 1 (inc 10000)))))
  ;"Elapsed time: 3735.233493 msecs"
  ;=> 3439
  (/ 3735.233493 227.860885)
  ;=> 16.39260504495978
  ;; Ugh, more that 10, but keep going ...
  (time (part2-rasv (vec (range 1 (inc 100000)))))
  ;"Elapsed time: 321606.14705 msecs"
  ;=> 40951
  ;; Yikes!
  (->> [227.860885 3735.233493 321606.14705]
       (partition 2 1)
       (map #(apply / (reverse %))))
  ;(16.39260504495978 86.10068089523848)
  ;; quadratic-ish? not sure but ...
  (* (/ input 100000.0) (/ 321606.14705 1000 60 60))
  ;=> 2.6929038577433566
  ;; Even if linear, best case is ~3 hours, no thanks

  #_ [])

;; A friend used a Java ArrayList for part 1 and let it do its
;; natural compaction when removing elements.
;; Maybe it is fast enough.
;; Frankly, it's feeling like it will not be but I know better than
;; to trust feelings. Only actual benchmarking will do.

(defn fill-al
  "Fill an ArrayList from a coll/seq"
  [s]
  (ArrayList. s))

(defn inc-pos
  "From p, add n in a circular list of size sz."
  [p n sz]
  (mod (+ p n) sz))

(defn remove-and-return-new-pos-al
  [^ArrayList al pos]
  (let [sz (.size al)
        rem-pos (inc-pos pos (offset sz) sz)]
    (.remove al (int rem-pos))
    (inc-pos pos (if (> rem-pos pos) 1 0) (.size al))))

(defn part2-rarnpa
  [sz]
  (let [al (fill-al (range 1 (inc sz)))]
    (loop [cp 0]
      (if (= 1 (.size al))
        (.get al 0)
        (recur (remove-and-return-new-pos-al al cp))))))

(comment
  (part2-rarnpa 5)
  ;=> 2
  ;; At least that works

  (time (part2-rarnpa 1000))
  ;"Elapsed time: 16.184753 msecs"
  ;=> 271
  ;; Whew! Same answer as before.
  ;; And much faster
  (time (part2-rarnpa 10000))
  ;Elapsed time: 103.272685 msecs"
  ;=> 3439
  (time (part2-rarnpa 100000))
  ;"Elapsed time: 609.963466 msecs"
  ;=> 40951
  (time (part2-rarnpa 1000000))
  ;"Elapsed time: 37473.35494 msecs"
  ;=> 468559
  (->> [16.184753 103.272685 609.963466 37473.35494]
       (partition 2 1)
       (map #(apply / (reverse %))))
  ;=> (6.380862593330895 5.90633879616861 61.43540888725948)
  ;; ok, better, but still ...
  (* (/ input 1000000.0) (/ 37473.35494 1000 60))
  ;=> 1.8826532329586965
  ;; best case a couple minutes but looks to be longer
  ;; let's go for it
  ;; Wait, I'm curious. Do these two solutions give the
  ;; same answers for inout I'm willing to wait for?
  (every? (fn [a] (= (part2-rasv (vec (range 1 (inc a))))
                     (part2-rarnpa a)))
          (range 5 1001))
  ;=> true
  ;; Good
  ;; Here goes
  (time (part2-rarnpa input))
  ;"Elapsed time: 428969.40844 msecs"
  ;=> 1420064
  ;; And ... that's the right answer!
  (/ 428969.40844 1000 60)
  ;=> 7.149490140666668
  ;; But 7 minutes!?

  #_ [])

;; So, I should take a win and move on. But it feels like
;; this should be faster by a lot. I mean a ~3 million
;; input size should be nothing for this laptop. Yeah,
;; feelings, I know.
;;
;; Two things keep nagging at me as I'm doing all this.
;; 1. A simple(r) linked list structure ought to help
;;    speed this up a lot.
;; 2. Feels like there might be a way to figure out the
;;    next item to delete relative to the last one deleted.
;;
;; I kept ignoring the first because I wanted to stay in
;; Clojure land. And the second I was too lazy to sit down
;; and think seriously about. More on this later.
;; So, we can stay in Clojure and build a basic linked
;; list data structure. What the heck, we already abandoned
;; immutability with the ArrayList.

(defprotocol PCell
  (n [_])
  (link [_])
  (set-link [_ cell]))

(deftype Cell [n ^:unsynchronized-mutable link]
  PCell
  (n [_] n)
  (link [_] link)
  (set-link [_this cell] (set! link cell)))

(defn ring
  "Returns a ring of numbers 1..n.
  Cell with 1 is returned."
  [n]
  (let [f (->Cell 1 nil)]
    (loop [current f i 2]
      (let [new-cell (set-link current (->Cell i nil))]
        (if (= i n)
          (do (set-link new-cell f)
              f)
          (recur new-cell (inc i)))))))

(defn move
  "move n links from cell c"
  [c n]
  (loop [on c i n]
    (if (= i 0)
      on
      (recur (link on) (dec i)))))

(defn link-past-next
  "We need to be pointing at cell before the one
  we want to remove. This does that by linking the
  the cell to the one past the next one.
  Still, return the cell we were given."
  [c]
  (let [rem (link c)]
    (set-link c (link rem))
    ;; might as well
    (set-link rem nil)
    c))

(defn step-rem-across [cell cnt]
  (let [roff (offset cnt)
        _rem (link-past-next (move cell (dec roff)))]
    (link cell)))

(defn part2-sra
  [sz]
  (let [r (ring sz)]
    (loop [cr r i sz]
      (if (= i 1)
        (n cr)
        (recur (step-rem-across cr i) (dec i))))))

(comment
  (part2-sra 5)
  ;=> 2
  ;; well, there's that, but I want more assurance
  (every? (fn [a] (= (part2-rasv (vec (range 1 (inc a))))
                     (part2-rarnpa a)
                     (part2-sra a)))
          (range 5 1001))
  ;=> true
  ;; Great.
  ;;
  ;; So, this should be faster, right?
  (time (part2-sra 1000))
  ;"Elapsed time: 5.866219 msecs"
  ;=> 271
  (time (part2-sra 10000))
  ;Elapsed time: 504.010464 msecs"
  ;=> 3439
  (time (part2-sra 100000))
  ;"Elapsed time: 49817.657233 msecs"
  ;=> 40951
  ;; Seriously? Linked list traversal is slower than
  ;; ArrayList elements shifting?
  ;; Well, ok, yeah, I guess I can see that now that I've
  ;; forced this to be in my face.
  (->> [5.866219 504.010464 49817.657233]
       (partition 2 1)
       (map #(apply / (reverse %))))
  ;=> (85.91743063121237 98.84250584329138)
  ;; Looking very quadratic. This actually makes sense
  ;; now that I think about it.
  (* (/ input 100000.0) (/ 49817.657233 1000 60))
  ;=> 25.02828305560186
  ;; 25 minutes if linear from the 100000 time
  ;; (which it isn't)
  ;; Faster that the first attempt but not great compared
  ;; to the last one. Just surprised. Once again, follow
  ;; intuitions, but don't trust them until you test it.

  #_ [])

;; Now, checking in with another friend ... he tells me he has
;; sub-second run times.
;; Excrement!
;; So there IS a way to move from node to delete to next node
;; to delete in some regular way.
;; Sure enough, after talking, I have a colloquial understanding
;; of the pattern which, now in hindsight, makes a whole lot of
;; intuitive sense. But I will still want to see the math.
;;
;; I can use most of my Clojure linked list ring solution and see if
;; it works to give the correct answer for this problem.
;; I'll explain the algorithm later.

(defn part2-link-follow
  [sz]
  (let [r (ring sz)
        roff (offset sz)
        st-cell (move r (dec roff))]
    (loop [c-cell st-cell
           stp (odd? sz)
           i sz]
      (if (= i 1)
        (n c-cell)
        (recur (if stp
                 (link (link-past-next c-cell))
                 (link-past-next c-cell))
               (not stp)
               (dec i))))))

(comment
  (part2-link-follow 5)
  ;=> 2
  ;; wipes forehead
  ;; I'm expecting speed, so let's just jump straight to it
  (time (part2-link-follow input))
  ;"Elapsed time: 296.812132 msecs"
  ;=> 1420064
  ;; Well, there it is.
  ;; It's what I always felt the complexity of this problem should be.

  #_ [])

;; The observation is that you will always delete two nodes
;; together, then skip one, then repeat. Another way to
;; think of it is you will either delete the next node or
;; the one after that depending on whether there are an even
;; or odd number of nodes. The even/odd -ness alternates as
;; you delete nodes so you will alternate between deleting the
;; node you're pointing at and skipping a node before deleting.
;;
;; Now, thing is, for me, I'd like to prove this a little more
;; formally. But first some code. If this is all true then this
;; should be true.

(comment
  (let [input 3014387
        deltas (->> (range input 0 -1)
                    (map offset)
                    (map + (range))
                    (partition 2 1)
                    (map #(apply - (reverse %))))]
    (or (= deltas (take (count deltas) (cycle [0 1])))
        (= deltas (take (count deltas) (cycle [1 0])))))
  ;=> true
  #_ [])

;; Good. This is good. But since the earlier code actually works,
;; I kind of expected this. But I've been surprised before.
;;
;; Now, I sat down to do what I should have done it the first place.
;; Aside: Why are some things easier to figure out once you know
;;        that an actual solution exists?
;;
;; For this problem, all we need to determine is the relationship
;; between A = floor(n/2) and B = floor((n-1)/2)+1.
;; If there is a regular relationship, then we can capitalize on it.
;; I'll cut to the chase. If you play with the equations you will
;; find that B = A when n is even and B = A + 1 when n is odd.
;; This means that once you establish the first node to delete,
;; and then delete it, you will delete the next node if you started
;; with an even number of nodes or will skip one and delete the
;; next node if you started with an odd number of nodes. And since
;; deleting changes either odd to even or even to odd, you will
;; begin alternating either, skip and delete then delete again,
;; or, delete and then skip and delete, as you go around the ring.
;;
;; This is exactly what is embodied in the code for part2-link-follow.

;; I needed to work this out formally to satisfy my understanding
;; of the problem. Like Christian Wolff says, "I need to finish."
;; Finally, I feel like the horse can't be killed any more.

;; I want to thank @drbobbeaty and @tessellator for their
;; camaraderie, enablement, and indulgence as I work through
;; problems and for not discouraging me when I get carried
;; away by the deep minutia of programming.