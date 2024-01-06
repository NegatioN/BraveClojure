(ns clojure-noob.core
  (:gen-class))

(def asym-hobbit-body-parts [{:name "head" :size 3}
                             {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "left-shoulder" :size 3}
                             {:name "left-upper-arm" :size 3}
                             {:name "chest" :size 10}
                             {:name "back" :size 10}
                             {:name "left-forearm" :size 3}
                             {:name "abdomen" :size 6}
                             {:name "left-kidney" :size 1}
                             {:name "left-hand" :size 2}
                             {:name "left-knee" :size 2}
                             {:name "left-thigh" :size 4}
                             {:name "left-lower-leg" :size 3}
                             {:name "left-achilles" :size 1}
                             {:name "left-foot" :size 2}])

(defn matching-part
  [{:keys [name size]}]
  {:name (clojure.string/replace name #"^left-" "right-")
   :size size})

(defn symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (loop [remaining-asym-parts asym-body-parts
         final-body-parts []]
    (if (empty? remaining-asym-parts)
      final-body-parts
      (let [[part & remaining] remaining-asym-parts]
        (recur remaining
               (into final-body-parts
                     (set [part (matching-part part)])))))))
(symmetrize-body-parts asym-hobbit-body-parts)

(defn better-symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (reduce (fn [final-body-parts part]
            (into final-body-parts (set [part (matching-part part)])))
          []
          asym-body-parts))

(better-symmetrize-body-parts asym-hobbit-body-parts)

(defn hit
  [asym-body-parts]
  (let [sym-parts (better-symmetrize-body-parts asym-body-parts)
        body-part-size-sum (reduce + (map :size sym-parts))
        target (rand body-part-size-sum)]
    (loop [[part & remaining] sym-parts
             accumulated-size (:size part)]
        (if (> accumulated-size target)
          part
          (recur remaining (+ accumulated-size (:size (first remaining))))))))

(hit asym-hobbit-body-parts)


(comment "From here, do some exercises"
         "Use the str, vector, list, hash-map, and hash-set functions."
         "Write a function that takes a number and adds 100 to it."
         "Write a function, dec-maker, that works exactly like the function inc-maker except with subtraction:"
         "Write a function, mapset, that works like map except the return value is a set: ")
(str (list 1 2 3))
(set (list 1 2 3))
(hash-map :a 1 :b 2 :c 3)

(defn add100 [a] (+ a 100))

(add100 5)

(defn dec-maker 
  "Create a custom decrementer function"
  [dec-by]
  #(- % dec-by))

(def dec7 (dec-maker 7))

(dec7 100)

(defn mapset
  "like map, but returns a set."
  [f collection]
  (set (map f collection)))


(defn mapset2
  "This ended up being pointless, but I wanted to try using reduce on a set structure"
  [f collection]
  (set (reduce (fn [c elem] (into c [(f elem)])) 
          [] 
          collection)))

(mapset2 inc (list 1 2 2 3))

(into [] (set [1]))

