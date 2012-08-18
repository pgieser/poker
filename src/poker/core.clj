(ns poker.core)

;; initialize
(def ranks [:ace :two :three :four :five :six :seven :eight :nine :ten :jack :queen :king ])
(def ranks-display {:ace "A" :two "2" :three "3" :four "4" :five "5" :six "6" :seven "7" :eight "8" :nine "9" :ten "T" :jack "J" :queen "Q" :king "K"})
(def reverse-ranks-display (clojure.contrib.datalog.util/reverse-map ranks-display))
(def rank-order (zipmap ranks (iterate inc 1)))

(def suits [:club :diamond :heart :spade ])
(def suits-display {:club "C" :diamond "D" :heart "H" :spade "S"})
(def reverse-suits-display (clojure.contrib.datalog.util/reverse-map suits-display))

; verify rank/suit?
(defrecord Card [rank suit])

(defn is-card? [X]
  (= Card (type X)))

(defn make-card [[R S]]
  (Card. (get reverse-ranks-display (str R)) (get reverse-suits-display (str S))))

(defn to-cards [X]
  (map make-card (clojure.contrib.string/split #"-" X)))

(defn make-suit [suit]
  (vec (map #(Card. % suit) ranks)))

(def clubs (make-suit :club ))
(def diamonds (make-suit :diamond ))
(def hearts (make-suit :heart ))
(def spades (make-suit :spade ))

(def deck (flatten [clubs diamonds hearts spades]))

(def hand-ranks [:straight-flush :four-of-a-kind :full-house :flush :straight :three-of-a-kind :two-pair :pair :high-card ])
(def hand-order (zipmap hand-ranks (iterate inc 1)))
(def hand-rank-tests [straight-flush? four-of-a-kind? full-house? flush? straight? three-of-a-kind? two-pair? pair?])

(defn valid-hand? [x]
  (and (= 5 (count x))
    (every? is-card? x)))

;; print
(defn card-display [card]
  {:pre [(is-card? card)]}
  (str (get ranks-display (:rank card)) (get suits-display (:suit card))))

(defn print-cards [cards]
  {:pre [(every? is-card? cards)]}
  (str (apply str (interpose "-" (map #(card-display %) cards)))))

;(def shuffled-deck (shuffle deck))
;(def hand (take 5 shuffled-deck))
;(def remaining-deck (take-last 47 shuffled-deck))


;; analyze
(defn card-rank-order [card]
  {:pre [(is-card? card)]}
  (get rank-order (:rank card)))

(defn sort-cards [cards]
  {:pre [(every? is-card? cards)]}
  (sort-by card-rank-order cards))

(defn distinct-suits [cards]
  {:pre [(every? is-card? cards)]}
  (distinct (map #(:suit %) cards)))

(defn rank-counts [cards]
  {:pre [(every? is-card? cards)]}
  (map #(:rank %) cards))

(defn card-rank-multiples [cards]
  {:pre [(every? is-card? cards)]}
  (sort (map #(count (val %)) (group-by card-rank-order cards))))


;; evaluate
; verify hand is 5 cards
(defn four-of-a-kind? [hand]
  {:pre [(valid-hand? hand)]}
  (= '(1 4) (card-rank-multiples hand)))

(defn full-house? [hand]
  {:pre [(valid-hand? hand)]}
  (= '(2 3) (card-rank-multiples hand)))

(defn three-of-a-kind? [hand]
  {:pre [(valid-hand? hand)]}
  (= '(1 1 3) (card-rank-multiples hand)))

(defn two-pair? [hand]
  {:pre [(valid-hand? hand)]}
  (= '(1 2 2) (card-rank-multiples hand)))

(defn pair? [hand]
  {:pre [(valid-hand? hand)]}
  (= '(1 1 1 2) (card-rank-multiples hand)))

(defn flush? [hand]
  {:pre [(valid-hand? hand)]}
  (= 1 (count (distinct-suits hand))))

(defn straight? [hand]
  {:pre [(valid-hand? hand)]}
  (def sorted-ranks (map card-rank-order (sort-cards hand)))
  (= '(0 1 2 3 4) (map #(- % (first sorted-ranks)) sorted-ranks)))

(defn straight-flush? [hand]
  {:pre [(valid-hand? hand)]}
  (and (straight? hand) (flush? hand)))

(defn high-hand-eval [hand]
  (map #(% hand) hand-rank-tests))

(defn high-hand [hand]
  {:pre [(valid-hand? hand)]}
  (if (straight-flush? hand)
    :straight-flush (if (four-of-a-kind? hand)
                      :four-of-a-kind (if (full-house? hand)
                                        :full-house (if (flush? hand)
                                                      :flush (if (straight? hand)
                                                               :straight (if (three-of-a-kind? hand)
                                                                           :three-of-a-kind (if (two-pair? hand)
                                                                                              :two-pair (if (pair? hand)
                                                                                                          :pair :high-card )))))))))

;; generate
(defn random-hand []
  (take 5 (shuffle deck)))

(defn sample-hands [n]
  (loop [cnt n high-hands ()]
    (if (zero? cnt) high-hands
      (recur (dec cnt) (conj high-hands (high-hand (random-hand)))))))

(defn summarize [high-hands]
  (map (fn [[key val]] [key (count val)]) (group-by identity high-hands)))

(defn sort-by-freq [summary]
  (sort-by (fn [[hand freq]] freq) summary))

(defn sort-by-hand [summary]
  (sort-by (fn [[hand freq]] (get hand-order hand)) summary))
