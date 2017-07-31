(ns dd-solver.core
  (:require [ysera.test :refer [is= is is-not]]
            [ysera.collections :refer [seq-contains? remove-one]]
            [clojure.string :refer [split]]))

(defn create-state []
  {:remaining-hands nil
   :tricks-ns       0
   :tricks-ew       0
   :played-cards    []
   :player-in-turn  nil
   :starting-hands  nil})

(defn add-hands
  {:test (fn []
           (is= (-> (create-state)
                    (add-hands {:north "AKQJ AKQJ AKQJ A"
                                :east  "xxx xxx xxx xxxx"
                                :south "123 456 789 9"
                                :west  "AKJ sdj sdjkhsk ds"}))
                {:remaining-hands {:east  ["Sx" "Sx" "Sx" "Hx" "Hx" "Hx" "Dx" "Dx" "Dx" "Cx" "Cx" "Cx" "Cx"]
                                   :north ["SA" "SK" "SQ" "SJ" "HA" "HK" "HQ" "HJ" "DA" "DK" "DQ" "DJ" "CA"]
                                   :south ["S1" "S2" "S3" "H4" "H5" "H6" "D7" "D8" "D9" "C9"]
                                   :west  ["SA" "SK" "SJ" "Hs" "Hd" "Hj" "Ds" "Dd" "Dj" "Dk" "Dh" "Ds" "Dk" "Cd" "Cs"]}
                 :tricks-ns       0
                 :tricks-ew       0
                 :played-cards    []
                 :player-in-turn  nil
                 :starting-hands  {:east  ["Sx" "Sx" "Sx" "Hx" "Hx" "Hx" "Dx" "Dx" "Dx" "Cx" "Cx" "Cx" "Cx"]
                                   :north ["SA" "SK" "SQ" "SJ" "HA" "HK" "HQ" "HJ" "DA" "DK" "DQ" "DJ" "CA"]
                                   :south ["S1" "S2" "S3" "H4" "H5" "H6" "D7" "D8" "D9" "C9"]
                                   :west  ["SA" "SK" "SJ" "Hs" "Hd" "Hj" "Ds" "Dd" "Dj" "Dk" "Dh" "Ds" "Dk" "Cd" "Cs"]}}))}
  [state {north :north
          east  :east
          south :south
          west  :west}]
  (let [north-suits (split north #" ")
        east-suits (split east #" ")
        south-suits (split south #" ")
        west-suits (split west #" ")
        suit-symbol-list ["S" "H" "D" "C"]
        get-card-list (fn [suits]
                        (-> (map-indexed (fn [i suit]
                                           (->> (split suit #"")
                                                (map (fn [card-symbol]
                                                       (str (nth suit-symbol-list i) card-symbol)))))
                                         suits)
                            (flatten)
                            (into [])))
        hands-map {:north (get-card-list north-suits)
                   :east  (get-card-list east-suits)
                   :south (get-card-list south-suits)
                   :west  (get-card-list west-suits)}]
    (-> state
        (assoc :starting-hands hands-map)
        (assoc :remaining-hands hands-map))))

(defn set-player-in-turn [state player-in-turn]
  (assoc state :player-in-turn player-in-turn))

(defn get-next-player
  {:test (fn []
           (is= (get-next-player :east)
                :south)
           (is= (get-next-player :north)
                :east))}
  [player]
  (let [next-player-map {:north :east
                         :east  :south
                         :south :west
                         :west  :north}]
    (player next-player-map)))

(defn get-card-value
  {:test (fn []
           (is= (get-card-value "A")
                14)
           (is= (get-card-value "7")
                7))}
  [card]
  (condp = card
    "A" 14
    "K" 13
    "Q" 12
    "J" 11
    "T" 10
    "9" 9
    "8" 8
    "7" 7
    "6" 6
    "5" 5
    "4" 4
    "3" 3
    "2" 2
    :default nil))

(defn determine-trick-winner
  {:test (fn []
           (is= (determine-trick-winner {:north           "S7"
                                         :east            "S6"
                                         :south           "S8"
                                         :west            "HA"
                                         :starting-player :north})
                :south)
           (is= (determine-trick-winner {:north           "SJ"
                                         :east            "SQ"
                                         :south           "S8"
                                         :west            "HA"
                                         :starting-player :north})
                :east))}
  [{starting-player :starting-player
    :as             args}]
  (loop [current-leader starting-player
         next-player (get-next-player starting-player)]
    (if (= next-player starting-player)
      current-leader
      (recur (if (or (not= (subs (current-leader args) 0 1)
                           (subs (next-player args) 0 1))
                     (> (get-card-value (subs (current-leader args) 1 2))
                        (get-card-value (subs (next-player args) 1 2))))
               current-leader
               next-player)
             (get-next-player next-player)))))

(defn visualise-deal [{north :north
                       east  :east
                       south :south
                       west  :west}]
  (let [north-suits (split north #" ")
        east-suits (split east #" ")
        south-suits (split south #" ")
        west-suits (split west #" ")]
    (dotimes [i 4]
      (println "        " (nth north-suits i)))
    (dotimes [i 4]
      (println (nth west-suits i) (apply str (repeat (- 14 (count (nth west-suits i))) \space)) (nth east-suits i)))
    (dotimes [i 4]
      (println "        " (nth south-suits i)))))

(defn get-remaining-cards-of-player
  {:test (fn []
           (is= (-> (create-state)
                    (add-hands {:north "AKQJ AKQJ AKQJ A"
                                :east  "T987 T987 T987 K"
                                :south "6543 6543 6543 Q"
                                :west  "2 2 2 JT98765432"})
                    (get-remaining-cards-of-player :north))
                ["SA" "SK" "SQ" "SJ" "HA" "HK" "HQ" "HJ" "DA" "DK" "DQ" "DJ" "CA"]))}
  [state player]
  (get-in state [:remaining-hands player]))

(defn add-empty-trick
  {:test (fn []
           (is= (-> (create-state)
                    (add-empty-trick :north)
                    (add-empty-trick :south)
                    (:played-cards))
                [{:north           nil
                  :east            nil
                  :south           nil
                  :west            nil
                  :starting-player :north}
                 {:north           nil
                  :east            nil
                  :south           nil
                  :west            nil
                  :starting-player :south}]))}
  [state starting-player]
  (update state :played-cards conj {:north           nil
                                    :east            nil
                                    :south           nil
                                    :west            nil
                                    :starting-player starting-player}))

(defn get-last-trick
  {:test (fn []
           (is= (-> (create-state)
                    (add-empty-trick :south)
                    (get-last-trick))
                {:north           nil
                 :east            nil
                 :south           nil
                 :west            nil
                 :starting-player :south}))}
  [state]
  (-> state
      (:played-cards)
      (last)))

(defn ongoing-trick?
  {:test (fn []
           (is-not (-> (create-state)
                       (ongoing-trick?)))
           (is (-> (create-state)
                   (add-empty-trick :north)
                   (ongoing-trick?))))}
  [state]
  (-> state
      (get-last-trick)
      (vals)
      (seq-contains? nil)))

(defn play-card
  {:test (fn []
           (let [state (-> (create-state)
                           (set-player-in-turn :south)
                           (add-empty-trick :south)
                           (add-hands {:north "A A A A"
                                       :east  "T T T K"
                                       :south "65 6 6 Q"
                                       :west  "2 2 2 2"})
                           (play-card "S6"))]
             (is= (get-last-trick state)
                  {:north           nil
                   :east            nil
                   :south           "S6"
                   :west            nil
                   :starting-player :south})
             (is= (:player-in-turn state)
                  :west)
             (is= (:remaining-hands state)
                  {:north ["SA" "HA" "DA" "CA"]
                   :east  ["ST" "HT" "DT" "CK"]
                   :south ["S5" "H6" "D6" "CQ"]
                   :west  ["S2" "H2" "D2" "C2"]})))}
  [state card]
  (-> state
      (assoc-in [:played-cards (- (count (:played-cards state)) 1) (:player-in-turn state)] card)
      (update-in [:remaining-hands (:player-in-turn state)] remove-one card)
      (update :player-in-turn get-next-player)))

(defn unfinished-trick?
  {:test (fn []
           (is-not (-> (create-state)
                       (add-empty-trick :north)
                       (unfinished-trick?)))
           (is (-> (create-state)
                   (set-player-in-turn :south)
                   (add-empty-trick :south)
                   (add-hands {:north "A A A A"
                               :east  "T T T K"
                               :south "65 6 6 Q"
                               :west  "2 2 2 2"})
                   (play-card "S6")
                   (unfinished-trick?))))}
  [state]
  ;;TODO Vad är ett unfinished trick? Måste ett kort vara spelat eller inte?
  ;;TODO Räcker det med att sticket är skapat? Därför testet fallerar
  ;;TODO Vad är skillnaden mellan denna och ongoing-trick?
  ;;TODO Måste se över arkitekturen och hur programmet ska vara strukturerat...
  ;;TODO Testa att krava genom att bestämma funktioner från början och skriva testen först!
  (as-> state $
        (get-last-trick $)
        (dissoc $ :starting-player)
        (vals $)
        (reduce (fn [a card]
                  (println card)
                  (or (nil? card)
                      a))
                false
                $)))

(defn get-trick-suit-symbol
  {:test (fn []
           (is= (-> (create-state)
                    (set-player-in-turn :south)
                    (add-empty-trick :south)
                    (add-hands {:north "A A A A"
                                :east  "T T T K"
                                :south "65 6 6 Q"
                                :west  "2 2 2 2"})
                    (play-card "S6")
                    (get-trick-suit-symbol))
                "S")
           (is= (-> (create-state)
                    (add-empty-trick :south)
                    (get-trick-suit-symbol))
                nil))}
  [state]
  (when (not (ongoing-trick? state))
    state))

(defn get-playable-cards-for-player-in-turn
  {:test (fn []
           (is= (-> (create-state)
                    (add-hands {:north "AKQJ AKQJ AKQJ A"
                                :east  "T987 T987 T987 K"
                                :south "6543 6543 6543 Q"
                                :west  "2 2 2 JT98765432"})
                    (set-player-in-turn :north)
                    (get-playable-cards-for-player-in-turn))
                ["SA" "SK" "SQ" "SJ" "HA" "HK" "HQ" "HJ" "DA" "DK" "DQ" "DJ" "CA"]))}
  [state]
  nil)

;; TODO
;; Kolla vilka kort en spelare kan spela
;; Tolka input av en giv och kolla vem som vinner stick om korten spelas på ett visst sätt
;; Få till min/max för att avgöra bästa spel
;; Lägg till trumf
;; Optimera? Eventuellt lagra resultat så att det inte måste räknas om hela tiden. Kanske behövs för att det ens ska gå att köra
;; Skapa vy där man kan klicka och spela en giv

(comment
  ;; Useful commands
  (visualise-deal {:north "AKQJ AKQJ AKQJ A"
                   :east  "xxx xxx xxx xxxx"
                   :south "123 456 789 9"
                   :west  "AKJ sdj sdjkhsk ds"})
  ;; .
  )
