(ns nanc-in-a-can.taller-abierto.graph.dijkstra
  "By kototamo@gmail.com, taken from https://snipplr.com/view/22183/dijkstras-algorithm-in-clojure/")

(declare dijkstra build-path add-rdist update-rdists take-minnode)

(defn shortest-path
  ([net root nodedst children distance]
   " return [path dist]"
   " net is the graph "
   " root the source node "
   " nodedst the destination "
   " children a function returning the children for a node "
   " distance a function returning the distance between two nodes "
   (let [preds (dijkstra net root nodedst children distance)
         path (build-path preds root nodedst)]
     (if (nil? path)
       nil
       [path (second (preds nodedst))])))

  ([net root nodedst children]
   (shortest-path net root nodedst children (constantly 1))))

(defn- dijkstra [net root nodedst children distance]
  (loop [rdists (sorted-map 0 {root root})
         minnode root
         preds {root [root 0]}
         dist 0]
    (if (empty? rdists)
      preds
      (let [[nminnode ndist nrdists npreds] (take-minnode rdists preds)
            [nnrdists nnpreds] (update-rdists nrdists
                                              npreds
                                              net
                                              nminnode
                                              ndist
                                              children distance)]
        (recur nnrdists nminnode nnpreds ndist)))))


(defn- build-path [preds root nodedst]
  "reverse walk on preds to reconstruct the shortest path"
  (loop [[pred dist] (preds nodedst) path (list nodedst)]
    (if (nil? pred)
      nil
      (if (= pred root)
        (cons root path)
        (recur (preds pred) (cons pred path))))))

(defn- add-rdist
  ([rdists node pred dist]
   "add a known rdist (rdist = distance to the root)"
   (if-let [nodes (rdists dist)]
     (assoc rdists dist (assoc nodes node pred))
     (assoc rdists dist {node pred})))

  ([rdists node pred dist prevdist]
   (let [nrdists (add-rdist rdists node pred dist)
         minnodes (rdists prevdist)
         nminnodes (dissoc minnodes node)]
     (if (empty? nminnodes)
       (dissoc nrdists prevdist)
       (assoc nrdists prevdist nminnodes)))))

(defn- update-rdists [rdists preds net node dist children distance]
  "return [rdists preds] updated"
  (reduce (fn [acc x]
            (let [curdist (+ dist (distance net node x))
                  prevdist (second (preds x))
                  nrdists (first acc)
                  npreds (second acc)]
              (if (nil? prevdist)
                [(add-rdist nrdists x node curdist) (assoc npreds x [node curdist])]
                (if (< curdist prevdist)
                  [(add-rdist nrdists x node curdist prevdist)
                   (assoc npreds x [node curdist])]
                  [nrdists npreds]))))
          [rdists preds]
          (children net node)))

(defn- take-minnode [rdists preds]
  "return a vector [minnode dist rdists preds]"
  (let [ [dist minnodes] (first rdists)
        [minnode pred] (first minnodes)
        others (rest minnodes)]
    [minnode
     dist
     (if (empty? others)
       (dissoc rdists dist)
       (assoc rdists dist others))
     (assoc preds minnode [pred dist]) ]))


(comment

  ;;
  ;; Example (based on the french wikipedia)
  ;; http://fr.wikipedia.org/wiki/Algorithme_de_Dijkstra
  ;;

  (def net {:A {:B 85, :C 217, :E 173},
            :B {:F 80},
            :C {:G 186 :H 103},
            :D {},
            :E {:J 502},
            :F {:I 250}
            :G {},
            :H {:D 183 :J 167}
            :I {:J 84},
            :J {}
            })


  (defn children [net node]
    (keys (net node)))

  (defn distance [net nodesrc nodedst]
    ((net nodesrc) nodedst))

  (shortest-path net :A :J children distance)


  (shortest-path (add-weights g2) #'red :blue children))


(defn add-weights
  [unweighted-graph]
  (->> unweighted-graph
       (map (fn [[n vs]]
              [n (apply merge (map #(hash-map % 1) vs))]))
       (into {})))


(comment
  (declare red)

  (def demo-graph {#'red    {:green 10, :blue   5, :orange 8},
                   :green  {#'red 10,   :blue   3},
                   :blue   {:green 3,  #'red    5, :purple 7},
                   :purple {:blue 7,   :orange 2},
                   :orange {:purple 2, #'red    2}})


  (def g2 {#'red #{#'red :blue}
           :blue #{:green :orange}
           :green #{#'red}
           :orange #{#'red}})


  (dijkstra demo-graph #'red)
  (dijkstra demo-graph #'red))
