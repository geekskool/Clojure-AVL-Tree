(ns bst.core
  (:gen-class))

(defrecord Node [key left right])

(defn right-rotate [{:keys [key left right] :as tree}]
  (Node. (:key left) (:left left) (Node. key nil right)))

(defn left-rotate [{:keys [key left right] :as tree}]
  (Node. (:key right) (Node. key left nil) (:right right)))

;;Inserting into tree
(defn insert [{:keys [key left right] :as tree} value]
  (cond
   (nil? tree) (Node. value nil nil)
   (< value key) (Node. key (insert left value) right)
   (> value key) (Node. key left (insert right value))
   :else tree))

(declare height-factor)

(defn insert-balance [{:keys [key left right] :as tree} value]
  (cond
     (and (>= (height-factor tree) -1) (<= (height-factor tree) 1)) tree
     (and (> (height-factor tree) 1) (< value (:key left))) (right-rotate tree)
     (and (< (height-factor tree) -1) (> value (:key right))) (left-rotate tree)
     (and (> (height-factor tree) 1) (> value (:key left))) (right-rotate (Node. key (left-rotate left) right))
     (and (< (height-factor tree) -1) (< value (:key right))) (left-rotate (Node. key left (right-rotate right)))))

(defn insert-into-tree [{:keys [key left right] :as tree} value]
  (insert-balance (insert tree value) value))

(def to-tree #(reduce insert-into-tree nil %))
;;Inserting ends

;;Find an element
(defn find? [{:keys [key left right] :as tree} elem]
  (cond
   (nil? tree) false
   (> elem key) (recur right elem)
   (< elem key) (recur left elem)
   :else true))

(defn min? [{:keys [key left]}]
  (if (nil? left) 
    key
    (recur left)))

(defn max? [{:keys [key right]}]
  (if (nil? right) 
    key
    (recur right)))

(defn -remove [{:keys [key left right] :as tree} value]
  (cond
   (nil? tree) nil
   (< value key) (Node. key (-remove left value) right)
   (> value key) (Node. key left (-remove right value))
   (nil? left) right
   (nil? right) left
   :else (let [min-value (min? right)]
           (Node. min-value left (-remove right min-value)))))
(declare height?)
(defn remove-balance [{:keys [key left right] :as tree} value]
  (let [new-tree (-remove tree value)]
    (cond 
     (and (>= (height? new-tree) -1) (<= (height? new-tree) 1)) new-tree
     (and (> (height? new-tree) 1) (>= (height? (:left new-tree)) 0)) (right-rotate new-tree)
     (and (> (height? new-tree) 1) (< (height? (:left new-tree)) 0)) (right-rotate (Node. (:key new-tree) (left-rotate (:left new-tree)) (:right new-tree)))
     (and (< (height? new-tree) -1) (<= (height? (:right new-tree)) 0)) (left-rotate new-tree)
     (and (< (height? new-tree) -1) (> (height? (:right new-tree)) 0)) (left-rotate (Node. (:key new-tree) (:left new-tree) (right-rotate (:right new-tree)))))))

(declare height?)

(defn height-factor [{:keys [key left right] :as tree}]
  (- (if (nil? left) 0 (inc (height? left))) (if (nil? right) 0 (inc (height? right)))))

(defn height? [{:keys [key left right] :as tree}]
  (if (and (nil? left) (nil? right))
    0
    (let [lDepth (height? left) rDepth (height? right)]
      (if (> lDepth rDepth)
        (inc lDepth)
        (inc rDepth)))))

(defn inorder-traversal [{:keys [key left right] :as tree}]
  (when tree
    (concat (inorder-traversal left) [key] (inorder-traversal right))))

(defn preorder-traversal [{:keys [key left right] :as tree}]
  (when tree
    (concat [key] (preorder-traversal left) (preorder-traversal right))))

(defn adding []
  (def tree (to-tree '(1 2 3 4 5 6 7 8)))
  ;; (println (java.util.Objects/hashCode tree))
  ;(def tree2 (insert-into-tree tree 100))
  (def tree2 (remove-balance tree 10))
  (println (inorder-traversal tree))
  (println "Preorder " (preorder-traversal tree))
  (println (height-factor tree)))

