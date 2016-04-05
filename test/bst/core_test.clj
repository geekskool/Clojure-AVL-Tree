(ns bst.core-test
  (:require [clojure.test :refer :all]
            [bst.core :refer :all]))

(def res (to-tree '(1 2 3 4 5 6 7 8 9 10)))

(deftest insert-test
  (testing "insert failed"
    (is (= (type (insert res 10)) bst.core.Node))))

(deftest remove-test
  (testing "remove failed"
    (is (= (-remove nil 10) nil))))

(deftest insert-balance-test
  (testing "insert-balance Failed"
    (is (= (preorder-traversal res) '(5 4 3 2 1 6 7 8 9 10)))))

(deftest remove-balance-test
  (testing "Remove balance Failed"
    (is (= (preorder-traversal (remove-balance res 10)) '(4 3 2 1 5 6 7 8 9)) )))

(deftest min?-test
  (testing "Min? failed"
    (is (= (min? res) 1))))

(deftest max?-test
  (testing "Max? failed"
    (is (= (max? res) 10))))

(deftest height?-test
  (testing "Height? failed"
    (is (= (height? res) 5))))

(deftest height-factor-test
  (testing "height-factor failed"
    (is (= (height-factor res) -1))))

(deftest inorder-traversal-test
  (testing "inorder-traversal failed"
    (is (= (inorder-traversal res) '(1 2 3 4 5 6 7 8 9 10)))))

(deftest preorder-traversal-test
  (testing "preorder-traversal failed"
    (is (= (preorder-traversal res) '(5 4 3 2 1 6 7 8 9 10)))))
