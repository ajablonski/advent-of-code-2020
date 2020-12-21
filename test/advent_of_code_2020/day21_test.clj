(ns advent-of-code-2020.day21-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day21 :refer :all]
            [advent-of-code-2020.utils :refer [with-minimal-output]]))

(deftest parse-food-test
  (testing "Should parse food into ingredients and allergents"
    (is (= {:ingredients #{"mxmxvkd" "kfcds" "sqjhc" "nhms"}
            :allergens   #{"dairy" "fish"}}
           (parse-food "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)")))))


(deftest get-possible-allergens-test
  (testing "Should get allergens from multiple foods"
    (is (= #{"dairy" "fish" "soy"}
           (get-all-allergens
             (list
               {:ingredients #{"mxmxvkd" "kfcds" "sqjhc" "nhms"}
                :allergens   #{"dairy" "fish"}}
               {:ingredients #{"adsfpo" "dosi" "eiu"}
                :allergens   #{"soy" "fish"}}
               ))
           ))))

(deftest get-possible-culprits-for-allergen-test
  (testing "Should get all of the possible sources of an allergen"
    (is (= #{"mxmxvkd" "sqjhc"}
           (get-possible-culprits-for-allergen
             "fish"
             (list
               {:ingredients #{"sqjhc" "mxmxvkd" "sbzzf"}
                :allergens   #{"fish"}}
               {:ingredients #{"mxmxvkd" "kfcds" "sqjhc" "nhms"}
                :allergens   #{"dairy" "fish"}}))))))

(deftest get-all-ingredients-test
  (testing "Should return all ingredients"
    (is (= #{"sqjhc" "mxmxvkd" "sbzzf" "nhms"}
           (get-all-ingredients
             (list
               {:ingredients #{"sqjhc" "mxmxvkd" "sbzzf"}
                :allergens   #{"fish"}}
               {:ingredients #{"mxmxvkd" "sbzzf" "sqjhc" "nhms"}
                :allergens   #{"dairy" "fish"}}))))))

(deftest get-allergen-potential-ingredients-test
  (testing "Should return all ingredients that may contain an allergen"
    (is (= #{"mxmxvkd" "sqjhc" "fvjkl"}
           (get-allergen-potential-ingredients
             (list
               {:ingredients #{"mxmxvkd" "kfcds" "sqjhc" "nhms"}
                :allergens   #{"dairy" "fish"}}
               {:ingredients #{"trh" "fvjkl" "sbzzf" "mxmxvkd"}
                :allergens   #{"dairy"}}
               {:ingredients #{"sqjhc" "fvjkl"}
                :allergens   #{"soy"}}
               {:ingredients #{"sqjhc" "mxmxvkd" "sbzzf"}
                :allergens   #{"fish"}}))))))

(deftest get-allergen-free-ingredients-test
  (testing "Should return all ingredients that cannot contain an allergen"
    (is (= #{"kfcds" "nhms" "sbzzf" "trh"}
           (get-allergen-free-ingredients
             (list
               {:ingredients #{"mxmxvkd" "kfcds" "sqjhc" "nhms"}
                :allergens   #{"dairy" "fish"}}
               {:ingredients #{"trh" "fvjkl" "sbzzf" "mxmxvkd"}
                :allergens   #{"dairy"}}
               {:ingredients #{"sqjhc" "fvjkl"}
                :allergens   #{"soy"}}
               {:ingredients #{"sqjhc" "mxmxvkd" "sbzzf"}
                :allergens   #{"fish"}}))))))

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "5\n"
           (with-out-str (main-1))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "mxmxvkd,sqjhc,fvjkl\n"
           (with-out-str (with-minimal-output (main-2)))))))
