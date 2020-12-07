(ns advent-of-code-2020.day7-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day7 :refer :all]))

(deftest parse-rule-test
  (testing "Parse bag containing one other bag type"
    (is (= (parse-rule "bright white bags contain 1 shiny gold bag.")
           {"bright white"
            [{:color "shiny gold" :quantity 1}]})))
  (testing "Parse bag containing multiple other bag type"
    (is (= (parse-rule "shiny gold bags contain 1 dark olive bag, 1 vibrant plum bag.")
           {"shiny gold"
            [{:color "dark olive" :quantity 1}
             {:color "vibrant plum" :quantity 1}]})))
  (testing "Parse bag containing multiple other bag type with larger quantities"
    (is (= (parse-rule "shiny gold bags contain 20 dark olive bags, 10 vibrant plum bags.")
           {"shiny gold"
            [{:color "dark olive" :quantity 20}
             {:color "vibrant plum" :quantity 10}]})))
  (testing "Parse bag containing no other bags"
    (is (= (parse-rule "faded blue bags contain no other bags.")
           {"faded blue" []}))))

(let [test-rules (parse-rules "light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags.\n")]
  (deftest get-can-contain-test
    (testing "Shows colors that can contain this color at 1 level"
      (is (= (get-can-contain
               test-rules
               "shiny gold")
             #{"bright white" "muted yellow" "dark orange" "light red"}))))
  (deftest get-bags-contained-test
    (testing "Shows empty for bottom level bag"
      (is (= (get-bags-contained test-rules "dotted black")
             {})))
    (testing "Shows first level pass for bag with level-1 rule"
      (is (= (get-bags-contained test-rules "dark olive")
             {"faded blue"   3
              "dotted black" 4})))
    (testing "Shows bags that need to be nested"
      (is (= (get-bags-contained test-rules "shiny gold")
             {"dark olive"   1
              "vibrant plum" 2
              "faded blue"   13
              "dotted black" 16})))
    (testing "Shows combined multipliers"
      (is (= (get-bags-contained test-rules "muted yellow")
             {"shiny gold"   2,
              "dark olive"   2,
              "faded blue"   35,
              "dotted black" 32,
              "vibrant plum" 4})))))

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "4\n"
           (with-out-str (main-1))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "32\n"
           (with-out-str (main-2))))))
