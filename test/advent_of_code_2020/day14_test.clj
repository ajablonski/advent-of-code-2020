(ns advent-of-code-2020.day14-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day14 :refer :all]))

(deftest parse-instruction-test
  (testing "Parse mask instruction"
    (let [initial-state {:mask   "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                         :memory {}}]
      (is (= {:mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" :memory {}}
             ((parse-instruction "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" set-memory)
              initial-state)))))
  (testing "Parse mem instruction"
    (let [initial-state {:mask   "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                         :memory {}}]
      (is (= {:mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" :memory {0 2}}
             ((parse-instruction "mem[0] = 2" set-memory)
              initial-state))))))


(deftest apply-mask-test
  (testing "Xs should result in no change"
    (is (= 2r010000000000000000000000000000000000
           (apply-mask 2r010000000000000000000000000000000000
                       (apply str (repeat 36 "X"))))))
  (testing "1s should result in change to bits"
    (is (= 2r111111111111111111111111111111111111
           (apply-mask 2r010000000000000000000000000000000000
                       (apply str (repeat 36 "1"))))))
  (testing "0s should result in change to bits"
    (is (= 2r000000000000000000000000000000000000
           (apply-mask 2r010000000000000000000000000000000000
                       (apply str (repeat 36 "0"))))))
  (testing "Mask should combine all 3"
    (is (= 2r000000000000000000000000000001001001
           (apply-mask 2r000000000000000000000000000000001011
                       "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"))))
  )

(deftest resolve-floating-masks-test
  (testing "Should return values with all 0s and all 1s as list"
    (is (= (list 2r000000000000000000000000000001001001)
           (resolve-float-bits "000000000000000000000000000001001001"))))
  (testing "Should return both values when an X is seen with all 0s and all 1s as list"
    (is (= (list
             2r000000000000000000000000000001001001
             2r100000000000000000000000000001001001)
           (resolve-float-bits "X00000000000000000000000000001001001"))))
  (testing "Should iterate through all possibilities of Xs"
    (is (= (list
             2r000000000000000000000000000001001000
             2r000000000000000000000000000001001001
             2r100000000000000000000000000001001000
             2r100000000000000000000000000001001001)
           (resolve-float-bits "X0000000000000000000000000000100100X"))))
  )

(deftest apply-mask-to-mem-test
  (testing "Should preserve memory address values when mask has 0s and fill space"
    (is (= "000000000000000000000000000000001011"
           (apply-mask-to-mem 2r1011 (apply str (repeat 36 "0"))))))
  (testing "Should overwrite memory address with X when X provided"
    (is (= "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
           (apply-mask-to-mem 2r1011 (apply str (repeat 36 "X"))))))
  (testing "Should overwrite memory address with X when X provided"
    (is (= "111111111111111111111111111111111111"
           (apply-mask-to-mem 2r1011 (apply str (repeat 36 "1"))))))
  )

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "51\n"
           (with-out-str (main-1))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "208\n"
           (with-out-str (main-2))))))
