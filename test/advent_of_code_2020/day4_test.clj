(ns advent-of-code-2020.day4_test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day4 :refer :all]))

(deftest to-document-test
  (testing "Should convert to document when all on one line"
    (is (= (to-document "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm")
           {:ecl "gry" :pid "860033327" :eyr "2020" :hcl "#fffffd" :byr "1937" :iyr "2017" :cid "147" :hgt "183cm"})))
  (testing "Should convert to document when on separate lines"
    (is (= (to-document "ecl:gry pid:860033327\neyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017\ncid:147 hgt:183cm")
           {:ecl "gry" :pid "860033327" :eyr "2020" :hcl "#fffffd" :byr "1937" :iyr "2017" :cid "147" :hgt "183cm"})))
  )

(deftest valid-document-test
  (testing "Should count as valid when all required fields are present"
    (is (true? (valid-document? {:byr "" :iyr "" :eyr "" :hgt "" :hcl "" :ecl "" :pid ""}))))
  (testing "Should count as invalid when all required fields are not present"
    (is (false? (valid-document? {:byr "" :iyr "" :hgt "" :hcl "" :ecl "" :pid ""})))))

(deftest extra-valid-document-test
  (testing "Should validate birth year"
    (is (false? (extra-valid-document? {:pid "087499704" :hgt "74in" :ecl "grn" :iyr "2020" :eyr "2030" :byr "1919" :hcl "#623a2f"})))
    (is (false? (extra-valid-document? {:pid "087499704" :hgt "74in" :ecl "grn" :iyr "2020" :eyr "2030" :byr "2003" :hcl "#623a2f"})))
    (is (true? (extra-valid-document? {:pid "087499704" :hgt "74in" :ecl "grn" :iyr "2020" :eyr "2030" :byr "1920" :hcl "#623a2f"})))
    (is (true? (extra-valid-document? {:pid "087499704" :hgt "74in" :ecl "grn" :iyr "2020" :eyr "2030" :byr "2002" :hcl "#623a2f"})))
    )
  (testing "Should validate issue year"
    (is (false? (extra-valid-document? {:pid "087499704" :hgt "74in" :ecl "grn" :iyr "2009" :eyr "2030" :byr "1992" :hcl "#623a2f"})))
    (is (false? (extra-valid-document? {:pid "087499704" :hgt "74in" :ecl "grn" :iyr "2021" :eyr "2030" :byr "1992" :hcl "#623a2f"})))
    (is (true? (extra-valid-document? {:pid "087499704" :hgt "74in" :ecl "grn" :iyr "2010" :eyr "2030" :byr "1992" :hcl "#623a2f"})))
    (is (true? (extra-valid-document? {:pid "087499704" :hgt "74in" :ecl "grn" :iyr "2020" :eyr "2030" :byr "1992" :hcl "#623a2f"})))
    )
  (testing "Should validate expiration year"
    (is (false? (extra-valid-document? {:pid "087499704" :hgt "74in" :ecl "grn" :iyr "2015" :eyr "2019" :byr "1992" :hcl "#623a2f"})))
    (is (false? (extra-valid-document? {:pid "087499704" :hgt "74in" :ecl "grn" :iyr "2015" :eyr "2031" :byr "1992" :hcl "#623a2f"})))
    (is (true? (extra-valid-document? {:pid "087499704" :hgt "74in" :ecl "grn" :iyr "2015" :eyr "2020" :byr "1992" :hcl "#623a2f"})))
    (is (true? (extra-valid-document? {:pid "087499704" :hgt "74in" :ecl "grn" :iyr "2015" :eyr "2030" :byr "1992" :hcl "#623a2f"})))
    )
  (testing "Should validate height"
    (is (false? (extra-valid-document? {:pid "087499704" :hgt "58in" :ecl "grn" :iyr "2015" :eyr "2025" :byr "1992" :hcl "#623a2f"})))
    (is (true? (extra-valid-document? {:pid "087499704" :hgt "59in" :ecl "grn" :iyr "2015" :eyr "2025" :byr "1992" :hcl "#623a2f"})))
    (is (true? (extra-valid-document? {:pid "087499704" :hgt "76in" :ecl "grn" :iyr "2015" :eyr "2025" :byr "1992" :hcl "#623a2f"})))
    (is (false? (extra-valid-document? {:pid "087499704" :hgt "77in" :ecl "grn" :iyr "2015" :eyr "2025" :byr "1992" :hcl "#623a2f"})))
    (is (false? (extra-valid-document? {:pid "087499704" :hgt "149cm" :ecl "grn" :iyr "2015" :eyr "2025" :byr "1992" :hcl "#623a2f"})))
    (is (true? (extra-valid-document? {:pid "087499704" :hgt "150cm" :ecl "grn" :iyr "2015" :eyr "2025" :byr "1992" :hcl "#623a2f"})))
    (is (true? (extra-valid-document? {:pid "087499704" :hgt "193cm" :ecl "grn" :iyr "2015" :eyr "2025" :byr "1992" :hcl "#623a2f"})))
    (is (false? (extra-valid-document? {:pid "087499704" :hgt "194cm" :ecl "grn" :iyr "2015" :eyr "2025" :byr "1992" :hcl "#623a2f"})))
    )
  (testing "Should validate hair color"
    (is (false? (extra-valid-document? {:pid "087499704" :hgt "74in" :ecl "grn" :iyr "2015" :eyr "2025" :byr "1992" :hcl "#623a2"})))
    (is (false? (extra-valid-document? {:pid "087499704" :hgt "74in" :ecl "grn" :iyr "2015" :eyr "2025" :byr "1992" :hcl "#623a2fa"})))
    (is (false? (extra-valid-document? {:pid "087499704" :hgt "74in" :ecl "grn" :iyr "2015" :eyr "2025" :byr "1992" :hcl "garbage"})))
    (is (false? (extra-valid-document? {:pid "087499704" :hgt "74in" :ecl "grn" :iyr "2015" :eyr "2025" :byr "1992" :hcl "#gg..gg"})))
    (is (true? (extra-valid-document? {:pid "087499704" :hgt "74in" :ecl "grn" :iyr "2015" :eyr "2025" :byr "1992" :hcl "#623a2f"})))
    )
  (testing "Should validate eye color"
    (is (true? (extra-valid-document? {:pid "087499704" :hgt "74in" :ecl "amb" :iyr "2015" :eyr "2025" :byr "1992" :hcl "#623a2f"})))
    (is (true? (extra-valid-document? {:pid "087499704" :hgt "74in" :ecl "blu" :iyr "2015" :eyr "2025" :byr "1992" :hcl "#623a2f"})))
    (is (true? (extra-valid-document? {:pid "087499704" :hgt "74in" :ecl "brn" :iyr "2015" :eyr "2025" :byr "1992" :hcl "#623a2f"})))
    (is (true? (extra-valid-document? {:pid "087499704" :hgt "74in" :ecl "gry" :iyr "2015" :eyr "2025" :byr "1992" :hcl "#623a2f"})))
    (is (true? (extra-valid-document? {:pid "087499704" :hgt "74in" :ecl "grn" :iyr "2015" :eyr "2025" :byr "1992" :hcl "#623a2f"})))
    (is (true? (extra-valid-document? {:pid "087499704" :hgt "74in" :ecl "hzl" :iyr "2015" :eyr "2025" :byr "1992" :hcl "#623a2f"})))
    (is (true? (extra-valid-document? {:pid "087499704" :hgt "74in" :ecl "oth" :iyr "2015" :eyr "2025" :byr "1992" :hcl "#623a2f"})))
    (is (false? (extra-valid-document? {:pid "087499704" :hgt "74in" :ecl "bla" :iyr "2015" :eyr "2025" :byr "1992" :hcl "#623a2f"})))
    (is (false? (extra-valid-document? {:pid "087499704" :hgt "74in" :ecl "0123" :iyr "2015" :eyr "2025" :byr "1992" :hcl "#623a2f"})))
    )
  (testing "Should validate passport ID color"
    (is (true? (extra-valid-document? {:pid "087499704" :hgt "74in" :ecl "grn" :iyr "2015" :eyr "2025" :byr "1992" :hcl "#623a2f"})))
    (is (false? (extra-valid-document? {:pid "08749974" :hgt "74in" :ecl "grn" :iyr "2015" :eyr "2025" :byr "1992" :hcl "#623a2f"})))
    (is (false? (extra-valid-document? {:pid "0874997040" :hgt "74in" :ecl "grn" :iyr "2015" :eyr "2025" :byr "1992" :hcl "#623a2f"})))
    (is (false? (extra-valid-document? {:pid "abcdefghi" :hgt "74in" :ecl "grn" :iyr "2015" :eyr "2025" :byr "1992" :hcl "#623a2f"}))))
  )


(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "10\n"
           (with-out-str (main-1))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "6\n"
           (with-out-str (main-2))))))
