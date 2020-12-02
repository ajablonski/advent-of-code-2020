(defproject advent-of-code-2020 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url  "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :profiles {:dev {:resource-paths ["test_resources"]}
             :day1-1 {:main advent-of-code-2020.day1/main-1}
             :day1-2 {:main advent-of-code-2020.day1/main-2}
             :day2-1 {:main advent-of-code-2020.day2/main-1}
             :day2-2 {:main advent-of-code-2020.day2/main-2}
             })
