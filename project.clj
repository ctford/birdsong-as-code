(defproject birdsong-as-code "0.1.0-SNAPSHOT"
  :main ^{:skip-aot true} birdsong-as-code.song
  :jvm-opts ^:replace []
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [overtone "0.10.1"]
                 [leipzig "0.10.0"]])
