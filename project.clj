(defproject ishihara "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/tools.cli "0.4.2"]
                 [clj-kdtree "1.2.0" :exclusions [org.clojure/clojure]]
                 [quil "3.1.1-SNAPSHOT"]
                 [com.clojure-goes-fast/clj-async-profiler "0.4.0"]]
  :aot [ishihara.core]
  :main ishihara.core)
