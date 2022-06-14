(defproject elevation-plot "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[com.taoensso/truss "1.6.0"]
                 [org.clojure/clojure "1.10.3"]
                 [org.clojure/data.csv "1.0.1"]
                 [org.clojure/math.numeric-tower "0.0.5"]
                 [org.clojure/tools.cli "1.0.206"]
                 [org.clojure/tools.trace "0.7.11"]
                 [quil "3.1.0"]
                 [com.rpl/specter "1.1.4"]]
  :main ^:skip-aot elevation-plot.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
