(ns sutra.calx.fn.stats-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.calx.core :as c]
            [sutra.calx.fn.stats :as s]
            [sutra.calx.test-helpers :refer [approx= err]]))

(def wb (c/workbook (array-map "Sheet1" [[1 2 3] ["x" true nil] ["'"]]
                               "Data" [["a" 10 1] ["b" 20 2] ["a" 30 3] ["c" 40 4]])
                    {:functions s/functions}))

(defn calc [f] (c/calculate wb f))

(defn- close
  ([f expected] (close f expected 1e-6))
  ([f expected eps]
   (let [r (calc f)]
     (is (approx= r expected eps) (str f " => " (pr-str r) ", expected " expected)))))

(defn- close-array [f expected eps]
  (let [r (calc f)]
    (is (and (= (count r) (count expected))
             (every? true?
                     (map (fn [ra re]
                            (and (= (count ra) (count re))
                                 (every? true?
                                         (map #(if (number? %2) (approx= %1 %2 eps) (= %1 %2)) ra
                                              re))))
                          r expected)))
        (str f " => " (pr-str r)))))

(deftest averages-and-counts
  (testing "range vs direct-argument coercion"
    (is (= 2.0 (calc "=AVERAGE(A1:C2)")))
    (is (= 1.4 (calc "=AVERAGEA(A1:C2)")))
    (is (= 2.0 (calc "=AVERAGE(TRUE,3)")))
    (is (= (err :div0) (calc "=AVERAGE(A2)")))
    (is (= (err :value) (calc "=AVERAGE(\"x\")")))
    (is (= 3.0 (calc "=COUNT(A1:C2)")))
    (is (= 3.0 (calc "=COUNT(1,\"2\",\"x\",TRUE)")))
    (is (= 5.0 (calc "=COUNTA(A1:C2)")))
    (is (= 4.0 (calc "=COUNTBLANK(A1:C3)")))
    (is (= 25.0 (calc "=COUNTBLANK(A1:C10)"))))
  (testing "MAX/MIN family"
    (is (= 3.0 (calc "=MAX(A1:C2)")))
    (is (= 0.0 (calc "=MAX(A2)")))
    (is (= 0.0 (calc "=MINA(A1:C2)")))
    (is (= 3.0 (calc "=MAXA(A1:C2)")))
    (is (= -1.0 (calc "=MIN(A1:C2,-1)"))))
  (testing "errors in ranges propagate"
    (is (= (err :div0) (calc "=AVERAGE({1,2},1/0)")))))

(deftest conditional-aggregates
  (is (= 2.0 (calc "=COUNTIF(A1:C2,\">1\")")))
  (is (= 4.0 (calc "=COUNTIF(A1:C3,\"\")")))
  (is (= 25.0 (calc "=COUNTIF(A1:C10,\"\")")))
  (is (= 2.0 (calc "=COUNTIF(Data!A1:A4,\"a\")")))
  (is (= [[2.0 1.0]] (calc "=COUNTIF(Data!A1:A4,{\"a\",\"b\"})")))
  (is (= 1.0 (calc "=COUNTIFS(Data!A1:A4,\"a\",Data!B1:B4,\">15\")")))
  (is (= (err :value) (calc "=COUNTIFS(Data!A1:A4,\"a\",Data!B1:B3,\">15\")")))
  (is (= 20.0 (calc "=AVERAGEIF(Data!A1:A4,\"a\",Data!B1:B4)")))
  (is (= 20.0 (calc "=AVERAGEIF(Data!A1:A4,\"a\",Data!B1)")))
  (is (= 30.0 (calc "=AVERAGEIF(Data!B1:B4,\">10\")")))
  (is (= (err :div0) (calc "=AVERAGEIF(Data!A1:A4,\"z\",Data!B1:B4)")))
  (is (= 30.0 (calc "=AVERAGEIFS(Data!B1:B4,Data!A1:A4,\"a\",Data!C1:C4,\">1\")")))
  (is (= 30.0 (calc "=MAXIFS(Data!B1:B4,Data!A1:A4,\"a\")")))
  (is (= 10.0 (calc "=MINIFS(Data!B1:B4,Data!A1:A4,\"a\")")))
  (is (= 0.0 (calc "=MAXIFS(Data!B1:B4,Data!A1:A4,\"z\")"))))

(deftest descriptive
  (let [d "{1345,1301,1368,1322,1310,1370,1318,1350,1303,1299}"]
    (close (str "=STDEV.S(" d ")") 27.46391572)
    (close (str "=STDEV(" d ")") 27.46391572)
    (close (str "=STDEV.P(" d ")") 26.05455814)
    (close (str "=VAR.S(" d ")") 754.2666667)
    (close (str "=VARP(" d ")") 678.84))
  (is (= (err :div0) (calc "=STDEV(1)")))
  (close "=STDEVA(A1:C2)" 1.140175425)
  (close "=VARPA(A1:C2)" 1.04)
  (is (= 3.5 (calc "=MEDIAN(1,2,3,4,5,6)")))
  (is (= (err :num) (calc "=MEDIAN(A2)")))
  (is (= 3.0 (calc "=MODE.SNGL({1,3,3,2,2})")))
  (is (= (err :na) (calc "=MODE({1,2,3})")))
  (is (= [[1.0] [2.0] [3.0]] (calc "=MODE.MULT({1,2,3,4,3,2,1,2,3,5,6,1})")))
  (close "=GEOMEAN(4,5,8,7,11,4,3)" 5.476986969)
  (close "=HARMEAN(4,5,8,7,11,4,3)" 5.028375962)
  (is (= (err :num) (calc "=GEOMEAN(1,0)")))
  (close "=AVEDEV(4,5,6,7,5,4,3)" 1.020408163)
  (is (= 48.0 (calc "=DEVSQ(4,5,8,7,11,4,3)")))
  (close "=TRIMMEAN({4,5,6,7,2,3,4,5,1,2,3},0.2)" 3.777777778)
  (close "=KURT(3,4,5,2,3,4,5,6,4,7)" -0.151799637)
  (close "=SKEW(3,4,5,2,3,4,5,6,4,7)" 0.359543071)
  (close "=SKEW.P(3,4,5,2,3,4,5,6,4,7)" 0.303193339)
  (is (= (err :div0) (calc "=KURT(1,2,3)")))
  (close "=STANDARDIZE(42,40,1.5)" 1.333333333))

(deftest order-statistics
  (is (= 5.0 (calc "=LARGE({3,5,3,5,4,4,2,4,6,7},3)")))
  (is (= 3.0 (calc "=SMALL({3,5,3,5,4,4,2,4,6,7},3)")))
  (is (= [[7.0 6.0]] (calc "=LARGE({3,5,3,5,4,4,2,4,6,7},{1,2})")))
  (is (= (err :num) (calc "=LARGE({1,2},3)")))
  (is (= 4.0 (calc "=RANK.AVG(94,{89,88,92,101,94,97,95})")))
  (is (= 5.0 (calc "=RANK.EQ(7,{7,3.5,3.5,1,2},1)")))
  (is (= 3.0 (calc "=RANK(3.5,{7,3.5,3.5,1,2},1)")))
  (is (= 3.5 (calc "=RANK.AVG(3.5,{7,3.5,3.5,1,2},1)")))
  (is (= (err :na) (calc "=RANK(9,{1,2})")))
  (close "=PERCENTILE.INC({1,3,2,4},0.3)" 1.9)
  (close "=PERCENTILE({1,3,2,4},0.3)" 1.9)
  (close "=PERCENTILE.EXC({1,2,3,6,6,6,7,8,9},0.25)" 2.5)
  (is (= (err :num) (calc "=PERCENTILE.EXC({1,2,3},0.1)")))
  (close "=QUARTILE.INC({1,2,4,7,8,9,10,12},1)" 3.5)
  (close "=QUARTILE.EXC({6,7,15,36,39,40,41,42,43,47,49},1)" 15)
  (close "=QUARTILE.EXC({6,7,15,36,39,40,41,42,43,47,49},3)" 43)
  (is (= (err :num) (calc "=QUARTILE.EXC({1,2,3},4)")))
  (let [d "{13,12,11,8,4,3,2,1,1,1}"]
    (close (str "=PERCENTRANK.INC(" d ",2)") 0.333)
    (close (str "=PERCENTRANK.INC(" d ",4)") 0.555)
    (close (str "=PERCENTRANK(" d ",8)") 0.666)
    (close (str "=PERCENTRANK.INC(" d ",5)") 0.583)
    (is (= (err :na) (calc (str "=PERCENTRANK(" d ",20)")))))
  (close "=PERCENTRANK.EXC({1,2,3,6,6,6,7,8,9},7)" 0.7)
  (close "=PERCENTRANK.EXC({1,2,3,6,6,6,7,8,9},5.43)" 0.381)
  (close "=PERCENTRANK.EXC({1,2,3,6,6,6,7,8,9},5.43,1)" 0.3)
  (is (= [[1.0] [2.0] [4.0] [2.0]] (calc "=FREQUENCY({79,85,78,85,50,81,95,88,97},{70,79,89})")))
  (is (= [[1.0] [2.0] [0.0]] (calc "=FREQUENCY({1,2,3},{5,2})"))))

(deftest paired-statistics
  (close "=CORREL({3,2,4,5,6},{9,7,12,15,17})" 0.997054486)
  (close "=PEARSON({9,7,5,3,1},{10,6,1,5,3})" 0.699379)
  (close "=RSQ({2,3,9,1,8,7,5},{6,5,11,7,5,4,4})" 0.05795)
  (close "=SLOPE({2,3,9,1,8,7,5},{6,5,11,7,5,4,4})" 0.305555556)
  (close "=INTERCEPT({2,3,9,1,8},{6,5,11,7,5})" 0.048387097)
  (close "=STEYX({2,3,9,1,8,7,5},{6,5,11,7,5,4,4})" 3.305718950)
  (close "=COVARIANCE.P({3,2,4,5,6},{9,7,12,15,17})" 5.2)
  (close "=COVAR({3,2,4,5,6},{9,7,12,15,17})" 5.2)
  (close "=COVARIANCE.S({2,4,8},{5,11,12})" 9.666666667)
  (close "=FORECAST(30,{6,7,9,15,21},{20,28,31,38,40})" 10.607253)
  (close "=FORECAST.LINEAR(30,{6,7,9,15,21},{20,28,31,38,40})" 10.607253)
  (is (= (err :na) (calc "=CORREL({1,2,3},{1,2})")))
  (is (= (err :div0) (calc "=SLOPE({1,2},{3,3})")))
  (close "=PROB({0,1,2,3},{0.2,0.3,0.1,0.4},2)" 0.1)
  (close "=PROB({0,1,2,3},{0.2,0.3,0.1,0.4},1,3)" 0.8)
  (is (= (err :num) (calc "=PROB({0,1},{0.2,0.3},1)"))))

(deftest regression
  (close-array "=LINEST({1,9,5,7},{0,4,2,3})" [[2.0 1.0]] 1e-9)
  (close-array "=LINEST({1,2,3,5},{1,2,3,4},TRUE,TRUE)"
               [[1.3 -0.5]
                [0.173205081 0.474341649]
                [0.965714286 0.387298335]
                [56.33333333 2.0]
                [8.45 0.3]] 1e-8)
  (testing "multiple regression"
    (close-array "=LINEST({9;8;19;18;29},{1,2;2,1;3,4;4,3;5,6})" [[3.0 2.0 1.0]] 1e-9))
  (testing "no constant"
    (close-array "=LINEST({2,4,6},{1,2,3},FALSE)" [[2.0 0.0]] 1e-9))
  (let [y "{33100,47300,69000,102000,150000,220000}" x "{11,12,13,14,15,16}"]
    (close-array (str "=LOGEST(" y "," x ")") [[1.463275628 495.3047]] 1e-6)
    (close-array (str "=GROWTH(" y "," x ",{17,18})") [[320196.7184 468536.0539]] 1e-6))
  (close "=TREND({133890,135000,135790,137300,138130,139100,139900,141120,141890,143230,144000,145290},{1,2,3,4,5,6,7,8,9,10,11,12},13)"
         146171.5152 1e-8)
  (close-array "=TREND({1,2,3,5})" [[0.8 2.1 3.4 4.7]] 1e-9))

(deftest hypothesis-tests
  (close "=T.TEST({3,4,5,8,9,1,2,4,5},{6,19,3,2,14,4,5,17,1},2,1)" 0.196015785)
  (close "=TTEST({3,4,5,8,9,1,2,4,5},{6,19,3,2,14,4,5,17,1},2,2)" 0.191995887)
  (close "=T.TEST({3,4,5,8,9,1,2,4,5},{6,19,3,2,14,4,5,17,1},1,3)" 0.101146962)
  (is (= (err :num) (calc "=T.TEST({1,2},{3,4},3,1)")))
  (close "=F.TEST({6,7,9,15,21},{20,28,31,38,40})" 0.648317846)
  (close "=Z.TEST({3,6,7,8,6,5,4,2,1,9},4)" 0.090574197)
  (close "=Z.TEST({3,6,7,8,6,5,4,2,1,9},6)" 0.863043389)
  (close "=CHISQ.TEST({58,35;11,25;10,23},{45.35,47.65;17.56,18.44;16.09,16.91})" 0.000308192)
  (is (= (err :na) (calc "=CHITEST({1,2},{1;2})"))))

(deftest normal-family
  (close "=NORM.DIST(42,40,1.5,TRUE)" 0.908788780)
  (close "=NORM.DIST(42,40,1.5,FALSE)" 0.109340050)
  (close "=NORMDIST(42,40,1.5,TRUE)" 0.908788780)
  (close "=NORM.INV(0.908789,40,1.5)" 42.000002)
  (close "=NORM.S.DIST(1.333333,TRUE)" 0.908788726)
  (close "=NORM.S.DIST(1.333333,FALSE)" 0.164010148)
  (close "=NORMSDIST(-8)" 6.22096057e-16 1e-6)
  (close "=NORM.S.INV(0.908789)" 1.333334673)
  (close "=NORMSINV(0.975)" 1.959963985 1e-9)
  (close "=NORMSINV(1E-10)" -6.361340902 1e-9)
  (is (= (err :num) (calc "=NORM.S.INV(0)")))
  (is (= (err :num) (calc "=NORM.DIST(1,0,0,TRUE)")))
  (close "=LOGNORM.DIST(4,3.5,1.2,TRUE)" 0.039083556)
  (close "=LOGNORM.DIST(4,3.5,1.2,FALSE)" 0.017617597)
  (close "=LOGNORMDIST(4,3.5,1.2)" 0.039083556)
  (close "=LOGNORM.INV(0.039084,3.5,1.2)" 4.000025219)
  (close "=PHI(0.75)" 0.301137432)
  (close "=GAUSS(2)" 0.477249868)
  (close "=CONFIDENCE.NORM(0.05,2.5,50)" 0.692951912)
  (close "=CONFIDENCE(0.05,2.5,50)" 0.692951912)
  (close "=CONFIDENCE.T(0.05,1,50)" 0.284196855)
  (close "=FISHER(0.75)" 0.972955075)
  (close "=FISHERINV(0.972955)" 0.75)
  (is (= [[0.5 0.841344746]] (mapv #(mapv (fn [x] (/ (Math/round (* x 1e9)) 1e9)) %)
                                   (calc "=NORM.S.DIST({0,1},TRUE)")))))

(deftest discrete-distributions
  (close "=BINOM.DIST(6,10,0.5,FALSE)" 0.205078125)
  (close "=BINOMDIST(6,10,0.5,TRUE)" 0.828125)
  (close "=BINOM.DIST.RANGE(60,0.75,48)" 0.083974967)
  (close "=BINOM.DIST.RANGE(60,0.75,45,50)" 0.523629793)
  (is (= 4.0 (calc "=BINOM.INV(6,0.5,0.75)")))
  (is (= 4.0 (calc "=CRITBINOM(6,0.5,0.75)")))
  (is (= (err :num) (calc "=BINOM.DIST(11,10,0.5,FALSE)")))
  (close "=POISSON.DIST(2,5,TRUE)" 0.124652019)
  (close "=POISSON(2,5,FALSE)" 0.084224337)
  (close "=NEGBINOM.DIST(10,5,0.25,TRUE)" 0.313514058)
  (close "=NEGBINOM.DIST(10,5,0.25,FALSE)" 0.055048660)
  (close "=NEGBINOMDIST(10,5,0.25)" 0.055048660)
  (close "=HYPGEOM.DIST(1,4,8,20,TRUE)" 0.465428277)
  (close "=HYPGEOM.DIST(1,4,8,20,FALSE)" 0.363261094)
  (close "=HYPGEOMDIST(1,4,8,20)" 0.363261094))

(deftest continuous-distributions
  (close "=EXPON.DIST(0.2,10,TRUE)" 0.864664717)
  (close "=EXPONDIST(0.2,10,FALSE)" 1.353352832)
  (close "=GAMMA.DIST(10.00001131,9,2,FALSE)" 0.032639)
  (close "=GAMMA.DIST(10.00001131,9,2,TRUE)" 0.068094)
  (close "=GAMMA.INV(0.068094,9,2)" 10.0000112)
  (close "=GAMMAINV(0.068094,9,2)" 10.0000112)
  (close "=BETA.DIST(2,8,10,TRUE,1,3)" 0.685470581)
  (close "=BETA.DIST(2,8,10,FALSE,1,3)" 1.483764648)
  (close "=BETADIST(2,8,10,1,3)" 0.685470581)
  (close "=BETA.INV(0.685470581,8,10,1,3)" 2.0)
  (close "=BETAINV(0.2,4,5)" 0.303225845)
  (close "=WEIBULL.DIST(105,20,100,TRUE)" 0.929581390)
  (close "=WEIBULL(105,20,100,FALSE)" 0.035588864)
  (close "=GAMMA(2.5)" 1.329340388)
  (close "=GAMMA(-0.75)" -4.834146080)
  (is (= 24.0 (calc "=GAMMA(5)")))
  (is (= (err :num) (calc "=GAMMA(-1)")))
  (close "=GAMMALN(4)" 1.791759469)
  (close "=GAMMALN.PRECISE(4.5)" 2.453736571)
  (is (= (err :num) (calc "=GAMMALN(0)"))))

(deftest chisq-t-f
  (close "=CHISQ.DIST(0.5,1,TRUE)" 0.520499878)
  (close "=CHISQ.DIST(2,3,FALSE)" 0.207553749)
  (close "=CHISQ.DIST.RT(18.307,10)" 0.050000589)
  (close "=CHIDIST(18.307,10)" 0.050000589)
  (close "=CHISQ.INV(0.93,1)" 3.283020287)
  (close "=CHISQ.INV(0.6,2)" 1.832581464)
  (close "=CHISQ.INV.RT(0.050001,10)" 18.30697346)
  (close "=CHIINV(0.050001,10)" 18.30697346)
  (close "=T.DIST(60,1,TRUE)" 0.994695326)
  (close "=T.DIST(8,3,FALSE)" 0.000736906)
  (close "=T.DIST.2T(1.959999998,60)" 0.054644930)
  (close "=T.DIST.RT(1.959999998,60)" 0.027322465)
  (close "=TDIST(1.959999998,60,2)" 0.054644930)
  (close "=TDIST(1.959999998,60,1)" 0.027322465)
  (is (= (err :num) (calc "=TDIST(-1,60,1)")))
  (close "=T.INV(0.75,2)" 0.816496581)
  (close "=T.INV(0.25,2)" -0.816496581)
  (close "=T.INV.2T(0.546449,60)" 0.606533)
  (close "=TINV(0.05,10)" 2.228138852)
  (close "=F.DIST(15.2069,6,4,TRUE)" 0.99)
  (close "=F.DIST(15.2069,6,4,FALSE)" 0.001223792)
  (close "=F.DIST.RT(15.2068649,6,4)" 0.01)
  (close "=FDIST(15.2068649,6,4)" 0.01)
  (close "=F.INV(0.01,6,4)" 0.109309914)
  (close "=F.INV.RT(0.01,6,4)" 15.20686486)
  (close "=FINV(0.01,6,4)" 15.20686486)
  (is (= (err :num) (calc "=F.DIST(-1,6,4,TRUE)"))))
