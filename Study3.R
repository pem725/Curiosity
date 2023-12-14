## Study 3 analysis

### here are my (PEM's additions to Dave's analysis)

### 5-factor cfa - changed according to the top non-correlated error modification indices from Dave's model
m3 <- "
je =~ je1 + je2 + je3 + je4 + je5 + sc1 + sc2
ds =~ ds1 + ds2 + ds3 + ds4 + ds5
str =~ st1r + st2r + st3r + st4r + st5r + ds2
sc =~ sc1 +  sc2 + sc3 + sc4 + sc5
ts =~ ts1 + ts2 + ts3 + ts4 + ts5
je ~~ ds + str + sc + ts
ds ~~ str + sc + ts
str ~~ sc + ts
sc ~~ ts"
f2.p <- cfa(model = m3, data = ds, meanstructure = T, fixed.x = F,
          estimator = "MLM", likelihood = "wishart")
summary(f2.p, fit.measures = T, standardized = T)
modificationIndices(f2.p,sort.=T,high.power=0.8)
f2.pw <- lavaan.survey(lavaan.fit = f2.p, survey.design = wgt25, estimator = "MLM")



