### NEW CURIOSITY MEASURE (5D-Curiosity) CFAs ###############################
### 3000 people representative sample #######################################

### notes
# sampling weights are caled "weight" - use them for the CFA
# reverse score stress tolerance items so they actually reflect greater stress tolerance

### libraries
library(foreign)
library(psych)
library(car)
library(multicon)
library(lavaan)
library(survey)
library(lavaan.survey)

### input data
d <- read.spss(file = "Curiosity study 3 cfa 092816.sav", use.value.labels = F,
                 to.data.frame = T, use.missings = T)
names(d[1:200]); nrow(d)

### 25 items ### 25 items ### 25 items ### 25 items ### 25 items ### 25 items

### rename items
ds <- d[,c("je1","je2","je3","je4","je5", 
           "ds1","ds2","ds3","ds4","ds5",
           "st1","st2","st3","st4","st5",
           "sc1","sc2","sc3","sc4","sc5",
           "ts1","ts2","ts3","ts4","ts5",
           "weight")]
names(ds)


### reverse code items
ds[,c("st1r","st2r","st3r","st4r","st5r")] <- sapply(
  ds[,c("st1","st2","st3","st4","st5")], function(x) {8-x})
cor(ds$st1,ds$st1r)

### exploratory data analysis
head(ds, n = 25)
sum(is.na(ds)) # no missing data
range(ds[,1:25])
range(ds$weight)
hist(ds[,c(1)]); skew(ds[,c(1)])
hist(ds[,c(6)]); skew(ds[,c(6)])
hist(ds[,c(11)]); skew(ds[,c(11)])
hist(ds[,c(16)]); skew(ds[,c(16)])
hist(ds[,c(21)]); skew(ds[,c(21)])
hist(ds[,c(27)]); skew(ds[,c(27)])

### correlation matrix
names(ds[,c(1:10,16:25,27:31)])
r <- cor(ds[,c(1:10,16:25,27:31)])
range(r)
table(r); hist(r, n = 30)


## EFA 2.18.17 for factor loadings table

pa <- fa(r = ds[,c("je1","je2","je3","je4","je5", 
                   "ds1","ds2","ds3","ds4","ds5",
                   "st1","st2","st3","st4","st5",
                   "sc1","sc2","sc3","sc4","sc5",
                   "ts1","ts2","ts3","ts4","ts5")], nfactors = 5, rotate = "promax",
          residuals = T, SMC = T, max.iter = 100, fm = "pa"); summary(pa)
pa$dof; pa$fit
print(x = pa$Structure, digits = 2, cutoff = .00) # Structure loadings
print(x = pa$loadings, digits = 2, cutoff = .00) # Pattern loadings
pa$Phi


### total scores
ds$je <- composite(set = ds[,c(1:5)]); head(ds$je)
ds$ds <- composite(set = ds[,c(6:10)]); head(ds$ds)
ds$st <- composite(set = ds[,c(11:15)]); head(ds$st)
ds$str <- composite(set = ds[,c(27:31)]); head(ds$str)
ds$sc <- composite(set = ds[,c(16:20)]); head(ds$sc)
ds$ts <- composite(set = ds[,c(21:25)]); head(ds$ts)
r <- cor(ds[,c("je","ds","str","sc","ts")]); print(r)


r <-  corr.test(ds[,c("je","ds","str","sc","ts")]); print(r$r)

round(r$r, 2)
round(r$p, 2)



### create survey design object
wgt25 <- svydesign(ids = ~1, weights = ds$weight, data = ds[,c(1:10,16:25,27:31)]); summary(wgt25)

### bifactor cfa
m1 <- "
cur =~ je1 + je2 + je3 + je4 + je5 + 
ds1 + ds2 + ds3 + ds4 + ds5 +
st1r + st2r + st3r + st4r + st5r + 
sc1 +  sc2 + sc3 + sc4 + sc5 + 
ts1 + ts2 + ts3 + ts4 + ts5
je =~ je1 + je2 + je3 + je4 + je5
ds =~ ds1 + ds2 + ds3 + ds4 + ds5
str =~ st1r + st2r + st3r + st4r + st5r
sc =~ sc1 +  sc2 + sc3 + sc4 + sc5
ts =~ ts1 + ts2 + ts3 + ts4 + ts5
cur ~~ 0*je + 0*ds + 0*str + 0*sc + 0*ts
je ~~ 0*ds + 0*str + 0*sc + 0*ts
ds ~~ 0*str + 0*sc + 0*ts
str ~~ 0*sc + 0*ts
sc ~~ 0*ts"
f1 <- cfa(model = m1, data = ds, meanstructure = T, fixed.x = F,
          estimator = "MLM", likelihood = "wishart")
summary(f1, fit.measures = T, standardized = T)
f1w <- lavaan.survey(lavaan.fit = f1, survey.design = wgt25,
                     estimator = "MLM")
summary(f1w, fit.measures = T, standardized = T)

### 5-factor cfa
m2 <- "
je =~ je1 + je2 + je3 + je4 + je5
ds =~ ds1 + ds2 + ds3 + ds4 + ds5
str =~ st1r + st2r + st3r + st4r + st5r
sc =~ sc1 +  sc2 + sc3 + sc4 + sc5
ts =~ ts1 + ts2 + ts3 + ts4 + ts5
je ~~ ds + str + sc + ts
ds ~~ str + sc + ts
str ~~ sc + ts
sc ~~ ts"
f2 <- cfa(model = m2, data = ds, meanstructure = T, fixed.x = F,
    estimator = "MLM", likelihood = "wishart")
summary(f2, fit.measures = T, standardized = T)
f2w <- lavaan.survey(lavaan.fit = f2, survey.design = wgt25,
              estimator = "MLM")
summary(f2w, fit.measures = T, standardized = T)
modificationIndices(f2w)
# je =~  ds5   41.319
# je =~  sc2   164.390
# str =~  ds2  85.774
# je4 ~~  je5  131.616
# sc1 ~~  sc2  254.139
# sc2 ~~  sc3  112.891
# sc3 ~~  sc4  205.143
# ts2 ~~  ts4  69.783

### higher-order cfa (5 lower factors)
m3 <- "
je =~ je1 + je2 + je3 + je4 + je5
ds =~ ds1 + ds2 + ds3 + ds4 + ds5
str =~ st1r + st2r + st3r + st4r + st5r
sc =~ sc1 +  sc2 + sc3 + sc4 + sc5
ts =~ ts1 + ts2 + ts3 + ts4 + ts5
cur =~ je + ds + str + sc + ts
je ~~ 0*ds + 0*str + 0*sc + 0*ts
ds ~~ 0*str + 0*sc + 0*ts
str ~~ 0*sc + 0*ts
sc ~~ 0*ts"
f3 <- cfa(model = m3, data = ds, meanstructure = T, fixed.x = F,
          estimator = "MLM", likelihood = "wishart")
summary(f3, fit.measures = T, standardized = T)
f3w <- lavaan.survey(lavaan.fit = f3, survey.design = wgt25,
                     estimator = "MLM")
summary(f3w, fit.measures = T, standardized = T)

### 6-factor cfa (no JE)
m4 <- "
je.per =~ je1 + je2 + je3
je.eps =~ je4 + je5
ds =~ ds1 + ds2 + ds3 + ds4 + ds5
str =~ st1r + st2r + st3r + st4r + st5r
sc =~ sc1 +  sc2 + sc3 + sc4 + sc5
ts =~ ts1 + ts2 + ts3 + ts4 + ts5
je.per ~~ je.eps + ds + str + sc + ts
je.eps ~~ ds + str + sc + ts
ds ~~ str + sc + ts
str ~~ sc + ts
sc ~~ ts"
f4 <- cfa(model = m4, data = ds, meanstructure = T, fixed.x = F,
          estimator = "MLM", likelihood = "wishart")
summary(f4, fit.measures = T, standardized = T)
f4w <- lavaan.survey(lavaan.fit = f4, survey.design = wgt25,
                     estimator = "MLM")
summary(f4w, fit.measures = T, standardized = T)

### 6-factor cfa (no SC)
m5 <- "
je =~ je1 + je2 + je3 + je4 + je5
ds =~ ds1 + ds2 + ds3 + ds4 + ds5
str =~ st1r + st2r + st3r + st4r + st5r
sc.psy =~ sc1 +  sc2
sc.gos =~ sc3 + sc4 + sc5
ts =~ ts1 + ts2 + ts3 + ts4 + ts5
je ~~ ds + str + sc.psy + sc.gos + ts
ds ~~ str + sc.psy + sc.gos + ts
str ~~ sc.psy + sc.gos + ts
sc.psy ~~ sc.gos + ts
sc.gos ~~ ts
"
f5 <- cfa(model = m5, data = ds, meanstructure = T, fixed.x = F,
          estimator = "MLM", likelihood = "wishart")
summary(f5, fit.measures = T, standardized = T)
f5w <- lavaan.survey(lavaan.fit = f5, survey.design = wgt25,
                     estimator = "MLM")
summary(f5w, fit.measures = T, standardized = T)

### 7-factor cfa (no JE & no SC)
m6 <- "
je.per =~ je1 + je2 + je3
je.eps =~ je4 + je5
ds =~ ds1 + ds2 + ds3 + ds4 + ds5
str =~ st1r + st2r + st3r + st4r + st5r
sc.psy =~ sc1 +  sc2
sc.gos =~ sc3 + sc4 + sc5
ts =~ ts1 + ts2 + ts3 + ts4 + ts5
je.per ~~ je.eps + ds + str + sc.psy + sc.gos + ts
je.eps ~~ ds + str + sc.psy + sc.gos + ts
ds ~~ str + sc.psy + sc.gos + ts
str ~~ sc.psy + sc.gos + ts
sc.psy ~~ sc.gos + ts
sc.gos ~~ ts
"
f6 <- cfa(model = m6, data = ds, meanstructure = T, fixed.x = F,
          estimator = "MLM", likelihood = "wishart")
summary(f6, fit.measures = T, standardized = T)
f6w <- lavaan.survey(lavaan.fit = f6, survey.design = wgt25,
                     estimator = "MLM")
summary(f6w, fit.measures = T, standardized = T)

### higher-order cfa (6 lower factors)
m7 <- "
je =~ je1 + je2 + je3 + je4 + je5
ds =~ ds1 + ds2 + ds3 + ds4 + ds5
str =~ st1r + st2r + st3r + st4r + st5r
sc.psy =~ sc1 +  sc2
sc.gos =~ sc3 + sc4 + sc5
ts =~ ts1 + ts2 + ts3 + ts4 + ts5
cur =~ je + ds + str + sc.psy + sc.gos + ts
je ~~ 0*ds + 0*str + 0*sc.psy + 0*sc.gos + 0*ts
ds ~~ 0*str + 0*sc.psy + 0*sc.gos + 0*ts
str ~~ 0*sc.psy + 0*sc.gos + 0*ts
sc.psy ~~ 0*sc.gos + 0*ts
sc.gos ~~ 0*ts
"
f7 <- cfa(model = m7, data = ds, meanstructure = T, fixed.x = F,
          estimator = "MLM", likelihood = "wishart")
summary(f7, fit.measures = T, standardized = T)
f7w <- lavaan.survey(lavaan.fit = f7, survey.design = wgt25,
                     estimator = "MLM")
summary(f7w, fit.measures = T, standardized = T)

### model comparisons (LRTs)

## 5-factor vs
lavTestLRT(f2w, f1w, method = "satorra.bentler.2001", type = "Chisq")
lavTestLRT(f2w, f3w, method = "satorra.bentler.2001", type = "Chisq")
lavTestLRT(f2w, f5w, method = "satorra.bentler.2001", type = "Chisq")
lavTestLRT(f2w, f4w, method = "satorra.bentler.2001", type = "Chisq")
lavTestLRT(f2w, f6w, method = "satorra.bentler.2001", type = "Chisq")

# other comparisons
lavTestLRT(f1w, f5w, method = "satorra.bentler.2001", type = "Chisq")
lavTestLRT(f5w, f7w, method = "satorra.bentler.2001", type = "Chisq")


##### LEVINS' ##########################################################################


### Now conducting a correlated 5 factor model for the 33 items #### ################## updated by Mel 9/30/16

ds33 <-d[,c("je1","je2","je3","je4","je5", "je6_33", "je7_33", "je8_33",
            "ds1","ds2","ds3","ds4","ds5", "ds6_33", 
            "st1","st2","st3","st4","st5", "st6_33",
            "sc1","sc2","sc3","sc4","sc5", "sc6_33",
            "ts1","ts2","ts3","ts4","ts5", "ts6_33", "ts7_33",
            "weight")] 

names(ds33)

### reverse code items
ds33[,c("st1r","st2r","st3r","st4r","st5r", "str6_33")] <- sapply(
  ds33[,c("st1","st2","st3","st4","st5", "st6_33")], function(x) {8-x})
cor(ds33$st1,ds33$st1r)

### create survey design object for 33 items 
wgt33 <- svydesign(ids = ~1, weights = ds33$weight, data = ds33[,c(1:14, 21:33, 35:40)]); summary(wgt33)

### correlated factor cfa for 33 items 
m2 <- "
je =~ je1 + je2 + je3 + je4 + je5 + je6_33 + je7_33 + je8_33
ds =~ ds1 + ds2 + ds3 + ds4 + ds5 + ds6_33
str =~ st1r + st2r + st3r + st4r + st5r + str6_33
sc =~ sc1 +  sc2 + sc3 + sc4 + sc5 + sc6_33
ts =~ ts1 + ts2 + ts3 + ts4 + ts5 + ts6_33
je ~~ ds + str + sc + ts
ds ~~ str + sc + ts
str ~~ sc + ts
sc ~~ ts"
f2 <- cfa(model = m2, data = ds33, meanstructure = F, fixed.x = F,
          estimator = "MLM", likelihood = "wishart")
summary(f2, fit.measures = T, standardized = T)
f2w <- lavaan.survey(lavaan.fit = f2, survey.design = wgt33,
                     estimator = "MLM")
summary(f2w, fit.measures = T, standardized = T)
modificationIndices(f2w)



### Preliminary modification indices on the old 25 items ##########################

### correlated factor cfa - splitting up sc
m1.5 <- "
je =~ je1 + je2 + je3 + je4 + je5
ds =~ ds1 + ds2 + ds3 + ds4 + ds5
str =~ st1r + st2r + st3r + st4r + st5r
sc.psy =~ sc1 +  sc2
sc.gos =~ sc3 + sc4 + sc5
ts =~ ts1 + ts2 + ts3 + ts4 + ts5
je ~~ ds + str + sc.psy + sc.gos + ts
ds ~~ str + sc.psy + sc.gos + ts
str ~~ sc.psy + sc.gos + ts
sc.psy ~~ sc.gos + ts
sc.gos ~~ ts"
f1.5 <- cfa(model = m1.5, data = ds, meanstructure = F, fixed.x = F,
          estimator = "ML", likelihood = "wishart",)
summary(f1.5, fit.measures = T, standardized = T)
lavTestLRT(f1, f1.5, method = "satorra.bentler.2010", type = "Chisq")
modificationIndices(f1.5)
# je =~ ds1
# str =~ ds2
# je4 ~~ je5
# ts2 ~~ ts4

### correlated factor cfa - splitting up sc & je
m1.6 <- "
je.per =~ je1 + je2 + je3
je.eps =~ je4 + je5
ds =~ ds1 + ds2 + ds3 + ds4 + ds5
str =~ st1r + st2r + st3r + st4r + st5r
sc.psy =~ sc1 +  sc2
sc.gos =~ sc3 + sc4 + sc5
ts =~ ts1 + ts2 + ts3 + ts4 + ts5
je.per ~~ je.eps + ds + str + sc.psy + sc.gos + ts
je.eps ~~ ds + str + sc.psy + sc.gos + ts
ds ~~ str + sc.psy + sc.gos + ts
str ~~ sc.psy + sc.gos + ts
sc.psy ~~ sc.gos + ts
sc.gos ~~ ts"
f1.6 <- cfa(model = m1.6, data = ds, meanstructure = F, fixed.x = F,
            estimator = "ML", likelihood = "wishart",)
summary(f1.6, fit.measures = T, standardized = T)
lavTestLRT(f1.5, f1.6, method = "satorra.bentler.2010", type = "Chisq")
modificationIndices(f1.6)

### correlated factor cfa - splitting up sc & je error r
m1.7 <- "
je =~ je1 + je2 + je3 + je4 + je5
je4 ~~ je5
ds =~ ds1 + ds2 + ds3 + ds4 + ds5
str =~ st1r + st2r + st3r + st4r + st5r
sc.psy =~ sc1 +  sc2
sc.gos =~ sc3 + sc4 + sc5
ts =~ ts1 + ts2 + ts3 + ts4 + ts5
je ~~ ds + str + sc.psy + sc.gos + ts
ds ~~ str + sc.psy + sc.gos + ts
str ~~ sc.psy + sc.gos + ts
sc.psy ~~ sc.gos + ts
sc.gos ~~ ts"
f1.7 <- cfa(model = m1.7, data = ds, meanstructure = F, fixed.x = F,
            estimator = "ML", likelihood = "wishart",)
summary(f1.7, fit.measures = T, standardized = T)
lavTestLRT(f1.5, f1.7, method = "satorra.bentler.2010", type = "Chisq")
modificationIndices(f1.7)
# je =~    ds1
# ts2 ~~    ts4

### correlated factor cfa - splitting up sc & je error r & ds1 cross-loading
m1.8 <- "
je =~ je1 + je2 + je3 + je4 + je5
je4 ~~ je5
ds =~ ds1 + ds2 + ds3 + ds4 + ds5
je =~ ds1
str =~ st1r + st2r + st3r + st4r + st5r
sc.psy =~ sc1 +  sc2
sc.gos =~ sc3 + sc4 + sc5
ts =~ ts1 + ts2 + ts3 + ts4 + ts5
je ~~ ds + str + sc.psy + sc.gos + ts
ds ~~ str + sc.psy + sc.gos + ts
str ~~ sc.psy + sc.gos + ts
sc.psy ~~ sc.gos + ts
sc.gos ~~ ts"
f1.8 <- cfa(model = m1.8, data = ds, meanstructure = F, fixed.x = F,
            estimator = "ML", likelihood = "wishart",)
summary(f1.8, fit.measures = T, standardized = T)
lavTestLRT(f1.7, f1.8, method = "satorra.bentler.2010", type = "Chisq")
modificationIndices(f1.8)
# str =~ ds2 
# ts2 ~~ ts4

### correlated factor cfa - splitting up sc & je error r & ds1 cross-loading & ts error r
m1.9 <- "
je =~ je1 + je2 + je3 + je4 + je5
je4 ~~ je5
ds =~ ds1 + ds2 + ds3 + ds4 + ds5
je =~ ds1
str =~ st1r + st2r + st3r + st4r + st5r
sc.psy =~ sc1 +  sc2
sc.gos =~ sc3 + sc4 + sc5
ts =~ ts1 + ts2 + ts3 + ts4 + ts5
ts2 ~~ ts4
je ~~ ds + str + sc.psy + sc.gos + ts
ds ~~ str + sc.psy + sc.gos + ts
str ~~ sc.psy + sc.gos + ts
sc.psy ~~ sc.gos + ts
sc.gos ~~ ts"
f1.9 <- cfa(model = m1.9, data = ds, meanstructure = F, fixed.x = F,
            estimator = "ML", likelihood = "wishart",)
summary(f1.9, fit.measures = T, standardized = T)
lavTestLRT(f1.8, f1.9, method = "satorra.bentler.2010", type = "Chisq")
modificationIndices(f1.9)
# str =~  ds2

### correlated factor cfa - splitting up sc & je error r & ds1 cross-loading & ts error r & ds2 cross-loading
m1.95 <- "
je =~ je1 + je2 + je3 + je4 + je5
je4 ~~ je5
ds =~ ds1 + ds2 + ds3 + ds4 + ds5
je =~ ds1
str =~ st1r + st2r + st3r + st4r + st5r
str =~ ds2
sc.psy =~ sc1 +  sc2
sc.gos =~ sc3 + sc4 + sc5
ts =~ ts1 + ts2 + ts3 + ts4 + ts5
ts2 ~~ ts4
je ~~ ds + str + sc.psy + sc.gos + ts
ds ~~ str + sc.psy + sc.gos + ts
str ~~ sc.psy + sc.gos + ts
sc.psy ~~ sc.gos + ts
sc.gos ~~ ts"
f1.95 <- cfa(model = m1.95, data = ds, meanstructure = F, fixed.x = F,
            estimator = "ML", likelihood = "wishart",)
summary(f1.95, fit.measures = T, standardized = T)
lavTestLRT(f1.9, f1.95, method = "satorra.bentler.2010", type = "Chisq")
modificationIndices(f1.95)

### second-order factor cfa
m2 <- "
je =~ je1 + je2 + je3 + je4 + je5
ds =~ ds1 + ds2 + ds3 + ds4 + ds5
str =~ st1r + st2r + st3r + st4r + st5r
sc =~ sc1 +  sc2 + sc3 + sc4 + sc5
ts =~ ts1 + ts2 + ts3 + ts4 + ts5
cur =~ je + ds + str + sc + ts"
f2 <- cfa(model = m2, data = ds, meanstructure = F, fixed.x = F,
         estimator = "ML", likelihood = "wishart",)
summary(f2, fit.measures = T, standardized = T)
lavTestLRT(f1, f2, method = "satorra.bentler.2010", type = "Chisq")

### bifactor cfa
m3 <- "
cur =~ je1 + je2 + je3 + je4 + je5 +
ds1 + ds2 + ds3 + ds4 + ds5 + 
st1r + st2r + st3r + st4r + st5r + 
sc1 +  sc2 + sc3 + sc4 + sc5 +
ts1 + ts2 + ts3 + ts4 + ts5
je =~ je1 + je2 + je3 + je4 + je5
ds =~ ds1 + ds2 + ds3 + ds4 + ds5
str =~ st1r + st2r + st3r + st4r + st5r
sc =~ sc1 +  sc2 + sc3 + sc4 + sc5
ts =~ ts1 + ts2 + ts3 + ts4 + ts5
cur ~~ 0*je + 0*ds + 0*str + 0*sc + 0*ts
je ~~ 0*ds + 0*str + 0*sc + 0*ts
ds ~~ 0*str + 0*sc + 0*ts
str ~~ 0*sc + 0*ts
sc ~~ 0*ts"
f3 <- cfa(model = m3, data = ds, meanstructure = F, fixed.x = F,
          estimator = "ML", likelihood = "wishart",)
summary(f3, fit.measures = T, standardized = T)
lavTestLRT(f1, f3, method = "satorra.bentler.2010", type = "Chisq")
lavTestLRT(f2, f3, method = "satorra.bentler.2010", type = "Chisq")

### bifactor cfa w/o je specific factor
m3.1 <- "
cur =~ je1 + je2 + je3 + je4 + je5 +
ds1 + ds2 + ds3 + ds4 + ds5 + 
st1r + st2r + st3r + st4r + st5r + 
sc1 +  sc2 + sc3 + sc4 + sc5 +
ts1 + ts2 + ts3 + ts4 + ts5
# je =~ je1 + je2 + je3 + je4 + je5
ds =~ ds1 + ds2 + ds3 + ds4 + ds5
str =~ st1r + st2r + st3r + st4r + st5r
sc =~ sc1 +  sc2 + sc3 + sc4 + sc5
ts =~ ts1 + ts2 + ts3 + ts4 + ts5
cur ~~ 0*ds + 0*str + 0*sc + 0*ts
ds ~~ 0*str + 0*sc + 0*ts
str ~~ 0*sc + 0*ts
sc ~~ 0*ts"
f3.1 <- cfa(model = m3.1, data = ds, meanstructure = F, fixed.x = F,
          estimator = "ML", likelihood = "wishart",)
summary(f3.1, fit.measures = T, standardized = T)
lavTestLRT(f1, f3.1, method = "satorra.bentler.2010", type = "Chisq")
lavTestLRT(f3, f3.1, method = "satorra.bentler.2010", type = "Chisq")

### bifactor cfa w/o je specific factor + w/o st general loadings
m3.2 <- "
cur =~ je1 + je2 + je3 + je4 + je5 +
ds1 + ds2 + ds3 + ds4 + ds5 + 
# st1r + st2r + st3r + st4r + st5r + 
sc1 +  sc2 + sc3 + sc4 + sc5 +
ts1 + ts2 + ts3 + ts4 + ts5
# je =~ je1 + je2 + je3 + je4 + je5
ds =~ ds1 + ds2 + ds3 + ds4 + ds5
str =~ st1r + st2r + st3r + st4r + st5r
sc =~ sc1 +  sc2 + sc3 + sc4 + sc5
ts =~ ts1 + ts2 + ts3 + ts4 + ts5
cur ~~ 0*ds + 0*str + 0*sc + 0*ts
ds ~~ 0*str + 0*sc + 0*ts
str ~~ 0*sc + 0*ts
sc ~~ 0*ts"
f3.2 <- cfa(model = m3.2, data = ds, meanstructure = F, fixed.x = F,
          estimator = "ML", likelihood = "wishart",)
summary(f3.2, fit.measures = T, standardized = T)
lavTestLRT(f1, f3.2, method = "satorra.bentler.2010", type = "Chisq")
lavTestLRT(f3.1, f3.2, method = "satorra.bentler.2010", type = "Chisq")
modificationIndices(f3.2)
# ds ~~  str
# str ~~  sc
# str =~  ds2 (same mod as before)
# je4 ~~  je5 (same mod as before)
# sc1 ~~  sc2 (same mod as before)
