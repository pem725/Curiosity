# Playground for Curiosity

# Install and load the lavaan package if not already installed
if (!requireNamespace("lavaan", quietly = TRUE)) {
  install.packages("lavaan")
}

# Install and load the tidyverse package if not already installed
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

# Load the tidyverse package
library(tidyverse)
library(lavaan)

## PEM - I'm just going to code here.  When shit works, I will port it over to
## the Quarto file.  That file is driving me nuts.

df <- read.csv("Merck_Curiosity_Survey_Data_NO_PWD.csv", header = TRUE)

### Varnames
# read the variable names
df.varnames <- read.csv("Merck_Curiosity_Survey_Data_NO_PWD_varnames.csv", header = TRUE)
# print df.varnames with row.names
names(df.varnames) <- c("shortName","longName")
factor.names <- data.frame(shortName = c("ds","je","st","op"), 
                           longName = c("Deprivation Sensitivity", 
                                        "Joyous Exploration", 
                                        "Stress Tolerance", 
                                        "Openness to People's Ideas"))
df.varnames <- rbind(df.varnames,factor.names)

## select only Locale levels with sufficient data N > 200
df.fin <- df %>%
  group_by(Locale) %>%
  filter(n() > 200) %>%
  ungroup()

table(df.fin$Locale.f)




### Locale
Locale.dat <- data.frame(group=1:10, location = unique(df.fin$Locale))
df.fin$Locale.f <- as.factor(df.fin$Locale)
df.fin$Locale.f


library(lavaan)

Cur.model <- 'ds =~ Q1 + Q2 + Q3 + Q4
              je =~ Q5 + Q6 + Q7 + Q8
              st =~ Q9 + Q10 + Q11 + Q12
              op =~ Q13 + Q14 + Q15 + Q16'


fit.configural <- cfa(Cur.model,
                      data=df.fin,
                      group="Locale")

str(fit.configural)
summary(fit.configural)



fit.weak <- cfa(Cur.model,
                data=df.fin,
                group="Locale",
                group.equal = c("loadings"))

summary(fit.weak)
fitmeasures(fit.weak)


fit.strong <- cfa(Cur.model,
                  data=df.fin,
                  group="Locale",
                  group.equal = c("intercepts","loadings"))

lavTestLRT(fit.configural, fit.weak, fit.strong)

byLocal.cfa <- as.data.frame(round(cbind(fitmeasures(fit.configural),fitmeasures(fit.weak),fitmeasures(fit.strong)),3))
names(byLocal.cfa) <- c("configural","weak","strong")
byLocal.cfa$Paramater <- row.names(byLocal.cfa)
byLocal.cfa <- byLocal.cfa[,c(4,1:3)]
byLocal.cfa

## The weak model wins the day!  RMSEA and BIC are best for fit.  Now, let's see the model.

summary(fit.weak, fit.measures=TRUE)
summary(fit.weak, fit.measures=F)

inspect(fit.configural, "mean.lv", digits = 3)
inspect(fit.configural, "std.lv", digits = 3)

inspect(fit.configural, "est", digits = 3)

res.ModInds <- inspect(fit.weak, "modindices", digits = 3)
summary(res.ModInds)

# select the top 5 largest modification indices by group in tidyverse

library(tidyverse)
topMIs <- res.ModInds %>% 
  as_tibble() %>% 
  group_by(group) %>% 
  top_n(5, abs(mi)) %>% 
  arrange(group, desc(abs(mi))) %>%
  left_join(Locale.dat, by = "group") %>%
  left_join(df.varnames, by = c("lhs" = "shortName"))

summary(topMIs)
topMIs
write.csv(topMIs, file = "topMIs.csv", row.names = FALSE)

str(topMIs)

# exploratory factory analysis by group df.fin[,2:17]
efa1 <- df.fin[,2:17] %>% 
  psych::fa(nfactors = 4, rotate = "varimax", scores = "regression", fm = "minres")

summary(efa1)
efa1$loadings


fa.diagram(efa1, simple=F, digits = 2, cut = 0.3, sort=T, main = "EFA (PAF) All Groups Combined")

efa2 <- psych::omega(df.fin[complete.cases(df.fin[,2:17]),2:17], nfactors = 4, poly=T, scores = "regression")

df.itemsOnly <- df.fin[complete.cases(df.fin[,2:17]),2:17]

cp <- ggcorrplot(cor(df.itemsOnly), 
           hc.order = FALSE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method = "circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title = "Correlation Matrix of Items", 
           ggtheme = ggplot2::theme_gray)
cp
str(cp)
sldat <- data.frame(x=0:10, y=0:10)
cp + geom_line(sldat, aes(x, y), color = "black", linewidth = 2)


fit.conf.age <- cfa(Cur.model,
                    data=df.fin,
                    group="Age.Group")

fit.weak.age <- cfa(Cur.model,
                    data=df.fin,
                    group="Age.Group",
                    group.equal = c("loadings"))

fit.strong.age <- cfa(Cur.model,
                    data=df.fin,
                    group="Age.Group",
                    group.equal = c("loadings","intercepts"))

lavTestLRT(fit.conf.age, fit.weak.age, fit.strong.age)
byAge.cfa <- as.data.frame(round(cbind(fitmeasures(fit.conf.age),fitmeasures(fit.weak.age),fitmeasures(fit.strong.age)),3))
names(byAge.cfa) <- c("configural","weak","strong")
byAge.cfa$Paramater <- row.names(byAge.cfa)
byAge.cfa <- byAge.cfa[,c(4,1:3)]
byAge.cfa


## write all of these tables to the google sheet

library(googlesheets4)

write_sheet(byLocal.cfa,
            ss="https://docs.google.com/spreadsheets/d/1qHLtummehmJeh4Fy9vicI6EIvyNeLPzXjBOiPc6xd0c/edit#gid=1579530372",
            sheet="byLocal.cfa")



write_sheet(byAge.cfa,
            ss="https://docs.google.com/spreadsheets/d/1qHLtummehmJeh4Fy9vicI6EIvyNeLPzXjBOiPc6xd0c/edit#gid=1579530372",
            sheet="byAge.cfa")

## show figures of the two models for comparison

## local model
library(lavaanPlot)
lavaanPlot2(fit.weak)


## New Day, New Model

New.model <- 'ds =~ Q1 + Q3 + Q4
              je =~ Q6 + Q7 + Q8
              st =~ Q10 + Q11 + Q12
              op =~ Q13 + Q14 + Q15'

fit.configural.new <- cfa(New.model,
                      data=df.fin,
                      group="Locale")

str(fit.configural.new)
fitmeasures(fit.configural.new)
summary(fit.configural.new)


fit.weak.new <- cfa(New.model,
                data=df.fin,
                group="Locale",
                group.equal = c("loadings"))

summary(fit.weak.new)
fitmeasures(fit.weak.new)
summary(fit.weak.new, fit.measures=TRUE)
summary(fit.weak.new, fit.measures=F)

lavTestScore(fit.configural.new, fit.weak.new)


inspect(fit.configural.new, "mean.lv", digits = 3)
inspect(fit.configural.new, "std.lv", digits = 3)

inspect(fit.configural.new, "est", digits = 3)

res.ModInds.new <- inspect(fit.weak.new, "modindices", digits = 3)
summary(res.ModInds.new)

# select the top 5 largest modification indices by group in tidyverse

library(tidyverse)
topMIs.new <- res.ModInds.new %>% 
  as_tibble() %>% 
  group_by(group) %>% 
  top_n(5, abs(mi)) %>% 
  arrange(group, desc(abs(mi))) %>%
  left_join(Locale.dat, by = "group") %>%
  left_join(df.varnames, by = c("lhs" = "shortName"))

summary(topMIs.new)
topMIs.new
write.csv(topMIs.new, file = "topMIs.new.csv", row.names = FALSE)

str(topMIs.new)

## EFA models for NEW model

new.vars <- c("Q1","Q3","Q4","Q6","Q7","Q8","Q10","Q11","Q12","Q13","Q14","Q15")

efa1.new <- df.fin[,new.vars] %>% 
  psych::fa(nfactors = 4, rotate = "varimax", scores = "regression", fm = "minres")

summary(efa1.new)
efa1.new$loadings

efa2.new <- psych::omega(df.fin[complete.cases(df.fin[,new.vars]),new.vars], nfactors = 4, poly=T, scores = "regression")
summary(efa2.new)


fa.diagram(efa1, simple=F, digits = 2, cut = 0.3, sort=T, main = "EFA (PAF) All Groups Combined")

anova(efa2, efa2.new)
summary(efa2)
summary(efa2.new)

efa2
