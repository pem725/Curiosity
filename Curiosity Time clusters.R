### NEW CURIOSITY MEASURE (5D-Curiosity) Cleaning ###############################
### 3000 people representative sample #######################################

### notes
# sampling weights are caled "weight" - use them for the CFA
# reverse score stress tolerance items so they actually reflect greater stress tolerance

### libraries
library(foreign)
library(psych)
library(paran)
library(car)
library(multicon)

### input data
seg <- read.spss(file = "Time3000 data with 25item clusters.sav", use.value.labels = F,
               to.data.frame = T, use.missings = T)
names(seg[,c(1:3)]); names(seg[,c(1160:1163)]); nrow(seg)

### Clusters
table(seg$segment_25statements)
  # 1 = The Fascinated
  # 2 = The Problem Solvers
  # 3 = The Empathizers
  # 4 = The Avoiders

### identify outcome variables

# 1. demographics - aspects of people's identity (various responses)

# cleaning
dem <- seg[,c(1,2:28,1154:1157)]; names(dem)
names(dem) <- c("id","Gender","Age","State","Job_Advertising","Job_Communication",
                "Job_tv_radio","Job_finance","Job_mags_news","Job_accounting",
                "Job_healthcare","Job_other","Parent_House","Married","Employ",
                "Education","Income","Latino_mexican","Latino_Puerto","Latino_cuban","Latino_America",
                "Latino_other","Latino_none","White","Black","Asian","Native_American",
                "Race_other","Politics_philo","Politics_party","President_2016","Age_exact"); names(dem)
demb <- dem
sum(is.na(demb[,-c(1)]))/31/nrow(demb[,-c(1)])
head(demb$id, n = 10)
summary(demb$Gender); summary(demb$Age_exact)

# Descriptive statistics
table(demb$Gender, exclude = NULL)
table(demb$Age_exact, exclude = NULL)
table(demb$White, exclude = NULL)
table(demb$Education, exclude = NULL)
table(demb$Income, exclude = NULL)
table(demb$Employ, exclude = NULL)
table(demb$Politics_party, exclude = NULL)
table(demb$Married, exclude = NULL)
table(demb$Parent_House, exclude = NULL)

# Recode demographics
demb$male <- Recode(demb$Gender, recodes = "2=0", as.numeric.result = T); table(demb$male, exclude = NULL)
demb$edu <- demb$Education - 1; table(demb$edu, exclude = NULL)
demb$annual_income <- Recode(demb$Income, recodes = "1=10000;
                      2=20000; 3=30000; 4=40000; 5=50000;
                      6=60000; 7=70000; 8=85000; 9=125000;
                      10=175000; 11=225000; 12=500000", as.numeric.result = T); table(demb$annual_income, exclude = NULL)
demb$job <- Recode(demb$Employ, recodes = "2:8=0", as.numeric.result = T); table(demb$job, exclude = NULL)
demb$politics <- Recode(demb$Politics_party, recodes = "1:2=0; 3:4=1", as.numeric.result = T); table(demb$politics, exclude = NULL)
demb$marital_status <- Recode(demb$Married, recodes = "1=0; 2=1; 3:5=0", as.numeric.result = T); table(demb$marital_status, exclude = NULL)
demb$parent <- Recode(demb$Parent_House, recodes = "2=0", as.numeric.result = T); table(demb$parent, exclude = NULL)

# variable check
names(demb)

## 2. self-identity - words to describe themselves (0-1 binary)

# cleaning
self <- seg[,c(1,38:96)]; names(self)
names(self) <- c("id","Active","Adventurous","Agreeable","Appreciative","Aspiring","Assertive",
                 "Balanced","Brilliant","Calm","Complex","Confident","Creative","Cultured", "Curious",
                 "Deep","Detail_oriented","Driven","Easy-going","Empathetic","Enthusiastic","Extroverted",
                 "Focused","Friendly","Fun_loving","Funny","Generous","Happy","Hardworking",
                 "Healthy","Idealistic","Influential","Innovative","Intelligent","Introverted",
                 "Leader","Objective","Open_minded","Opinionated","Organized","Outspoken",
                 "Passionate","Patient","Persistent","Sensative","Social","Witty","Other",
                 "Anxious","Apathetic","Careless","Conceited","Cynical","Disorganized","Dramatic",
                 "Impatient","Impulsive","Lazy","Neurotic","Obsessive"); names(self)
selfb <- subset(x = self, select = -c(Brilliant,Intelligent,Curious,Healthy,Objective,
                                      Witty,Happy,Influential,Other)); names(selfb)
sum(is.na(selfb[,-c(1)]))/50/nrow(selfb[,-c(1)])
head(selfb$id, n = 10)
summary(selfb$Active); summary(selfb$Obsessive)

# PCA
pc <- principal(r = selfb[,-c(1)], nfactors = 50, residuals = T, rotate = "varimax",
                scores = F, missing = F); print(pc)
plot(pc$values)
pr <- paran(x = selfb[,-c(1)], status = T, all = T, 
            cfa = F, graph = T, color = T, seed = 101)
pr <- paran(x = selfb[,-c(1)], centile = 95, status = T, all = T, 
            cfa = F, graph = T, color = T, seed = 101)
pc <- principal(r = selfb[,-c(1)], nfactors = 5, residuals = T, rotate = "varimax",
                scores = F, missing = F); print(pc)
pc <- principal(r = selfb[,-c(1)], nfactors = 6, residuals = T, rotate = "varimax",
                scores = F, missing = F); print(pc)
pc <- principal(r = selfb[,-c(1)], nfactors = 7, residuals = T, rotate = "varimax",
                scores = F, missing = F); print(pc)

# Total scores

# Agreeable
selfb$agree <- 100*composite(set = selfb[,c("Agreeable","Appreciative","Calm","Easy-going","Empathetic",
                                        "Friendly","Generous","Patient","Sensative")], nomiss = .75,
                                            rel = T)
sum(is.na(selfb$agree)); mean(selfb$agree); range(selfb$agree)
hist(selfb$agree, n =50)

# Extroverted
selfb$Introverted_R <- Recode(var = selfb$Introverted, recodes = "0=1; 1=0", as.numeric.result = T)
selfb$extro <- 100*composite(set = selfb[,c("Active","Adventurous","Enthusiastic","Extroverted","Introverted_R",
                                            "Leader","Social")], nomiss = .75,
                                            rel = T)
sum(is.na(selfb$extro)); mean(selfb$extro); range(selfb$extro)
hist(selfb$extro, n =50)

# Openness
selfb$open <- 100*composite(set = selfb[,c("Aspiring","Complex","Creative","Cultured","Deep",
                                            "Idealistic","Innovative")], nomiss = .75,
                             rel = T)
sum(is.na(selfb$open)); mean(selfb$open); range(selfb$open)
hist(selfb$open, n =50)

# Conscientiousness
selfb$Disorganized_R <- Recode(var = selfb$Disorganized, recodes = "0=1; 1=0", as.numeric.result = T)
selfb$consc <- 100*composite(set = selfb[,c("Detail_oriented","Driven","Focused","Hardworking","Organized",
                                           "Disorganized_R")], nomiss = .75,
                            rel = T)
sum(is.na(selfb$consc)); mean(selfb$consc); range(selfb$consc)
hist(selfb$consc, n =50)

# Outspoken
selfb$outsp <- 100*composite(set = selfb[,c("Opinionated","Outspoken","Impatient")], nomiss = .75,
                             rel = T)
sum(is.na(selfb$outsp)); mean(selfb$outsp); range(selfb$outsp)
hist(selfb$outsp, n =50)

# Neurotic
selfb$neur <- 100*composite(set = selfb[,c("Anxious","Dramatic","Neurotic","Obsessive")], nomiss = .75,
                             rel = T)
sum(is.na(selfb$neur)); mean(selfb$neur); range(selfb$neur)
hist(selfb$neur, n =50)

# Apathetic
selfb$apath <- 100*composite(set = selfb[,c("Apathetic","Careless","Conceited","Lazy")], nomiss = .75,
                             rel = T)
sum(is.na(selfb$apath)); mean(selfb$apath); range(selfb$apath)
hist(selfb$apath, n =50)

# check variables
names(selfb)

# 3. social media - people's use of various social media websites (various responses)

# cleaning
sm <- seg[,c(1,153:187)]; names(sm)
names(sm) <- c("id","Stream_music","Mobile_apps","Read_mags","Play_video_games","Watch_smartphone",
               "social_media","Stream_online","Desktop_comp","Laptop","Smartphone","Tablet",
               "Facebook","Foursquare","Google_plus","Instagram","Kik_messenger","Line",
               "LinkedIn","Periscope","Pinterest","Reddit","Snapchat","Tinder","Tumblr",
               "Twitter","Vine","WhatsApp","YikYak","YouTube","Other1","Other2","Other3",
               "online_friends","online_followers","liked_organ"); names(sm)
smb <- subset(x = sm, select = -c(Stream_music,Mobile_apps,Read_mags,Play_video_games,Watch_smartphone,
                                  Stream_online,Desktop_comp,Laptop,Smartphone,Tablet,
                                  Other1,Other2,Other3)); names(smb)
sum(is.na(smb[,-c(1)]))/22/nrow(smb[,-c(1)])
head(smb$id, n = 10)
summary(smb$social_media); summary(smb$YouTube); summary(smb$liked_org)

# Descriptives
cor(smb$online_friends, smb$online_followers, use = "complete.obs")
mean(smb$online_friends, na.rm = T); range(smb$online_friends, na.rm = T); hist(smb$online_friends, n = 50)
mean(smb$online_followers, na.rm = T); range(smb$online_followers, na.rm = T); hist(smb$online_followers, n = 50)
mean(smb$Facebook, na.rm = T); range(smb$Facebook, na.rm = T)
mean(smb$Twitter, na.rm = T); range(smb$Twitter, na.rm = T)
mean(smb$Instagram, na.rm = T); range(smb$Instagram, na.rm = T)
mean(smb$Snapchat, na.rm = T); range(smb$Snapchat, na.rm = T)
mean(smb$Tumblr, na.rm = T); range(smb$Tumblr, na.rm = T)
# mean(smb$YikYak, na.rm = T); range(smb$YikYak, na.rm = T)
# mean(smb$Kik_messenger, na.rm = T); range(smb$Kik_messenger, na.rm = T)
mean(smb$Pinterest, na.rm = T); range(smb$Pinterest, na.rm = T)
mean(smb$Reddit, na.rm = T); range(smb$Reddit, na.rm = T)
mean(smb$YouTube, na.rm = T); range(smb$YouTube, na.rm = T)
# mean(smb$Foursquare, na.rm = T); range(smb$Foursquare, na.rm = T)
# mean(smb$Tinder, na.rm = T); range(smb$Tinder, na.rm = T)
mean(smb$Google_plus, na.rm = T); range(smb$Google_plus, na.rm = T)
mean(smb$LinkedIn, na.rm = T); range(smb$LinkedIn, na.rm = T)

# Total scores
smb$online_friends_W <- Recode(var = smb$online_friends, recodes = "2501:9999=2500", as.numeric.result = T)
hist(smb$online_friends_W, n = 50)
smb$online_followers_W <- Recode(var = smb$online_followers, recodes = "2501:9999=2500", as.numeric.result = T)
hist(smb$online_followers_W, n = 50)
smb$online_total_W <- 2*composite(set = smb[,c("online_friends_W","online_followers_W")], nomiss = 1.0, rel = T)
hist(smb$online_total_W, n = 50); skew(smb$online_total_W)

# check variables
names(smb)

# 4. expertise - topics other people ask you advice about (0-1 binary)

# cleaning
exp <- seg[,c(1,188:219)]; names(exp)
names(exp) <- c("id","Automobiles","Beauty","Alcohol","Books_mags","Celebrity","Cooking",
               "Education","Green_products","Fashion","Finance","Fishing","Gardening",
               "Healthcare","Health_fitness","Houses","Decorating","Hunting","Movies_tv",
               "Music","Technology","Cleaning_org","Parenting","Pets","Photography","Politics",
               "Real_estate","Religion","Sports","My_ethnicity","Travel","Video_games","None"); names(exp)
expb <- subset(x = exp, select = -c(None)); names(expb)
sum(is.na(expb[,-c(1)]))/31/nrow(expb[,-c(1)])
head(expb$id, n = 10)
summary(expb$Automobiles); summary(expb$Video_games)

# PCA
pc <- principal(r = expb[,-c(1)], nfactors = 31, residuals = T, rotate = "varimax",
                scores = F, missing = F); print(pc)
plot(pc$values)
pr <- paran(x = expb[,-c(1)], status = T, all = T, 
            cfa = F, graph = T, color = T, seed = 101)
pr <- paran(x = expb[,-c(1)], centile = 95, status = T, all = T, 
            cfa = F, graph = T, color = T, seed = 101)
pc <- principal(r = expb[,-c(1)], nfactors = 2, residuals = T, rotate = "varimax",
                scores = F, missing = F); print(pc)
pc <- principal(r = expb[,-c(1)], nfactors = 3, residuals = T, rotate = "varimax",
                scores = F, missing = F); print(pc)
pc <- principal(r = expb[,-c(1)], nfactors = 4, residuals = T, rotate = "varimax",
                scores = F, missing = F); print(pc)
pc <- principal(r = expb[,-c(1)], nfactors = 7, residuals = T, rotate = "varimax",
                scores = F, missing = F); print(pc)
pc <- principal(r = expb[,-c(1)], nfactors = 8, residuals = T, rotate = "varimax",
                scores = F, missing = F); print(pc)                
pc <- principal(r = expb[,-c(1)], nfactors = 16, residuals = T, rotate = "varimax",
                scores = F, missing = F); print(pc)                

# Descriptives
corr.test(expb[,c("Beauty","Celebrity","Fashion","Houses","Decorating")])
corr.test(expb[,c("Music","Celebrity","Movies_tv")])
corr.test(expb[,c("Fishing","Hunting","Automobiles","Sports")])
corr.test(expb[,c("Technology","Video_games")])
corr.test(expb[,c("Politics","Finance","Religion")])

# Total Scores
expb$expert_total <- 100*composite(set = expb[,c("Automobiles","Beauty","Alcohol","Books_mags","Celebrity","Cooking",
                                                 "Education","Green_products","Fashion","Finance","Fishing","Gardening",
                                                 "Healthcare","Health_fitness","Houses","Decorating","Hunting","Movies_tv",
                                                 "Music","Technology","Cleaning_org","Parenting","Pets","Photography","Politics",
                                                 "Real_estate","Religion","Sports","My_ethnicity","Travel","Video_games")], nomiss = .75, 
                                   rel = T)
sum(is.na(expb$expert_total)); hist(expb$expert_total, n = 50)

# check variables
names(expb)

# 5. passions - topics people are passionate about (0-1 binary)

# cleaning
pas <- seg[,c(1,220:251)]; names(pas)
names(pas) <- c("id","Automobiles","Beauty","Alcohol","Books_mags","Celebrity","Cooking",
                "Education","Green_products","Fashion","Finance","Fishing","Gardening",
                "Healthcare","Health_fitness","Houses","Decorating","Hunting","Movies_tv",
                "Music","Technology","Cleaning_org","Parenting","Pets","Photography","Politics",
                "Real_estate","Religion","Sports","My_ethnicity","Travel","Video_games","None"); names(pas)
pasb <- subset(x = pas, select = -c(None)); names(pasb)
sum(is.na(pasb[,-c(1)]))/31/nrow(pasb[,-c(1)])
head(pas$id, n = 10)
summary(pasb$Automobiles); summary(pasb$Video_games)

# PCA
pc <- principal(r = pasb[,-c(1)], nfactors = 31, residuals = F, rotate = "varimax",
                scores = F, missing = F); print(pc)
plot(pc$values)
pr <- paran(x = pasb[,-c(1)], status = T, all = T, 
            cfa = F, graph = T, color = T, seed = 101)
pr <- paran(x = pasb[,-c(1)], centile = 95, status = T, all = T, 
            cfa = F, graph = T, color = T, seed = 101)
pc <- principal(r = pasb[,-c(1)], nfactors = 2, residuals = F, rotate = "varimax",
                scores = F, missing = F); print(pc)
pc <- principal(r = pasb[,-c(1)], nfactors = 4, residuals = F, rotate = "varimax",
                scores = F, missing = F); print(pc)
pc <- principal(r = pasb[,-c(1)], nfactors = 8, residuals = F, rotate = "varimax",
                scores = F, missing = F); print(pc)
pc <- principal(r = pasb[,-c(1)], nfactors = 16, residuals = F, rotate = "varimax",
                scores = F, missing = F); print(pc)

# Descriptives
corr.test(pasb[,c("Beauty","Celebrity","Fashion","Houses","Decorating")])
corr.test(pasb[,c("Music","Celebrity","Movies_tv")])
corr.test(pasb[,c("Fishing","Hunting","Automobiles","Sports")])
corr.test(pasb[,c("Technology","Video_games")])
corr.test(pasb[,c("Politics","Finance","Religion")])

# Total Scores
pasb$passion_total <- 100*composite(set = pasb[,c("Automobiles","Beauty","Alcohol","Books_mags","Celebrity","Cooking",
                                                 "Education","Green_products","Fashion","Finance","Fishing","Gardening",
                                                 "Healthcare","Health_fitness","Houses","Decorating","Hunting","Movies_tv",
                                                 "Music","Technology","Cleaning_org","Parenting","Pets","Photography","Politics",
                                                 "Real_estate","Religion","Sports","My_ethnicity","Travel","Video_games")], nomiss = .75, 
                                   rel = T)
sum(is.na(pasb$passion_total)); hist(pasb$passion_total, n = 50)

# variable check
names(pasb)


# # 6. interests - words for things people seek more info about (1-9 likert)
# 
# # cleaning
# int <- seg[,c(686:716)]; names(int)
# names(int) <- c("Automobiles","Beauty","Alcohol","Books_mags","Celebrity","Cooking",
#                 "Education","Green_products","Fashion","Finance","Fishing","Gardening",
#                 "Healthcare","Health_fitness","Houses","Decorating","Hunting","Movies_tv",
#                 "Music","Technology","Cleaning_org","Parenting","Pets","Photography","Politics",
#                 "Real_estate","Religion","Sports","My_ethnicity","Travel","Video_games"); names(int)
# intb <- int; names(intb)
# sum(is.na(intb))/31/nrow(intb)
# summary(intb$Automobiles); summary(intb$Video_games)
# 
# # PCA
# pc <- principal(r = selfb, nfactors = 50, residuals = T, rotate = "varimax",
#                 scores = F, missing = F, ); print(pc)
# plot(pc$values)
# pr <- paran(x = selfb, status = T, all = T, 
#             cfa = F, graph = T, color = T, seed = 101)
# pr <- paran(x = selfb, centile = 95, status = T, all = T, 
#             cfa = F, graph = T, color = T, seed = 101)
# pc <- principal(r = selfb, nfactors = 5, residuals = T, rotate = "varimax",
#                 scores = F, missing = F, ); print(pc)
# pc <- principal(r = selfb, nfactors = 6, residuals = T, rotate = "varimax",
#                 scores = F, missing = F, ); print(pc)
# pc <- principal(r = selfb, nfactors = 7, residuals = T, rotate = "varimax",
#                 scores = F, missing = F, ); print(pc)

# 7. read - magazines looked at by people in the past 6 months (0-1 binary)

# cleaning
read <- seg[,c(1,758:814)]; names(read)
names(read) <- c("id","Afar","Businessweek","Bon_appetit","Coastal_living","Traveler_condo","Cooking_light",
                 "Cosmopolitan","Country_living","Departures","Eating_well","Ebony","Elle",
                 "Entertainment_weekly","ESPN_mag","Essence","Food_wine","Food_network_mag",
                 "Forbes","Fortune","Glamour","Golf","Golf_digest","Harpers_bazaar","Health",
                 "House_beautiful","InTouch","InStyle","Kiplinger_finance","Latina","Martha_stewart",
                 "Mens_health","Money","NYtimes","Oprah","People","People_espanol","Real_simple",
                 "Robb_report","Rolling_stone","Saveur","Shape","Southern_living","Sports_illustrated",
                 "Style_watch","Sunset","Time","Traditional_home","Travel_leisure","USweekly",
                 "Vanidades","Vanity_fair","Wallstreet","Womens_health","Other1","Other2","Other3","None"); names(read)
readb <- subset(x = read, select = -c(Other1,Other2,Other3,None)); names(readb)
sum(is.na(readb[,-c(1)]))/53/nrow(readb[,-c(1)])
head(readb$id, n = 10)
summary(readb$Afar); summary(readb$Womens_health)

# PCA
pc <- principal(r = readb[,-c(1)], nfactors = 53, residuals = T, rotate = "varimax",
                scores = F, missing = F); print(pc)
plot(pc$values)
pr <- paran(x = readb[,-c(1)], status = T, all = T, 
            cfa = F, graph = T, color = T, seed = 101)
pr <- paran(x = readb[,-c(1)], centile = 95, status = T, all = T, 
            cfa = F, graph = T, color = T, seed = 101)
pc <- principal(r = readb[,-c(1)], nfactors = 5, residuals = T, rotate = "varimax",
                scores = F, missing = F); print(pc)
pc <- principal(r = readb[,-c(1)], nfactors = 10, residuals = T, rotate = "varimax",
                scores = F, missing = F); print(pc)
pc <- principal(r = readb[,-c(1)], nfactors = 15, residuals = T, rotate = "varimax",
                scores = F, missing = F); print(pc)

# Total Scores

# Travel
readb$travel <- 4*composite(set = readb[,c("Traveler_condo","Afar","Saveur",
                                             "Travel_leisure")], nomiss = .75, 
                              rel = T)
sum(is.na(readb$travel)); table(readb$travel, exclude = NULL)
readb$travel_bin <- Recode(var = readb$travel, recodes = "1:10=1", as.numeric.result = T)
table(readb$travel_bin, exclude = NULL)

# Health
readb$health <- 4*composite(set = readb[,c("Health","Shape","Womens_health",
                                             "Mens_health")], nomiss = .75, 
                            rel = T)
sum(is.na(readb$health)); table(readb$health, exclude = NULL)
readb$health_bin <- Recode(var = readb$health, recodes = "1:10=1", as.numeric.result = T)
table(readb$health_bin, exclude = NULL)

# Home Decorating
readb$home <- 5*composite(set = readb[,c("Coastal_living","Country_living","House_beautiful",
                                           "Southern_living","Traditional_home")], nomiss = .75, 
                              rel = T)
sum(is.na(readb$home)); table(readb$home, exclude = NULL)
readb$home_bin <- Recode(var = readb$home, recodes = "1:10=1", as.numeric.result = T)
table(readb$home_bin, exclude = NULL)

# Sports
readb$sports <- 4*composite(set = readb[,c("ESPN_mag","Golf","Golf_digest",
                                           "Sports_illustrated")], nomiss = .75, 
                            rel = T)
sum(is.na(readb$sports)); table(readb$sports, exclude = NULL)
readb$sports_bin <- Recode(var = readb$sports, recodes = "1:10=1", as.numeric.result = T)
table(readb$sports_bin, exclude = NULL)

# Food
readb$food <- 5*composite(set = readb[,c("Bon_appetit","Cooking_light","Eating_well",
                                           "Food_wine","Food_network_mag")], nomiss = .75, 
                             rel = T)
sum(is.na(readb$food)); table(readb$food, exclude = NULL)
readb$food_bin <- Recode(var = readb$food, recodes = "1:10=1", as.numeric.result = T)
table(readb$food_bin, exclude = NULL)

# Money
readb$money <- 2*composite(set = readb[,c("Kiplinger_finance","Money")], nomiss = .75, 
                                  rel = T)
sum(is.na(readb$money)); table(readb$money, exclude = NULL)
readb$money_bin <- Recode(var = readb$money, recodes = "1:10=1", as.numeric.result = T)
table(readb$money_bin, exclude = NULL)

# Business
readb$business <- 4*composite(set = readb[,c("Businessweek","Forbes","Fortune",
                                                 "Wallstreet")], nomiss = .75, 
                                  rel = T)
sum(is.na(readb$business)); table(readb$business, exclude = NULL)
readb$business_bin <- Recode(var = readb$business, recodes = "1:10=1", as.numeric.result = T)
table(readb$business_bin, exclude = NULL)

# Celebrity
readb$celebrity <- 4*composite(set = readb[,c("Entertainment_weekly","InTouch","People",
                                                 "USweekly")], nomiss = .75, 
                                  rel = T)
sum(is.na(readb$celebrity)); table(readb$celebrity, exclude = NULL)
readb$celebrity_bin <- Recode(var = readb$celebrity, recodes = "1:10=1", as.numeric.result = T)
table(readb$celebrity_bin, exclude = NULL)

# Fashion
readb$fashion <- 6*composite(set = readb[,c("Cosmopolitan","Elle","Glamour",
                                              "Harpers_bazaar","InStyle","Vanity_fair")], nomiss = .75, 
                                    rel = T)
sum(is.na(readb$fashion)); table(readb$fashion, exclude = NULL)
readb$fashion_bin <- Recode(var = readb$fashion, recodes = "1:10=1", as.numeric.result = T)
table(readb$fashion_bin, exclude = NULL)

# Number of Magazine Genres
readb$read_total <- 9*composite(set = readb[,c("travel_bin","health_bin","home_bin","sports_bin",
                                               "food_bin","money_bin","business_bin","celebrity_bin",
                                               "fashion_bin")], nomiss = .75, 
                                rel = T)
sum(is.na(readb$read_total)); mean(readb$read_total, na.rm = T); table(readb$read_total, exclude = NULL)

# check variables
names(readb)

# # 8. subscribe - magazines subscribed to currently (0-1 binary)
# 
# # cleaning
# sub <- seg[,c(815:871)]; names(sub)
# names(sub) <- c("Afar","Businessweek","Bon_appetit","Coastal_living","Traveler_condo","Cooking_light",
#                  "Cosmopolitan","Country_living","Departures","Eating_well","Ebony","Elle",
#                  "Entertainment_weekly","ESPN_mag","Essence","Food_wine","Food_network_mag",
#                  "Forbes","Fortune","Glamour","Golf","Golf_digest","Harpers_bazaar","Health",
#                  "House_beautiful","InTouch","InStyle","Kiplinger_finance","Latina","Martha_stewart",
#                  "Mens_health","Money","NYtimes","Oprah","People","People_espanol","Real_simple",
#                  "Robb_report","Rolling_stone","Saveur","Shape","Southern_living","Sports_illustrated",
#                  "Style_watch","Sunset","Time","Traditional_home","Travel_leisure","USweekly",
#                  "Vanidades","Vanity_fair","Wallstreet","Womens_health","Other1","Other2","Other3","None"); names(sub)
# subb <- subset(x = read, select = -c(Other1,Other2,Other3,None)); names(subb)
# sum(is.na(subb))/53/nrow(subb)
# summary(subb$Afar); summary(subb$Womens_health)
# 
# # PCA
# pc <- principal(r = selfb, nfactors = 50, residuals = T, rotate = "varimax",
#                 scores = F, missing = F, ); print(pc)
# plot(pc$values)
# pr <- paran(x = selfb, status = T, all = T, 
#             cfa = F, graph = T, color = T, seed = 101)
# pr <- paran(x = selfb, centile = 95, status = T, all = T, 
#             cfa = F, graph = T, color = T, seed = 101)
# pc <- principal(r = selfb, nfactors = 5, residuals = T, rotate = "varimax",
#                 scores = F, missing = F, ); print(pc)
# pc <- principal(r = selfb, nfactors = 6, residuals = T, rotate = "varimax",
#                 scores = F, missing = F, ); print(pc)
# pc <- principal(r = selfb, nfactors = 7, residuals = T, rotate = "varimax",
#                 scores = F, missing = F, ); print(pc)

# 9. websites - websites visited in the past 30 days (0-1 binary)

# cleaning
web <- seg[,c(1,872:952)]; names(web)
names(web) <- c("id","Sports120","AllReciples","Amazon","AOL","BET","BleacherReport","BonAppetit",
                "BusinessInsider","Buzzfeed","CNNMoney","CNTraveler","CoastalLiving","CookingLight",
                "Cosmopolitan","CountryLiving","Cozi","Departures","Discovery","Eatingwell","Edmunds",
                "Epicurious","ESPN","Essence","EW","Facebook","FanSided","FoodandWine","FoodNetwork",
                "Fortune","Golf","GolfDigest","Golfsmith","Google","Health","HelloGiggles","HollywoodReporter",
                "HouseBeautiful","InStyle","KelleyBlueBook","Kiplinger","Latina","MadameNoire","MarthaStewart",
                "MimiChatter","Money","MyRecipes","MySpace","NYTimes","Oprah","People","PeopleEnEspanol",
                "PeopleStyleWatch","Pinterest","PopSugar","PopSugar_fitness","RealSimple","Refinery29",
                "RobbReport","SheKnows","SportsIllustrated","SouthernLiving","Sunset","Telemundo","TheDrive",
                "Thrillist","Time","TMZ","TraditionalHome","TravelandLeisure","Twitter","USMagazine",
                "Variety","Vice","Vox","Whowhatwear","Womenshealth","WSJ","XOJane","Yahoo","YahooFinance","None"); names(web)
webb <- subset(x = web, select = -c(Facebook,MySpace,Google,Twitter,None)); names(webb)
sum(is.na(webb[,-c(1)]))/76/nrow(webb[,-c(1)])
head(webb$id, n = 10)
summary(webb$Sports120); summary(webb$YahooFinance)

# PCA
pc <- principal(r = webb[,-c(1)], nfactors = 76, residuals = T, rotate = "varimax",
                scores = F, missing = F); print(pc)
plot(pc$values)
pr <- paran(x = webb[,-c(1)], status = T, all = T, 
            cfa = F, graph = T, color = T, seed = 101)
pr <- paran(x = webb[,-c(1)], centile = 95, status = T, all = T, 
            cfa = F, graph = T, color = T, seed = 101)
pc <- principal(r = webb[,-c(1)], nfactors = 5, residuals = T, rotate = "varimax",
                scores = F, missing = F); print(pc)
pc <- principal(r = webb[,-c(1)], nfactors = 10, residuals = T, rotate = "varimax",
                scores = F, missing = F); print(pc)
pc <- principal(r = webb[,-c(1)], nfactors = 15, residuals = T, rotate = "varimax",
                scores = F, missing = F); print(pc)
pc <- principal(r = webb[,-c(1)], nfactors = 20, residuals = T, rotate = "varimax",
                scores = F, missing = F); print(pc)

# Total Scores

# Fashion
webb$fashion <- 2*composite(set = webb[,c("Whowhatwear","InStyle")], nomiss = .75, 
                              rel = T)
sum(is.na(webb$fashion)); table(webb$fashion, exclude = NULL)
webb$fashion_bin <- Recode(var = webb$fashion, recodes = "1:10=1", as.numeric.result = T)
table(webb$fashion_bin, exclude = NULL)

# Celebrity
webb$celebrity <- 6*composite(set = webb[,c("Cosmopolitan","HollywoodReporter","USMagazine",
                                            "People","PeopleStyleWatch","EW")], nomiss = .75, 
                         rel = T)
sum(is.na(webb$celebrity)); table(webb$celebrity, exclude = NULL)
webb$celebrity_bin <- Recode(var = webb$celebrity, recodes = "1:10=1", as.numeric.result = T)
table(webb$celebrity_bin, exclude = NULL)

# News
webb$news <- 2*composite(set = webb[,c("Vice","Vox")], nomiss = .75, 
                         rel = T)
sum(is.na(webb$news)); table(webb$news, exclude = NULL)
webb$news_bin <- Recode(var = webb$news, recodes = "1:10=1", as.numeric.result = T)
table(webb$news_bin, exclude = NULL)

# Food
webb$food <- 7*composite(set = webb[,c("AllReciples","BonAppetit","CookingLight",
                                       "Epicurious","FoodandWine","FoodNetwork",
                                       "MyRecipes")], nomiss = .75, 
                         rel = T)
sum(is.na(webb$food)); table(webb$food, exclude = NULL)
webb$food_bin <- Recode(var = webb$food, recodes = "1:10=1", as.numeric.result = T)
table(webb$food_bin, exclude = NULL)

# Home Decorating
webb$home <- 4*composite(set = webb[,c("CoastalLiving","CountryLiving","SouthernLiving",
                                       "TraditionalHome")], nomiss = .75, 
                           rel = T)
sum(is.na(webb$home)); table(webb$home, exclude = NULL)
webb$home_bin <- Recode(var = webb$home, recodes = "1:10=1", as.numeric.result = T)
table(webb$home_bin, exclude = NULL)

# Travel
webb$travel <- 3*composite(set = webb[,c("CNTraveler","Departures","TravelandLeisure")], nomiss = .75, 
                          rel = T)
sum(is.na(webb$travel)); table(webb$travel, exclude = NULL)
webb$travel_bin <- Recode(var = webb$travel, recodes = "1:10=1", as.numeric.result = T)
table(webb$travel_bin, exclude = NULL)

# Money
webb$money <- 3*composite(set = webb[,c("Money","CNNMoney","Kiplinger")], nomiss = .75, 
                             rel = T)
sum(is.na(webb$money)); table(webb$money, exclude = NULL)
webb$money_bin <- Recode(var = webb$money, recodes = "1:10=1", as.numeric.result = T)
table(webb$money_bin, exclude = NULL)

# Business
webb$business <- 5*composite(set = webb[,c("BusinessInsider","Fortune","YahooFinance","WSJ",
                                           "NYTimes")], nomiss = .75, 
                           rel = T)
sum(is.na(webb$business)); table(webb$business, exclude = NULL)
webb$business_bin <- Recode(var = webb$business, recodes = "1:10=1", as.numeric.result = T)
table(webb$business_bin, exclude = NULL)

# Sports
webb$sports <- 8*composite(set = webb[,c("Sports120","BleacherReport","ESPN","FanSided",
                                          "Golf","GolfDigest","Golfsmith","SportsIllustrated")], nomiss = .75, 
                             rel = T)
sum(is.na(webb$sports)); table(webb$sports, exclude = NULL)
webb$sports_bin <- Recode(var = webb$sports, recodes = "1:10=1", as.numeric.result = T)
table(webb$sports_bin, exclude = NULL)

# Number of Website Genres
webb$web_total <- 9*composite(set = webb[,c("fashion_bin","celebrity_bin","home_bin","food_bin",
                                               "sports_bin","business_bin","money_bin","travel_bin",
                                               "news_bin")], nomiss = .75, 
                                rel = T)
sum(is.na(webb$web_total)); mean(webb$web_total, na.rm = T); table(webb$web_total, exclude = NULL)

# variables check
names(webb)

## 10. Values - words for things people value  (1 - 7 likert)

# cleaning
val <- seg[,c(1,1047:1088)]; names(val)
names(val) <- c("id","Wealth","Status","Ambition","Honesty","Nature","Enviornment",
                "Creativity","Freedom","Curiosity","Public_image","Protect-family",
                "Social_welfare","Equality","Stable_relations","Romance","Enjoy_life",
                "Having_fun","Adventure","Sex","Looking_good","Duty","Ancestors","Gender_roles",
                "Faith","Learning","Helpful","Friendship","Power","Open_minded","Social_tolerance",
                "Authenticity","Self_reliance","Tradition","Feeling_young","Excitement","Self_interest",
                "Knowledge","Simplicity","Cultural_purity","Working_hard","Modesty","Thrifty"); names(val)
valb <- subset(x = val, select = -c(Curiosity, Cultural_purity)); names(valb)
sum(is.na(valb[,-c(1)]))/40/nrow(valb[,-c(1)])
head(valb$id,, n = 10)
nrow(complete.cases(valb)); head(valb, n = 10)
summary(valb$Wealth); summary(valb$Thrifty)

# PCA
pc <- principal(r = valb[,-c(1)], nfactors = 40, residuals = T, rotate = "varimax",
                scores = F, missing = F); print(pc)
plot(pc$values)
# paran won't run because there are 0 complete cases
# pr <- paran(x = valb, status = T, all = T, 
#             cfa = F, graph = T, color = T, seed = 101)
# pr <- paran(x = valb, centile = 95, status = T, all = T, 
#             cfa = F, graph = T, color = T, seed = 101)
pc <- principal(r = valb[,-c(1)], nfactors = 5, residuals = T, rotate = "varimax",
                scores = F, missing = F); print(pc)
pc <- principal(r = valb[,-c(1)], nfactors = 7, residuals = T, rotate = "varimax",
                scores = F, missing = F); print(pc)
pc <- principal(r = valb[,-c(1)], nfactors = 10, residuals = T, rotate = "varimax",
                scores = F, missing = F); print(pc)

# Total scores

# Romance
valb$romance_tot <- (composite(set = valb[,c("Romance","Sex")], nomiss = .75,
                           rel = T) - 1)/6*100
sum(is.na(valb$romance_tot)); mean(valb$romance_tot, na.rm = T); range(valb$romance_tot, na.rm = T)
hist(valb$romance_tot, n =50)

# Hedonia
valb$hedonia_tot <- (composite(set = valb[,c("Enjoy_life","Having_fun","Adventure",
                                           "Excitement")], nomiss = .75,
                             rel = T) - 1)/6*100
sum(is.na(valb$hedonia_tot)); mean(valb$hedonia_tot, na.rm = T); range(valb$hedonia_tot, na.rm = T)
hist(valb$hedonia_tot, n =50)

# Tradition
valb$tradition_tot <- (composite(set = valb[,c("Ancestors","Gender_roles","Faith",
                                              "Tradition")], nomiss = .75,
                                rel = T) - 1)/6*100
sum(is.na(valb$tradition_tot)); mean(valb$tradition_tot, na.rm = T); range(valb$tradition_tot, na.rm = T)
hist(valb$tradition_tot, n =50)

# Independence
valb$independence_tot <- (composite(set = valb[,c("Freedom","Authenticity","Self_reliance",
                                        "Knowledge")], nomiss = .75,
                          rel = T) - 1)/6*100
sum(is.na(valb$independence_tot)); mean(valb$independence_tot, na.rm = T); range(valb$independence_tot, na.rm = T)
hist(valb$independence_tot, n =50)

# Status
valb$status_tot <- (composite(set = valb[,c("Wealth","Status","Ambition",
                                        "Public_image","Looking_good","Power",
                                        "Self_interest")], nomiss = .75,
                          rel = T) - 1)/6*100
sum(is.na(valb$status_tot)); mean(valb$status_tot, na.rm = T); range(valb$status_tot, na.rm = T)
hist(valb$status_tot, n =50)

# Duty
valb$duty_tot <- (composite(set = valb[,c("Protect-family","Stable_relations","Duty",
                                        "Helpful","Friendship","Working_hard")], nomiss = .75,
                          rel = T) - 1)/6*100
sum(is.na(valb$duty_tot)); mean(valb$duty_tot, na.rm = T); range(valb$duty_tot, na.rm = T)
hist(valb$duty_tot, n =50)

# Nature
valb$nature_tot <- (composite(set = valb[,c("Nature","Enviornment")], nomiss = .75,
                           rel = T) - 1)/6*100
sum(is.na(valb$nature_tot)); mean(valb$nature_tot, na.rm = T); range(valb$nature_tot, na.rm = T)
hist(valb$nature_tot, n =50)

# Social Justice
valb$justice_tot <- (composite(set = valb[,c("Social_welfare","Equality","Open_minded",
                                         "Social_tolerance")], nomiss = .75,
                             rel = T) - 1)/6*100
sum(is.na(valb$justice_tot)); mean(valb$justice_tot, na.rm = T); range(valb$justice_tot, na.rm = T)
hist(valb$justice_tot, n =50)

# variables check
names(valb)

# 11. attitudes - statements people agreed or disagreed with (1-4 likert)

# cleaning
att <- seg[,c(1,1146:1153)]; names(att)
names(att) <- c("id","Under_control","Friends_important","Avoid_conflict","Get_want_now",
                "Stress_alot","Short_attention","Emotion_aware","handle_problems"); names(att)
attb <- att; names(attb)
sum(is.na(attb[,c(-1)]))/8/nrow(attb[,c(-1)])
head(attb$id, n = 10)
nrow(complete.cases(attb))
summary(attb$Under_control); summary(attb$handle_problems)

# PCA
pc <- principal(r = attb[,c(-1)], nfactors = 8, residuals = T, rotate = "varimax",
                scores = F, missing = F); print(pc)
plot(pc$values)
# paran does not run because there are no complete cases
# pr <- paran(x = attb, status = T, all = T, 
#             cfa = F, graph = T, color = T, seed = 101)
# pr <- paran(x = attb, centile = 95, status = T, all = T, 
#             cfa = F, graph = T, color = T, seed = 101)
pc <- principal(r = attb[,c(-1)], nfactors = 2, residuals = T, rotate = "varimax",
                scores = F, missing = F); print(pc)
pc <- principal(r = attb[,c(-1)], nfactors = 3, residuals = T, rotate = "varimax",
                scores = F, missing = F); print(pc)
pc <- principal(r = attb[,c(-1)], nfactors = 4, residuals = T, rotate = "varimax",
                scores = F, missing = F); print(pc)

# Descriptive Statistics
corr.test(attb[,c(-1)])
table(attb$Under_control, exclude = NULL)
table(attb$Friends_important, exclude = NULL)
table(attb$Avoid_conflict, exclude = NULL)
table(attb$Get_want_now, exclude = NULL)
table(attb$Stress_alot, exclude = NULL)
table(attb$Short_attention, exclude = NULL)
table(attb$Emotion_aware, exclude = NULL)
table(attb$handle_problems, exclude = NULL)

# Total Scores = each item on its own (Recode to be dichotomous)
attb$Under_control_bin <- Recode(var = attb$Under_control, recodes = "1:2=1; 3:4=0", as.numeric.result = T)
table(attb$Under_control_bin, exclude = NULL)
attb$Friends_important_bin <- Recode(var = attb$Friends_important, recodes = "1:2=1; 3:4=0", as.numeric.result = T)
table(attb$Friends_important_bin, exclude = NULL)
attb$Avoid_conflict_bin <- Recode(var = attb$Avoid_conflict, recodes = "1:2=1; 3:4=0", as.numeric.result = T)
table(attb$Avoid_conflict_bin, exclude = NULL)
attb$Get_want_now_bin <- Recode(var = attb$Get_want_now, recodes = "1:2=1; 3:4=0", as.numeric.result = T)
table(attb$Get_want_now_bin, exclude = NULL)
attb$Stress_alot_bin <- Recode(var = attb$Stress_alot, recodes = "1:2=1; 3:4=0", as.numeric.result = T)
table(attb$Stress_alot_bin, exclude = NULL)
attb$Short_attention_bin <- Recode(var = attb$Short_attention, recodes = "1:2=1; 3:4=0", as.numeric.result = T)
table(attb$Short_attention_bin, exclude = NULL)
attb$Emotion_aware_bin <- Recode(var = attb$Emotion_aware, recodes = "1:2=1; 3:4=0", as.numeric.result = T)
table(attb$Emotion_aware_bin, exclude = NULL)
attb$handle_problems_bin <- Recode(var = attb$handle_problems, recodes = "1:2=1; 3:4=0", as.numeric.result = T)
table(attb$handle_problems_bin, exclude = NULL)

# variable check
names(attb)

### Combine datasets into final .csv file

# Confirm datasets correct
names(seg); nrow(seg) # initial data
names(demb); nrow(demb) # demographics
names(selfb); nrow(selfb) # self-descriptions
names(smb); nrow(smb) # social media
names(expb); nrow(expb) # expertise
names(pasb); nrow(pasb) # passions
names(readb); nrow(readb) # magazines
names(webb); nrow(webb) # websites
names(valb); nrow(valb) # values
names(attb); nrow(attb) # attitudes

# rename variables

# expb
names(expb) <- c("id","Automobiles_exp","Beauty_exp","Alcohol_exp","Books_mags_exp","Celebrity_exp",     
                 "Cooking_exp","Education_exp","Green_products_exp","Fashion_exp","Finance_exp","Fishing_exp",       
                 "Gardening_exp","Healthcare_exp","Health_fitness_exp","Houses_exp","Decorating_exp","Hunting_exp",       
                 "Movies_tv_exp","Music_exp","Technology_exp","Cleaning_org_exp","Parenting_exp","Pets_exp",          
                 "Photography_exp","Politics_exp","Real_estate_exp","Religion_exp","Sports_exp","My_ethnicity_exp",  
                 "Travel_exp","Video_games_exp","expert_total"); names(expb)
# pasb
names(pasb) <- c("id","Automobiles_pas","Beauty_pas","Alcohol_pas","Books_mags_pas","Celebrity_pas",     
                 "Cooking_pas","Education_pas","Green_products_pas","Fashion_pas","Finance_pas","Fishing_pas",       
                 "Gardening_pas","Healthcare_pas","Health_fitness_pas","Houses_pas","Decorating_pas","Hunting_pas",       
                 "Movies_tv_pas","Music_pas","Technology_pas","Cleaning_org_pas","Parenting_pas","Pets_pas",          
                 "Photography_pas","Politics_pas","Real_estate_pas","Religion_pas","Sports_pas","My_ethnicity_pas",  
                 "Travel_pas","Video_games_pas","passion_total"); names(pasb)

# readb
names(readb) <- c("id","Afar_read","Businessweek_read","Bon_appetit",         
                  "Coastal_living_read","Traveler_condo_read","Cooking_light_read","Cosmopolitan_read",        
                  "Country_living_read","Departures_read","Eating_well_read","Ebony_read",               
                  "Elle_read","Entertainment_weekly_read","ESPN_mag_read","Essence_read",             
                  "Food_wine_read","Food_network_mag_read","Forbes_read","Fortune_read",             
                  "Glamour_read","Golf_read","Golf_digest_read","Harpers_bazaar_read",      
                  "Health_read","House_beautiful_read","InTouch_read","InStyle_read",             
                  "Kiplinger_finance_read","Latina_read","Martha_stewart_read","Mens_health_read",         
                  "Money_read","NYtimes_read","Oprah_read","People_read",              
                  "People_espanol_read","Real_simple_read","Robb_report_read","Rolling_stone_read",       
                  "Saveur_read","Shape_read","Southern_living_read","Sports_illustrated_read",  
                  "Style_watch_read","Sunset_read","Time_read","Traditional_home_read",    
                  "Travel_leisure_read","USweekly_read","Vanidades_read","Vanity_fair_read",         
                  "Wallstreet_read","Womens_health_read","travel_read","travel_bin_read",          
                  "health_read","health_bin_read","home_read","home_bin_read",            
                  "sports_read","sports_bin_read","food_read","food_bin_read",            
                  "money_read","money_bin_read","business_read","business_bin_read",        
                  "celebrity_read","celebrity_bin_read","fashion_read","fashion_bin_read",         
                  "read_total"); names(readb)
# webb
names(webb) <- c("id","Sports120_web","AllReciples_web","Amazon_web","AOL_web",              
                 "BET_web","BleacherReport_web","BonAppetit_web","BusinessInsider_web","Buzzfeed_web",         
                 "CNNMoney_web","CNTraveler_web","CoastalLiving_web","CookingLight_web","Cosmopolitan_web",     
                 "CountryLiving_web","Cozi_web","Departures_web","Discovery_web","Eatingwell_web",       
                 "Edmunds_web","Epicurious_web","ESPN_web","Essence_web","EW_web",               
                 "FanSided_web","FoodandWine_web","FoodNetwork_web","Fortune_web","Golf",             
                 "GolfDigest_web","Golfsmith_web","Health_web","HelloGiggles_web","HollywoodReporter",
                 "HouseBeautiful_web","InStyle_web","KelleyBlueBook_web","Kiplinger_web","Latina",           
                 "MadameNoire_web","MarthaStewart_web","MimiChatter_web","Money_web","MyRecipes",        
                 "NYTimes_web","Oprah_web","People_web","PeopleEnEspanol_web","PeopleStyleWatch_web", 
                 "Pinterest_web","PopSugar_web","PopSugar_fitness_web","RealSimple_web","Refinery29",       
                 "RobbReport_web","SheKnows_web","SportsIllustrated_web","SouthernLiving_web","Sunset",           
                 "Telemundo_web","TheDrive_web","Thrillist_web","Time_web","TMZ",              
                 "TraditionalHome_web","TravelandLeisure_web","USMagazine_web","Variety_web","Vice",             
                 "Vox_web","Whowhatwear_web","Womenshealth_web","WSJ_web","XOJane",           
                 "Yahoo_web","YahooFinance_web","fashion_web","fashion_bin_web","celebrity",        
                 "celebrity_bin_web","news_web","news_bin_web","food_web","food_bin",         
                 "home_web","home_bin_web","travel_web","travel_bin_web","money_web",            
                 "money_bin_web","business_web","business_bin_web","sports_web","sports_bin_web",       
                 "web_total"); names(webb)
# Merge datasets
names(seg)[1] <- "id"; names(seg[,c(1:3)])
seg1 <- merge(x = seg, y = demb, by = "id", all = T)
seg2 <- merge(x = seg1, y = selfb, by = "id", all = T)
seg3 <- merge(x = seg2, y = smb, by = "id", all = T)
seg4 <- merge(x = seg3, y = expb, by = "id", all = T)
seg5 <- merge(x = seg4, y = pasb, by = "id", all = T)
seg6 <- merge(x = seg5, y = readb, by = "id", all = T)
seg7 <- merge(x = seg6, y = webb, by = "id", all = T)
seg8 <- merge(x = seg7, y = valb, by = "id", all = T)
seg9 <- merge(x = seg8, y = attb, by = "id", all = T)
options(max.print = 10000); names(seg9)
nrow(seg9)

# Write out (code ran on 01/11/17 by David Disabato)
write.csv(x = seg9, file = "Time3000 data with 25item clusters_outcomes.csv",
          sep = ",", na = "NA", row.names = F, col.names = T)

# Recode NAs and then write out (code ran on 01/23/17 by David Disabato)
seg10 <- seg9
seg10[is.na(seg10)] <- -999
write.csv(x = seg10, file = "Time3000 data with 25item clusters_outcomes_noNAs.csv",
          sep = ",", na = "NA", row.names = F, col.names = T)
