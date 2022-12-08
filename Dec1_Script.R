
#upload data 
library(readxl)
cchs2014 <- read_excel("cchs2014.xlsx")

########Step 1: exploratory analysis########## 

#make one way frequency tables 
table(cchs2014$CCC_280) #exposure variable for mood disorder  
table(cchs2014$UCN_010) #outcome variable for UHCN

#co-variates 

#predisposing factors 
table(cchs2014$DHHGAGE) #age 
table(cchs2014$DHH_SEX) #sex 
table(cchs2014$SDCGCGT) #race
table(cchs2014$EDUDH04) #education level 
table(cchs2014$SDCFIMM) #immigrant 
table(cchs2014$DHHGMS) #marital status 
table(cchs2014$GEN_10) #belonging to community 

#enabling factors 
table(cchs2014$INCG2) #household income
table(cchs2014$GEOGPRV) #province of residence 
table(cchs2014$HCU_1AA) #regular medical doctor 
table(cchs2014$DOINS) #extended health insurance 

#need factors 
table(cchs2014$LOPG050) #chronic conditions 
table(cchs2014$GEN_01) #perceived health

#check visual normal distribution of each variable 
Mood_Disorder <- cchs2014$CCC_280 
hist(Mood_Disorder)  ## non-normal, heavy skew right (no)

UHCN <- cchs2014$UCN_010 
hist(UHCN) ## non-normal, heavy skew right (no)

age <- cchs2014$DHHGAGE 
hist(age) #normal, slight skew right (older)

sex <- cchs2014$DHH_SEX 
hist(sex) ##non-normal 

race <- cchs2014$SDCGCGT 
hist(race) ## non-normal, heavy skew left (white)

education_level <- cchs2014$EDUDH04 
hist(education_level) ##non-normal, heavy skew right (post-secondary)

immigrant <- cchs2014$SDCFIMM
hist(immigrant) ##non-normal, heavy skew right (no)

martial_status <- cchs2014$DHHGMS 
hist(martial_status) ##non-normal, skew right(never marry), mode(married)

sense_of_belonging <- cchs2014$GEN_10
hist(sense_of_belonging)##non-normal, skew left (somewhat strong)

household_income <- cchs2014$INCG2
hist(household_income) ##non-normal, skew left (employed)

province <- cchs2014$GEOGPRV 
hist(province)##non-normal, mode(Ontario)

regular_doctor <- cchs2014$HCU_1AA 
hist(regular_doctor) #non-normal, heavy skew left (yes)

health_insurance <- cchs2014$DOINS
hist(health_insurance) #non-normal, heavy skew right (no)

chronic_conditions <- cchs2014$LOPG050
hist(chronic_conditions) ##non_normal, heavy skew right (NA)

self_health <- cchs2014$GEN_01 
hist(self_health) ##non-normal, skew left (very good)


############step 2: make necessary exclusions to dataset######### 

#exclude any data that is "don't know, refused or not stated" 
full2014 <- subset(cchs2014,CCC_280 <3)
full20142 <- subset(full2014,UCN_010 <3)
full20143 <- subset(full20142,SDCGCGT<3) 
full20143 <- subset(full20143,EDUDH04<5)
full20143 <- subset(full20143,SDCFIMM<3)
full20143 <- subset(full20143,GEN_10<5)
full20143 <- subset(full20143,INCG2<5)
full20143 <- subset(full20143,HCU_1AA<3)
full20143 <- subset(full20143, GEN_01<6)
full20143 <- subset(full20143,DHHGMS<5)
full20143 <- subset(full20143,LOPG050<98)   


#exclude age less than or equal to 18 
excluded <- subset(cchs2014,DHHGAGE >=3)


################step2.5 make descriptive tables for catergorical### 

#make two way tables 
table(full20143$CCC_280,full20143$UCN_010)
mood1 <- table(full20143$CCC_280,full20143$UCN_010)
chisq.test(mood1)

table(full20143$DHHGAGE,full20143$UCN_010)
age1 <- table(full20143$DHHGAGE,full20143$UCN_010)
chisq.test(age1)

table(full20143$INCG2,full20143$UCN_010)
income1 <- table(full20143$INCG2,full20143$UCN_010)
chisq.test(income1)

table(full20143$DHH_SEX,full20143$UCN_010)
sex1 <- table(full20143$DHH_SEX,full20143$UCN_010)
chisq.test(sex1)

table(full20143$SDCGCGT,full20143$UCN_010)
race1 <- table(full20143$SDCGCGT,full20143$UCN_010)
chisq.test(race1)

table(full20143$DHHGMS,full20143$UCN_010)
marry1 <- table(full20143$DHHGMS,full20143$UCN_010)
chisq.test(marry1)

table(full20143$GEN_10,full20143$UCN_010)
community1 <- table(full20143$GEN_10,full20143$UCN_010)
chisq.test(community1)

table(full20143$EDUDH04,full20143$UCN_010)
education1 <- table(full20143$EDUDH04,full20143$UCN_010)
chisq.test(education1)

table(full20143$SDCFIMM,full20143$UCN_010)
immigrant1 <- table(full20143$SDCFIMM,full20143$UCN_010)
chisq.test(immigrant1)

table(full20143$HCU_1AA,full20143$UCN_010)
doc1 <- table(full20143$HCU_1AA,full20143$UCN_010)
chisq.test(doc1)

table(full20143$DOINS,full20143$UCN_010)
insurance1 <- table(full20143$DOINS,full20143$UCN_010)
chisq.test(insurance1)

table(full20143$DOINS,full20143$UCN_010)
insurance1 <- table(full20143$DOINS,full20143$UCN_010)
chisq.test(insurance1)

table(full20143$GEN_01,full20143$UCN_010)
self1 <- table(full20143$GEN_01,full20143$UCN_010)
chisq.test(self1)

table(full20143$GEOGPRV,full20143$UCN_010)
prov1 <- table(full20143$GEOGPRV,full20143$UCN_010)
chisq.test(prov1)

#############step 3: conduct survey-weighted univariate analysis####
full20143$WTS_M
weighted_data <- subset(full20143, WTS_M=="") #### this didnt work 

#############step 4: check assumptions of multivariate logistic regression####

#check no perfect multicollinearity 

install.packages("carData")

mylogit <- glm(UCN_010~CCC_280+ DHHGAGE+ INCG2 +DHH_SEX 
               + SDCGCGT + DHHGMS + GEN_10 + EDUDH04 +SDCFIMM
               + HCU_1AA + DOINS + GEN_01 + GEOGPRV, data = full20143 )
vif(mylogit)

#Sufficient events per variable 

table(full20143$CCC_280,full20143$UCN_010)
mood1 <- table(full20143$CCC_280,full20143$UCN_010)

table(full20143$DHHGAGE,full20143$UCN_010)
age1 <- table(full20143$DHHGAGE,full20143$UCN_010)

table(full20143$INCG2,full20143$UCN_010)
income1 <- table(full20143$INCG2,full20143$UCN_010)

table(full20143$DHH_SEX,full20143$UCN_010)
sex1 <- table(full20143$DHH_SEX,full20143$UCN_010)

table(full20143$SDCGCGT,full20143$UCN_010)
race1 <- table(full20143$SDCGCGT,full20143$UCN_010)

table(full20143$DHHGMS,full20143$UCN_010)
marry1 <- table(full20143$DHHGMS,full20143$UCN_010)

table(full20143$GEN_10,full20143$UCN_010)
community1 <- table(full20143$GEN_10,full20143$UCN_010)

table(full20143$EDUDH04,full20143$UCN_010)
education1 <- table(full20143$EDUDH04,full20143$UCN_010)

table(full20143$SDCFIMM,full20143$UCN_010)
immigrant1 <- table(full20143$SDCFIMM,full20143$UCN_010)

table(full20143$HCU_1AA,full20143$UCN_010)
doc1 <- table(full20143$HCU_1AA,full20143$UCN_010)

table(full20143$DOINS,full20143$UCN_010)
insurance1 <- table(full20143$DOINS,full20143$UCN_010)

table(full20143$DOINS,full20143$UCN_010)
insurance1 <- table(full20143$DOINS,full20143$UCN_010)

table(full20143$GEN_01,full20143$UCN_010)
self1 <- table(full20143$GEN_01,full20143$UCN_010)

table(full20143$GEOGPRV,full20143$UCN_010)
prov1 <- table(full20143$GEOGPRV,full20143$UCN_010)

#collinearity 

cor.test(full20143$CCC_280,full20143$UCN_010)
cor.test(full20143$DHHGAGE,full20143$UCN_010)
cor.test(full20143$INCG2,full20143$UCN_010)
cor.test(full20143$DHH_SEX,full20143$UCN_010)
cor.test(full20143$SDCGCGT,full20143$UCN_010)
cor.test(full20143$DHHGMS,full20143$UCN_010)
cor.test(full20143$GEN_10,full20143$UCN_010)
cor.test(full20143$EDUDH04,full20143$UCN_010)
cor.test(full20143$SDCFIMM,full20143$UCN_010)
cor.test(full20143$HCU_1AA,full20143$UCN_010)
cor.test(full20143$DOINS,full20143$UCN_010)
cor.test(full20143$DOINS,full20143$UCN_010)
cor.test(full20143$GEN_01,full20143$UCN_010)
cor.test(full20143$GEOGPRV,full20143$UCN_010)

cor.test(full20143$CCC_280,full20143$CCC_280)
cor.test(full20143$DHHGAGE,full20143$CCC_280)
cor.test(full20143$INCG2,full20143$CCC_280)
cor.test(full20143$DHH_SEX,full20143$CCC_280)
cor.test(full20143$SDCGCGT,full20143$CCC_280)
cor.test(full20143$DHHGMS,full20143$CCC_280)
cor.test(full20143$GEN_10,full20143$CCC_280)
cor.test(full20143$EDUDH04,full20143$CCC_280)
cor.test(full20143$SDCFIMM,full20143$CCC_280)
cor.test(full20143$HCU_1AA,full20143$CCC_280)
cor.test(full20143$DOINS,full20143$CCC_280)
cor.test(full20143$DOINS,full20143$CCC_280)
cor.test(full20143$GEN_01,full20143$CCC_280)
cor.test(full20143$GEOGPRV,full20143$CCC_280)


cor.test(full20143$CCC_280,full20143$DHHGAGE)
cor.test(full20143$DHHGAGE,full20143$DHHGAGE)
cor.test(full20143$INCG2,full20143$DHHGAGE)
cor.test(full20143$DHH_SEX,full20143$DHHGAGE)
cor.test(full20143$SDCGCGT,full20143$DHHGAGE)
cor.test(full20143$DHHGMS,full20143$DHHGAGE)
cor.test(full20143$GEN_10,full20143$DHHGAGE)
cor.test(full20143$EDUDH04,full20143$DHHGAGE)
cor.test(full20143$SDCFIMM,full20143$DHHGAGE)
cor.test(full20143$HCU_1AA,full20143$DHHGAGE)
cor.test(full20143$DOINS,full20143$DHHGAGE)
cor.test(full20143$DOINS,full20143$DHHGAGE)
cor.test(full20143$GEN_01,full20143$DHHGAGE)
cor.test(full20143$GEOGPRV,full20143$DHHGAGE)


cor.test(full20143$CCC_280,full20143$INCG2)
cor.test(full20143$DHHGAGE,full20143$INCG2)
cor.test(full20143$INCG2,full20143$INCG2)
cor.test(full20143$DHH_SEX,full20143$INCG2)
cor.test(full20143$SDCGCGT,full20143$INCG2)
cor.test(full20143$DHHGMS,full20143$INCG2)
cor.test(full20143$GEN_10,full20143$INCG2)
cor.test(full20143$EDUDH04,full20143$INCG2)
cor.test(full20143$SDCFIMM,full20143$INCG2)
cor.test(full20143$HCU_1AA,full20143$INCG2)
cor.test(full20143$DOINS,full20143$INCG2)
cor.test(full20143$DOINS,full20143$INCG2)
cor.test(full20143$GEN_01,full20143$INCG2)
cor.test(full20143$GEOGPRV,full20143$INCG2)


cor.test(full20143$CCC_280,full20143$INCG2)
cor.test(full20143$DHHGAGE,full20143$INCG2)
cor.test(full20143$INCG2,full20143$INCG2)
cor.test(full20143$DHH_SEX,full20143$INCG2)
cor.test(full20143$SDCGCGT,full20143$INCG2)
cor.test(full20143$DHHGMS,full20143$INCG2)
cor.test(full20143$GEN_10,full20143$INCG2)
cor.test(full20143$EDUDH04,full20143$INCG2)
cor.test(full20143$SDCFIMM,full20143$INCG2)
cor.test(full20143$HCU_1AA,full20143$INCG2)
cor.test(full20143$DOINS,full20143$INCG2)
cor.test(full20143$DOINS,full20143$INCG2)
cor.test(full20143$GEN_01,full20143$INCG2)
cor.test(full20143$GEOGPRV,full20143$INCG2)


cor.test(full20143$CCC_280,full20143$DHH_SEX)
cor.test(full20143$DHHGAGE,full20143$DHH_SEX)
cor.test(full20143$INCG2,full20143$DHH_SEX)
cor.test(full20143$DHH_SEX,full20143$DHH_SEX)
cor.test(full20143$SDCGCGT,full20143$DHH_SEX)
cor.test(full20143$DHHGMS,full20143$DHH_SEX)
cor.test(full20143$GEN_10,full20143$DHH_SEX)
cor.test(full20143$EDUDH04,full20143$DHH_SEX)
cor.test(full20143$SDCFIMM,full20143$DHH_SEX)
cor.test(full20143$HCU_1AA,full20143$DHH_SEX)
cor.test(full20143$DOINS,full20143$DHH_SEX)
cor.test(full20143$DOINS,full20143$DHH_SEX)
cor.test(full20143$GEN_01,full20143$DHH_SEX)
cor.test(full20143$GEOGPRV,full20143$DHH_SEX)

cor.test(full20143$CCC_280,full20143$SDCGCGT)
cor.test(full20143$DHHGAGE,full20143$SDCGCGT)
cor.test(full20143$INCG2,full20143$SDCGCGT)
cor.test(full20143$DHH_SEX,full20143$SDCGCGT)
cor.test(full20143$SDCGCGT,full20143$SDCGCGT)
cor.test(full20143$DHHGMS,full20143$SDCGCGT)
cor.test(full20143$GEN_10,full20143$SDCGCGT)
cor.test(full20143$EDUDH04,full20143$SDCGCGT)
cor.test(full20143$SDCFIMM,full20143$SDCGCGT)
cor.test(full20143$HCU_1AA,full20143$SDCGCGT)
cor.test(full20143$DOINS,full20143$SDCGCGT)
cor.test(full20143$DOINS,full20143$SDCGCGT)
cor.test(full20143$GEN_01,full20143$SDCGCGT)
cor.test(full20143$GEOGPRV,full20143$SDCGCGT)

cor.test(full20143$CCC_280,full20143$DHHGMS)
cor.test(full20143$DHHGAGE,full20143$DHHGMS)
cor.test(full20143$INCG2,full20143$DHHGMS)
cor.test(full20143$DHH_SEX,full20143$DHHGMS)
cor.test(full20143$SDCGCGT,full20143$DHHGMS)
cor.test(full20143$DHHGMS,full20143$DHHGMS)
cor.test(full20143$GEN_10,full20143$DHHGMS)
cor.test(full20143$EDUDH04,full20143$DHHGMS)
cor.test(full20143$SDCFIMM,full20143$DHHGMS)
cor.test(full20143$HCU_1AA,full20143$DHHGMS)
cor.test(full20143$DOINS,full20143$DHHGMS)
cor.test(full20143$DOINS,full20143$DHHGMS)
cor.test(full20143$GEN_01,full20143$DHHGMS)
cor.test(full20143$GEOGPRV,full20143$DHHGMS)

cor.test(full20143$CCC_280,full20143$GEN_10)
cor.test(full20143$DHHGAGE,full20143$GEN_10)
cor.test(full20143$INCG2,full20143$GEN_10)
cor.test(full20143$DHH_SEX,full20143$GEN_10)
cor.test(full20143$SDCGCGT,full20143$GEN_10)
cor.test(full20143$DHHGMS,full20143$GEN_10)
cor.test(full20143$GEN_10,full20143$GEN_10)
cor.test(full20143$EDUDH04,full20143$GEN_10)
cor.test(full20143$SDCFIMM,full20143$GEN_10)
cor.test(full20143$HCU_1AA,full20143$GEN_10)
cor.test(full20143$DOINS,full20143$GEN_10)
cor.test(full20143$DOINS,full20143$GEN_10)
cor.test(full20143$GEN_01,full20143$GEN_10)
cor.test(full20143$GEOGPRV,full20143$GEN_10)

cor.test(full20143$CCC_280,full20143$EDUDH04)
cor.test(full20143$DHHGAGE,full20143$EDUDH04)
cor.test(full20143$INCG2,full20143$EDUDH04)
cor.test(full20143$DHH_SEX,full20143$EDUDH04)
cor.test(full20143$SDCGCGT,full20143$EDUDH04)
cor.test(full20143$DHHGMS,full20143$EDUDH04)
cor.test(full20143$GEN_10,full20143$EDUDH04)
cor.test(full20143$EDUDH04,full20143$EDUDH04)
cor.test(full20143$SDCFIMM,full20143$EDUDH04)
cor.test(full20143$HCU_1AA,full20143$EDUDH04)
cor.test(full20143$DOINS,full20143$EDUDH04)
cor.test(full20143$DOINS,full20143$EDUDH04)
cor.test(full20143$GEN_01,full20143$EDUDH04)
cor.test(full20143$GEOGPRV,full20143$EDUDH04)

cor.test(full20143$CCC_280,full20143$SDCFIMM)
cor.test(full20143$DHHGAGE,full20143$SDCFIMM)
cor.test(full20143$INCG2,full20143$SDCFIMM)
cor.test(full20143$DHH_SEX,full20143$SDCFIMM)
cor.test(full20143$SDCGCGT,full20143$SDCFIMM)
cor.test(full20143$DHHGMS,full20143$SDCFIMM)
cor.test(full20143$GEN_10,full20143$SDCFIMM)
cor.test(full20143$EDUDH04,full20143$SDCFIMM)
cor.test(full20143$SDCFIMM,full20143$SDCFIMM)
cor.test(full20143$HCU_1AA,full20143$SDCFIMM)
cor.test(full20143$DOINS,full20143$SDCFIMM)
cor.test(full20143$DOINS,full20143$SDCFIMM)
cor.test(full20143$GEN_01,full20143$SDCFIMM)
cor.test(full20143$GEOGPRV,full20143$SDCFIMM)


cor.test(full20143$CCC_280,full20143$HCU_1AA)
cor.test(full20143$DHHGAGE,full20143$HCU_1AA)
cor.test(full20143$INCG2,full20143$HCU_1AA)
cor.test(full20143$DHH_SEX,full20143$HCU_1AA)
cor.test(full20143$SDCGCGT,full20143$HCU_1AA)
cor.test(full20143$DHHGMS,full20143$HCU_1AA)
cor.test(full20143$GEN_10,full20143$HCU_1AA)
cor.test(full20143$EDUDH04,full20143$HCU_1AA)
cor.test(full20143$SDCFIMM,full20143$HCU_1AA)
cor.test(full20143$HCU_1AA,full20143$HCU_1AA)
cor.test(full20143$DOINS,full20143$HCU_1AA)
cor.test(full20143$DOINS,full20143$HCU_1AA)
cor.test(full20143$GEN_01,full20143$HCU_1AA)
cor.test(full20143$GEOGPRV,full20143$HCU_1AA)

cor.test(full20143$CCC_280,full20143$DOINS)
cor.test(full20143$DHHGAGE,full20143$DOINS)
cor.test(full20143$INCG2,full20143$DOINS)
cor.test(full20143$DHH_SEX,full20143$DOINS)
cor.test(full20143$SDCGCGT,full20143$DOINS)
cor.test(full20143$DHHGMS,full20143$DOINS)
cor.test(full20143$GEN_10,full20143$DOINS)
cor.test(full20143$EDUDH04,full20143$DOINS)
cor.test(full20143$SDCFIMM,full20143$DOINS)
cor.test(full20143$HCU_1AA,full20143$DOINS)
cor.test(full20143$DOINS,full20143$DOINS)
cor.test(full20143$DOINS,full20143$DOINS)
cor.test(full20143$GEN_01,full20143$DOINS)
cor.test(full20143$GEOGPRV,full20143$DOINS)


cor.test(full20143$CCC_280,full20143$GEN_01)
cor.test(full20143$DHHGAGE,full20143$GEN_01)
cor.test(full20143$INCG2,full20143$GEN_01)
cor.test(full20143$DHH_SEX,full20143$GEN_01)
cor.test(full20143$SDCGCGT,full20143$GEN_01)
cor.test(full20143$DHHGMS,full20143$GEN_01)
cor.test(full20143$GEN_10,full20143$GEN_01)
cor.test(full20143$EDUDH04,full20143$GEN_01)
cor.test(full20143$SDCFIMM,full20143$GEN_01)
cor.test(full20143$HCU_1AA,full20143$GEN_01)
cor.test(full20143$DOINS,full20143$GEN_01)
cor.test(full20143$DOINS,full20143$GEN_01)
cor.test(full20143$GEN_01,full20143$GEN_01)
cor.test(full20143$GEOGPRV,full20143$GEN_01)


cor.test(full20143$CCC_280,full20143$GEOGPRV)
cor.test(full20143$DHHGAGE,full20143$GEOGPRV)
cor.test(full20143$INCG2,full20143$GEOGPRV)
cor.test(full20143$DHH_SEX,full20143$GEOGPRV)
cor.test(full20143$SDCGCGT,full20143$GEOGPRV)
cor.test(full20143$DHHGMS,full20143$GEOGPRV)
cor.test(full20143$GEN_10,full20143$GEOGPRV)
cor.test(full20143$EDUDH04,full20143$GEOGPRV)
cor.test(full20143$SDCFIMM,full20143$GEOGPRV)
cor.test(full20143$HCU_1AA,full20143$GEOGPRV)
cor.test(full20143$DOINS,full20143$GEOGPRV)
cor.test(full20143$DOINS,full20143$GEOGPRV)
cor.test(full20143$GEN_01,full20143$GEOGPRV)
cor.test(full20143$GEOGPRV,full20143$GEOGPRV)

#goodness of fit test 

mylogit <- glm(UCN_010~CCC_280+ DHHGAGE+ INCG2 +DHH_SEX 
               + SDCGCGT + DHHGMS + GEN_10 + EDUDH04 +SDCFIMM
               + HCU_1AA + DOINS + GEN_01 + GEOGPRV, data = full20143 )

mylogit_excluded <- glm(UCN_010~CCC_280+ DHHGAGE+ INCG2 +DHH_SEX 
               + SDCGCGT + DHHGMS + GEN_10 + EDUDH04 +SDCFIMM
               + HCU_1AA + DOINS + GEN_01 + GEOGPRV, data = excluded )

ks.test(full20143$CCC_280,full20143$UCN_010,mylogit)

###############Step 4: Select model for logistic regression########## 

#backward selection 
library(MASS)
mylogit <- glm(UCN_010~CCC_280+ DHHGAGE+ INCG2 +DHH_SEX 
               + SDCGCGT + DHHGMS + GEN_10 + EDUDH04 +SDCFIMM
               + HCU_1AA + DOINS + GEN_01 + GEOGPRV, data = full20143 )

step.model <- stepAIC(mylogit, direction = "backward", 
                      trace = FALSE)

summary(step.model)
##AIC 
AIC(mylogit, step.model)

###############Step 5: Assess whether to keep variables with higher P-values ##########
summary(step.model)
wald.test(Sigma = vcov(step.model), b = coef(step.model), Terms = 3)
wald.test(Sigma = vcov(step.model), b = coef(step.model), Terms = 4)
wald.test(Sigma = vcov(step.model), b = coef(step.model), Terms = 6)

###############Step 6:  ##########

###############Step 7: regression analysis  ##########
step.model
exp(coef(step.model))
