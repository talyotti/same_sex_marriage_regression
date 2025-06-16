# add necessary libraries
library(ggplot2)
library(arm)
library(car)
library(gridExtra)
library(reshape2)
library(dplyr)
library(tidyr)
# set directory
setwd("~/Desktop/ST211/individual_project")

### --------------------------------------------------------------------------------------------

# GETTING & PREPARING DATA
# take a look at the whole data
ip.data <- read.csv("individual_data.csv", header=T)
summary(ip.data)
head(ip.data)

# replace negative values with NA 
gp.data[gp.data < 0] <- NA

### --------------------------------------------------------------------------------------------

# DEALING WITH MISSING VALUES
# calculate the percentage of missing values for each variable
missing_percentage <- (colSums(is.na(ip.data)) / nrow(ip.data)) * 100
missing_percentage

# identify columns with less than 10% missing data (excluding continuous and non-significant categorical variables)
exclude_vars <- c("househld", "livearea","hincpast","intwww","umineth","eqnow3","eqnow9","tea","tunionsa","ansseca","religcat","carehome","anyhcond","orient","glvis","knowgl","ssexmarr")
cols_less_10 <- names(missing_percentage)[missing_percentage <= 10]
cols_less_10 <- setdiff(cols_less_10, exclude_vars) 
cols_less_10

# remove the missing rows for categorical predictors with less than 10% missing
ip.data_filtered <- ip.data[complete.cases(ip.data[, cols_less_10]), ]
nrow(ip.data_filtered) # 862 to 984!
ip.data <- ip.data_filtered
### --------------------------------------------------------------------------------------------

# CREATING A "MISSING" LEVEL
# choose continuous variables
exclude_vars <- c("househld","rage","livearea","persinc2","glsocdist","glvis","ssexmarr")
exclude_vars <- exclude_vars[exclude_vars %in% names(ip.data)]
# convert variables to factors (excluding continuous)
for (var in setdiff(names(ip.data), exclude_vars)) {
  ip.data[[var]] <- as.factor(ip.data[[var]])
  }
# identify categorical columns
non_continuous_columns <- setdiff(names(ip.data), exclude_vars)

# loop through non-continuous columns and add "missing" level to factor variables with NA
for (var in non_continuous_columns) {  
  if (is.factor(ip.data[[var]]) && any(is.na(ip.data[[var]]))) {
    levels(ip.data[[var]]) <- c(levels(ip.data[[var]]), "missing")
  }
}

# replace NA values with "missing" in non-continuous columns
for (col in non_continuous_columns) {
  ip.data[[col]][is.na(ip.data[[col]])] <- "missing"
}

# check their levels
for (var in non_continuous_columns) {  
  cat("Variable:", var, "\n")
  cat("Levels:", levels(ip.data[[var]]), "\n\n")
}
### --------------------------------------------------------------------------------------------

# CONVERT ssexmarr TO BINARY
# change level 2 to 0 in ssexmarr
ip.data$ssexmarr[ip.data$ssexmarr == 2] <- 0
# Confirm that levels are now correct
summary(ip.data$ssexmarr)

### --------------------------------------------------------------------------------------------

# PLOTTING CONTINUOUS VARIABLES
# 1. rage
summary(ip.data$rage)
ip.breaks<-mutate(ip.data, bin=cut(rage,breaks = seq(15, 100, by=5)))

# estimate the proportion of people saying yes to ssexmarr in each bin (y-axis)
prop <- prop.table(with(ip.breaks, table(ssexmarr,bin)),2)[2,]
# the midpoint of each bin (x-axis)
midbin<- seq(17.5, 97.5, by=5)
#create a new data frame using these two variables
icu.bin<-data.frame(midbin,prop)
icu.bin

p1<- ggplot(ip.breaks,aes(x=rage,y=ssexmarr))
p1<-p1+ geom_count() 
p1<-p1+ geom_smooth(method = "glm", method.args = list(family = "binomial"),se = FALSE)
p1<-p1+ geom_point(data=icu.bin, aes(x=midbin,y=prop),shape=2,color="red", size=3) 
p1

# 2. livearea
summary(ip.data$livearea)
ip.breaks<-mutate(ip.data, bin=cut(livearea,breaks = seq(0, 100, by=5)))

# estimate the proportion of people saying yes to ssexmarr in each bin (y-axis)
prop <- prop.table(with(ip.breaks, table(ssexmarr,bin)),2)[2,]
# the midpoint of each bin (x-axis)
midbin<- seq(2.5, 97.5, by=5)
# create a new data frame using these two variables
icu.bin<-data.frame(midbin,prop)
icu.bin

p2<- ggplot(ip.breaks,aes(x=livearea,y=ssexmarr))
p2<-p2+ geom_count() 
p2<-p2+ geom_smooth(method = "glm", method.args = list(family = "binomial"),se = FALSE)
p2<-p2+ geom_point(data=icu.bin, aes(x=midbin,y=prop),shape=2,color="red", size=3) 
p2

# 3. persinc2
summary(ip.data$persinc2)
ip.breaks<-mutate(ip.data, bin=cut(persinc2,breaks = seq(0, 50000, by=2500)))

# estimate the proportion of people saying yes to ssexmarr in each bin (y-axis)
prop <- prop.table(with(ip.breaks, table(ssexmarr,bin)),2)[2,]
# the midpoint of each bin (x-axis)
midbin<- seq(1250, 50000, by=2500)
# create a new data frame using these two variables
icu.bin<-data.frame(midbin,prop)
icu.bin

p3<- ggplot(ip.breaks,aes(x=persinc2,y=ssexmarr))
p3<-p3+ geom_count() 
p3<-p3+ geom_smooth(method = "glm", method.args = list(family = "binomial"),se = FALSE)
p3<-p3+ geom_point(data=icu.bin, aes(x=midbin,y=prop),shape=2,color="red", size=3) 
p3

# 4. glsocdist
summary(ip.data$glsocdist)
ip.breaks<-mutate(ip.data, bin=cut(glsocdist,breaks = seq(0, 11, by=1)))

# estimate the proportion of people saying yes to ssexmarr in each bin (y-axis)
prop <- prop.table(with(ip.breaks, table(ssexmarr,bin)),2)[2,]
# the midpoint of each bin (x-axis)
midbin<- seq(0.5, 10.5, by=1)
#create a new data frame using these two variables
icu.bin<-data.frame(midbin,prop)
icu.bin

p4<- ggplot(ip.breaks,aes(x=glsocdist,y=ssexmarr))
p4<-p4+ geom_count() 
p4<-p4+ geom_smooth(method = "glm", method.args = list(family = "binomial"),se = FALSE)
p4<-p4+ geom_point(data=icu.bin, aes(x=midbin,y=prop),shape=2,color="red", size=3) 
p4

# try turning into factor and check level counts
#ip.data$factor_glsocdist <- factor(ip.data$glsocdist)
#summary(ip.data$factor_glsocdist)

# 5. househld
summary(ip.data$househld)
ip.breaks<-mutate(ip.data, bin=cut(househld,breaks = seq(1, 8, by=1)))

# estimate the proportion of people saying yes to ssexmarr in each bin (y-axis)
prop <- prop.table(with(ip.breaks, table(ssexmarr,bin)),2)[2,]
# the midpoint of each bin (x-axis)
midbin<- seq(1.5, 7.5, by=1)
#create a new data frame using these two variables
icu.bin<-data.frame(midbin,prop)
icu.bin

p5<- ggplot(ip.breaks,aes(x=househld,y=ssexmarr))
p5<-p5+ geom_count() 
p5<-p5+ geom_smooth(method = "glm", method.args = list(family = "binomial"),se = FALSE)
p5<-p5+ geom_point(data=icu.bin, aes(x=midbin,y=prop),shape=2,color="red", size=3) 
p5

# 6. glvis
summary(ip.data$glvis)
ip.breaks<-mutate(ip.data, bin=cut(glvis,breaks = seq(0, 3, by=0.5)))

# estimate the proportion of people saying yes to ssexmarr in each bin (y-axis)
prop <- prop.table(with(ip.breaks, table(ssexmarr,bin)),2)[2,]
# the midpoint of each bin (x-axis)
midbin<- seq(0.5, 3, by=0.5)
#create a new data frame using these two variables
icu.bin<-data.frame(midbin,prop)
icu.bin

p6<- ggplot(ip.breaks,aes(x=glvis,y=ssexmarr))
p6<-p6+ geom_count() 
p6<-p6+ geom_smooth(method = "glm", method.args = list(family = "binomial"),se = FALSE)
p6<-p6+ geom_point(data=icu.bin, aes(x=midbin,y=prop),shape=2,color="red", size=3) 
p6

# PLOTTING CATEGORICAL VARIABLES

# 7. glchild
p7<- ggplot(ip.data, aes(x=factor(glchild), y=ssexmarr))+ geom_boxplot()+ coord_flip()
p7<-p7+theme(legend.position = "none")
p7

# cross tabulation plot 
# 8. uprejgay and glchild
p8<- ggplot(ip.data,aes(x = factor(uprejgay), fill = factor(glchild))) +
  geom_bar(position = "fill") +  scale_y_continuous(name = "Within group Percentage"
                                                    , labels = scales::percent)
p8

# 9. ruhappy
p9<- ggplot(ip.data, aes(x=factor(ruhappy), y=ssexmarr))+ geom_boxplot()+ coord_flip()
p9<-p9+theme(legend.position = "none")
p9

# 10. healthyr
p10<- ggplot(ip.data, aes(x=factor(healthyr), y=ssexmarr))+ geom_boxplot()+ coord_flip()
p10<-p10+theme(legend.position = "none")
p10

# 11. famrelig
p11<- ggplot(ip.data, aes(x=factor(famrelig), y=ssexmarr))+ geom_boxplot()+ coord_flip()
p11<-p11+theme(legend.position = "none")
p11

# 12. tenshort
p12<- ggplot(ip.data, aes(x=factor(tenshort), y=ssexmarr))+ geom_boxplot()+ coord_flip()
p12<-p12+theme(legend.position = "none")
p12

# 13. rsex
p13<- ggplot(ip.data, aes(x=factor(rsex), y=ssexmarr))+ geom_boxplot()+ coord_flip()
p13<-p13+theme(legend.position = "none")
p13

# 14. polpart2
p14<- ggplot(ip.data, aes(x=factor(polpart2), y=ssexmarr))+ geom_boxplot()+ coord_flip()
p14<-p14+theme(legend.position = "none")
p14

# 15. glchild
p15<- ggplot(ip.data, aes(x=factor(glchild), y=ssexmarr))+ geom_boxplot()+ coord_flip()
p15<-p15+theme(legend.position = "none")
p15

# 16. work
p16<- ggplot(ip.data, aes(x=factor(work), y=ssexmarr))+ geom_boxplot()+ coord_flip()
p16<-p16+theme(legend.position = "none")
p16

# 17. chattnd2
p17<- ggplot(ip.data, aes(x=factor(chattnd2), y=ssexmarr))+ geom_boxplot()+ coord_flip()
p17<-p17+theme(legend.position = "none")
p17

# 18. rsect
p18<- ggplot(ip.data, aes(x=factor(rsect), y=ssexmarr))+ geom_boxplot()+ coord_flip()
p18<-p18+theme(legend.position = "none")
p18


### --------------------------------------------------------------------------------------------
# RELEVELLING

# relevelling to most common level
relevel_to_most_common <- function(data) {
  factor_vars <- Filter(is.factor, data)
  for (var in names(factor_vars)) {
    freq <- table(factor_vars[[var]])
    most_common_level <- names(sort(freq, decreasing = TRUE)[1])
    data[[var]] <- relevel(factor_vars[[var]], ref = most_common_level)
  }
  return(data)
}

ip.data <- relevel_to_most_common(ip.data)

# religcat
ip.data$religcat <- relevel(ip.data$religcat, ref=3)

# famrelig
ip.data$famrelig <- relevel(ip.data$famrelig, ref=3)

### --------------------------------------------------------------------------------------------

# APC (Average Predictive Comparisons)

# rage
summary(ip.data$rage)
ip.glm.apc <- glm(ssexmarr~ rage ,data=ip.data, family=binomial(link="logit"))
b <- coef(ip.glm.apc)
hi<- 97
lo<- 18
delta.rage <- with(ip.data, (invlogit(b[1] + b[2] * hi) - invlogit(b[1] + b[2] * lo)))
print(mean(delta.rage)) # -0.7178686 

# glsocdist
summary(ip.data$glsocdist)
ip.glm.apc <- glm(ssexmarr~ glsocdist ,data=ip.data, family=binomial(link="logit"))
b <- coef(ip.glm.apc)
hi<- 11
lo<- 0
delta.glsocdist <- with(ip.data, (invlogit(b[1] + b[2] * hi) - invlogit(b[1] + b[2] * lo)))
print(mean(delta.glsocdist)) # -0.73 

# persinc2
summary(ip.data$persinc2)
ip.glm.apc <- glm(ssexmarr~ persinc2 ,data=ip.data, family=binomial(link="logit"))
b <- coef(ip.glm.apc)
hi<- 75000
lo<- 260
delta.persinc2 <- with(ip.data, (invlogit(b[1] + b[2] * hi) - invlogit(b[1] + b[2] * lo)))
print(mean(delta.persinc2)) # + 0.16

### --------------------------------------------------------------------------------------------

# CENTERING (if continuous variable's y-intercept doesn't make sense)
# rage
cent.rage<-with(ip.data, (rage-mean(rage)))
cent.rage.glm<-glm(ssexmarr~cent.rage, data=ip.data, family = binomial(link="logit"))
display(cent.rage.glm) # y-intercept shows avg age has 0.64 prob to say yes to ssexmarr


### --------------------------------------------------------------------------------------------

# DATA MANIPULATION
# 1. cont to categorical: glvis
# try turning into factor and check level counts
ip.data$factor_glvis <- factor(ip.data$glvis)
summary(ip.data$factor_glvis)
# try merging levels
#ip.data$factor_glvis[ip.data$factor_glvis == 3] <- 2
#ip.data$factor_glvis[ip.data$factor_glvis == 2] <- 1

# 2. ansseca
#ip.data$ansseca[ip.data$ansseca == 10] <- NA
#ip.data$ansseca[ip.data$ansseca == 9] <- NA
#ip.data$ansseca <- droplevels(ip.data$ansseca)

# 3. rmarstat - not significant but increased significance
summary(ip.data$rmarstat)
ip.data$rmarstat[ip.data$rmarstat == 3] <- 2

# 4. tenshort - already significant, increased significance
summary(ip.data$tenshort)
ip.data$tenshort[ip.data$tenshort == 7] <- 6
#ip.data$tenshort[ip.data$tenshort == 3] <- 2
#ip.data$tenshort[ip.data$tenshort == 5] <- 6

# 5. highqual - was not significant, made it very close to significant 0.06
summary(ip.data$highqual)
ip.data$highqual[ip.data$highqual == 7] <- 6
ip.data$highqual[ip.data$highqual == 5] <- 4
ip.data$highqual[ip.data$highqual == 2] <- 1

# 6. tea - was not significant, still not but changed by a lot
ip.data$tea[ip.data$tea == 8] <- 6
ip.data$tea[ip.data$tea == 7] <- 6
ip.data$tea[ip.data$tea == 3] <- 2
ip.data$tea[ip.data$tea == 2] <- 1
ip.data$tea[ip.data$tea == 4] <- 1

# 7. religcat
ip.data$religcat[ip.data$religcat == 1] <- 2

# 8. famrelig
ip.data$famrelig[ip.data$famrelig == 1] <- 2

# 9. chattnd2
ip.data$chattnd2[ip.data$chattnd2 == 6] <- 7
ip.data$chattnd2[ip.data$chattnd2 == 2] <- 1
ip.data$chattnd2[ip.data$chattnd2 == 4] <- 3
ip.data$chattnd2[ip.data$chattnd2 == 5] <- 7

# 10. orient
summary(ip.data$orient)
ip.data$orient[ip.data$orient == 3] <- 2
ip.data$orient[ip.data$orient == 4] <- 2

# 11. polpart2 - was not significant - MERGING MADE SIGNIFICANT
summary(ip.data$polpart2)
ip.data$polpart2[ip.data$polpart2 == 3] <- 1
ip.data$polpart2[ip.data$polpart2 == 5] <- 6
ip.data$polpart2[ip.data$polpart2 == 7] <- 6
ip.data$polpart2[ip.data$polpart2 == 2] <- 6

# try converting to binary, made it less significant
#ip.data$binary_polpart2 <- ifelse(ip.data$polpart2!=4,0,1 )

# 12. ruhappy - deviance decrease was too much
summary(ip.data$ruhappy)
#ip.data$ruhappy[ip.data$ruhappy == 2] <- 1
#ip.data$ruhappy[ip.data$ruhappy == 4] <- 3 # significant of level 4 increase a lot, but model variance decreased by around 10 points

# 13. healthyr 0.18 -> 0.11 -> 0.04 MADE IT SIGNIFICANT
summary(ip.data$healthyr)
ip.data$healthyr[ip.data$healthyr == 5] <- 4
ip.data$healthyr[ip.data$healthyr == 4] <- 3
ip.data$healthyr[ip.data$healthyr == 2] <- 3


# 14. uprejgay 0.0002467 - didn't increase significance
summary(ip.data$uprejgay)
#ip.data$uprejgay[ip.data$uprejgay == 2] <- 1

# 15. glchild 0.0051119 -> 0.0020282
summary(ip.data$glchild)
# ip.data$glchild[ip.data$glchild == 5] <- 4
ip.data$glchild[ip.data$glchild == 2] <- 1

# 16. work 0.0002736
summary(ip.data$work)
ip.data$work[ip.data$work == 4] <- 3

# 17. rsect
summary(ip.data$rsect)
ip.data$rsect[ip.data$rsect == 4] <- 3

# convert continuous persinc2 to categorical
#summary(ip.data$persinc2)
#breaks <- c(-Inf, 5000, 10000,15000,20000, Inf)
#labels <- c("1", "2", "3", "4", "5")
#ip.data$factor_persinc2 <- cut(ip.data$persinc2, breaks = breaks, labels = labels, include.lowest = TRUE)
#ip.data$factor_persinc2 <- factor(ip.data$factor_persinc2, levels = c("1", "2", "3", "4","5", "missing"))
#ip.data$factor_persinc2[is.na(ip.data$persinc2)] <- "missing"
#levels(ip.data$factor_persinc2)
#summary(ip.data$factor_persinc2)

### --------------------------------------------------------------------------------------------

# ALL VARIABLES MODEL
ip.glm.0<-glm(ssexmarr~. ,data=ip.data, family=binomial(link="logit"))
display(ip.glm.0)
#vif(ip.glm.0)
alias(ip.glm.0)

# FIRST MODEL ( -persinc2- glvis)
ip.glm<-glm(ssexmarr~.-persinc2-glvis ,data=ip.data, family=binomial(link="logit"))
display(ip.glm) # difference = 452.7, 51.2, 50.7, 449.6, 448.8, 448.3, 448.1, 447.6, 446.8, 446.6, 445.6
summary(ip.glm)
vif(ip.glm)
alias(ip.glm)
anova(ip.glm.0,ip.glm,test="Chisq")
Anova(ip.glm) 

summary(ip.data$ansseca)
levels(ip.data$ansseca)
summary(ip.data$work)

# SECOND MODEL (after alias, vif)
ip.glm.2<-glm(ssexmarr~. -work -rsuper-persinc2- glvis,data=ip.data, family=binomial(link="logit"))
display(ip.glm.2) # difference 414.8, 3 variables removed, change should be 7.8, but its more
anova(ip.glm, ip.glm.2, test="Chisq")
summary(ip.glm.2)
vif(ip.glm.2)
### --------------------------------------------------------------------------------------------

# THIRD MODEL - non-significant continuous and binary vars removed (backward elimination)
ip.data <- ip.data[, !(names(ip.data) %in% c("persinc2"))]
ip.glm.3<-glm(ssexmarr~. -umineth-eqnow9-livearea-anyhcond -knowgl - intwww  -religcat - eqnow3 -tunionsa -househld-orient-work-rsuper- glvis,data=ip.data, family=binomial(link="logit")) # difference 406.8 initially for k=73
display(ip.glm.3)
summary(ip.glm.3)
Anova(ip.glm.3)

# FOURTH MODEL - after non-significant categorical removed using anova() test
# went up to 489.1, 486.3, 485.4, 484.5, 483.7
ip.glm.4<-glm(ssexmarr~. - carehome- factor_glvis - knowtg- ansseca- hincpast -rmarstat-tenshort-eqnow11  -tea -umineth -eqnow9 - livearea -anyhcond - knowgl- intwww- religcat - eqnow3 -tunionsa-househld-orient - work-rsuper- glvis,data=ip.data, family=binomial(link="logit"))
display(ip.glm.4) # 424.4, 424.3, 424.2, 424.1, 423.9, 423.4, 422.9, 422.8, 421.5, 420.7, 412.1, 409.9, 409.3
summary(ip.glm.4)
anova(ip.glm.3, ip.glm.4, test="Chisq")
Anova(ip.glm.4)
vif(ip.glm.4)

# FINAL MODEL after anova() test, tried adding back aliased, try adding back non-significant to see if it will increase significance (one by one) 
# healthyr and rsect removed, nothing added back
ip.glm.7<-glm(ssexmarr~glborn+ glsocdist + glchild+ uprejgay+ healthyr+ruhappy+polpart2+ chattnd2+ rsect+eqnow8+ highqual+rsex+rage,data=ip.data, family=binomial(link="logit"))
ip.glm.final<-glm(ssexmarr~glborn+ glsocdist + glchild+ uprejgay+ruhappy+polpart2+ chattnd2+eqnow8+ highqual+rsex+rage,data=ip.data, family=binomial(link="logit"))
#ip.glm.final2<-glm(ssexmarr~factor_persinc2 + glborn+ glsocdist + glchild+ uprejgay+ruhappy+polpart2+ chattnd2+eqnow8+ highqual+rsex+rage,data=ip.data, family=binomial(link="logit"))
#anova(ip.glm.final2,ip.glm.final, test="Chisq")
anova(ip.glm.7,ip.glm.final, test="Chisq")
display(ip.glm.7)
display(ip.glm.final)
Anova(ip.glm.final)
summary(ip.glm.final)

# check odd ratios
coefficients <- coef(ip.glm.final)
or <- exp(coefficients)
print(or)

### --------------------------------------------------------------------------------------------
# INTERACTIONS
# CHECKING INTERACTIONS BETWEEN THE MOST 3 SIGNIFICANT PREDICTORS

# regression polpart2 and uprejgay - SIGNIFICANT
glm_combined.1 <- glm(ssexmarr ~ polpart2 * uprejgay, data = ip.data, family=binomial(link="logit"))
display(glm_combined.1)
Anova(glm_combined.1)
# regression polpart2 and glchild - NOT SIGNIFICANT
glm_combined.2 <- glm(ssexmarr ~ polpart2 * glchild, data = ip.data, family=binomial(link="logit"))
display(glm_combined.2)
Anova(glm_combined.2)
# regression glchild and uprejgay - NOT SIGNIFICANT
glm_combined.3 <- glm(ssexmarr ~ glchild  * uprejgay, data = ip.data, family=binomial(link="logit"))
display(glm_combined.3)
Anova(glm_combined.3)

ip.glm.final_int<-glm(ssexmarr~( polpart2 * uprejgay) + glborn+ glsocdist + glchild+ uprejgay+ruhappy+polpart2+ chattnd2+eqnow8+ highqual+rsex+rage,data=ip.data, family=binomial(link="logit"))
display(ip.glm.final_int)
anova(ip.glm.final, ip.glm.final_int, test="Chisq") # not significant, hence not added to the models

### --------------------------------------------------------------------------------------------

# PREDICTING - CONFUSION MATRIX
# precition function
ct.op<-function(predicted,observed){ #arguments
  #create the data frame  
  df.op<-data.frame(predicted=predicted,observed=observed)
  #create a table 
  op.tab<-table(df.op)
  #use the prop.table function to obtain the proportions we need:
  #those who were correctly predicted as 0 
  #@position 1,1 in the table of proportions
  obs0.tab<-round(prop.table(op.tab,2)[1,1],2)
  #those who were correctly predicted as 1
  #@position 2,2 in the table of proportions
  obs1.tab<-round(prop.table(op.tab,2)[2,2],2)
  #and put them under the table 
  op.tab<-rbind(op.tab,c(obs0.tab,obs1.tab))
  #name the rows
  rownames(op.tab)<-c("pred=0","pred=1","%corr")
  #name the columns
  colnames(op.tab)<-c("obs=0","obs=1")
  #return the table
  op.tab
}

# predict using final model
pred.ip.glm.final<-as.numeric(ip.glm.final$fitted.values>0.5)
ct.op(pred.ip.glm.final,ip.data$ssexmarr)


### --------------------------------------------------------------------------------------------

# CHANGE "MISSING" BACK TO NA AND COMPARE MODELS
# Loop through variables and check their levels (identify which ones have "missing" level)
for (var in non_continuous_columns) {  
  cat("Variable:", var, "\n")
  cat("Levels:", levels(ip.data[[var]]), "\n\n")
}

# change missing levels back to NA (only the ones used in model)
ip.data$glborn[ip.data$glborn == "missing" ] <- "NA"
ip.data$polpart2[ip.data$polpart2 == "missing" ] <- "NA"
ip.data$chattnd2[ip.data$chattnd2 == "missing" ] <- "NA"

# run regression again
NA.removed.glm<-glm(ssexmarr~glborn+ glsocdist + glchild+ uprejgay+ruhappy+polpart2+ chattnd2+eqnow8+ highqual+rsex+rage,data=ip.data, family=binomial(link="logit"))
display(ip.glm.final) # deviance difference 493.1
display(NA.removed.glm) # deviance difference 331.2
qchisq(0.95,3) # 7.8, but the difference is much bigger (about 130), so model got worse


### --------------------------------------------------------------------------------------------

# PREDICTING FOR LAY REPORT PERSONAS

amy_data <- data.frame(
  glborn = factor(1), 
  glsocdist = c(0),  
  glchild = factor(1),  
  uprejgay = factor(3),
  ruhappy = factor(1),
  polpart2 = factor(4),
  chattnd2 = factor(7),
  eqnow8 = factor(1),
  highqual = factor(1),
  rsex = factor(2),
  rage = c(25)
)

amy_probability <- predict(ip.glm.final, newdata = amy_data, type = "response")
print(amy_probability)

davis_data <- data.frame(
  glborn = factor(2), 
  glsocdist = c(5),  
  glchild = factor(5),  
  uprejgay = factor(1),
  ruhappy = factor(3),
  polpart2 = factor(1),
  chattnd2 = factor(1),
  eqnow8 = factor(2),
  highqual = factor(6),
  rsex = factor(1),
  rage = c(60)
)

davis_probability <- predict(ip.glm.final, newdata = davis_data, type = "response")
print(davis_probability)
