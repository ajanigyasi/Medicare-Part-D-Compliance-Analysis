#########################################################Attach imputations for missing data
library(plyr)
t <- dat65
t <- t[,-c(1)]


colnames(t) <- c("drVisitDiab", "drLowSalt", "exerAny", "bmiClass", "exerActType", "race", "sex")
#Change yes and no levels
levels(t$exerAny) <- c(1,0)
levels(t$drLowSalt) <- c(1,0)
#Reorder levels & remove age column
t$bmiClass <- factor(t$bmiClass, levels(t$bmiClass)[c(8,6,7,1,5,2:4)])

t <- subset(t, !is.na(drVisitDiab))
levels(t$drVisitDiab)[c(43,45,47)] <- c(NA, 0, NA) # per BRFSS Codebook

#NA's
#race 335 
#exerAny 1233
#drLowSalt 13,885
#drVisitDiab 769

# Convert drVisistDiab to numeric safely
t$drVisitDiab <- as.numeric(levels(t$drVisitDiab))[t$drVisitDiab]

library(mice) # To impute exerAny, drLowSalt, & race
imp <- mice( t[,c(2:3, 6)] , seed = 123)
comp <- complete(imp)

t <- with(t, cbind(drVisitDiab,  comp, sex, bmiClass, exerActType))

average_wt <- ddply(t, c('race', 'sex', 'bmiClass'), function (x) c(drVisitDiab = round(mean(x$drVisitDiab, na.rm = TRUE))))

#Impute function
impute <- function (drVisitDiab, race, sex , bmiClass) {
  fill <- NA
  if (!is.na(drVisitDiab)) {
    fill <- c(drVisitDiab)
  }
  else {
    fill <- average_wt[average_wt$race == race & average_wt$sex == sex & average_wt$bmiClass == bmiClass, "drVisitDiab"]
  }
}

# To impute drVisitDiab using averages based on race, sex, and bmiClass
t$drVisitDiab <- with(t, mapply(impute, drVisitDiab, race, sex, bmiClass ))

dataHealth <- t

write.csv(dataHealth, file = "dataHealth.csv")
