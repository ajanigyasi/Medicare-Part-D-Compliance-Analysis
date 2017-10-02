#################################################################################### Exercise Cloud Visualization
library(plyr)
library(dplyr)
library(wordcloud2)
library(wesanderson)

dataHealth <- read.csv("dataHealth.csv", colClasses = c(drVisitDiab = "integer", drLowSalt = "factor", 
                                                        exerAny = "factor", race = "factor", sex = "factor",  
                                                        bmiClass = "factor", exerActType = "factor" ))

# Obtain exercise type frequencies among weights classes
normal <- subset(dataHealth, bmiClass == "normal")
normalPrimExer <- plyr::count(normal, 'exerActType')
colnames(normalPrimExer) <- c("exercise", "freq")
normalPrimExer <- normalPrimExer %>% filter(!is.na(exercise)) %>% arrange(desc(freq))
normalPrimExer$freq <- log(normalPrimExer$freq)
wordcloud2(normalPrimExer, size = 0.50,color=wes_palette("Darjeeling"), shape = 'diamond', rotateRatio = 0)

overweight <- subset(dataHealth, bmiClass == "overweight")
overPrimExer <- plyr::count(overweight,  'exerActType')
colnames(overPrimExer) <- c("exercise", "freq")
overPrimExer <- overPrimExer %>% filter(!is.na(exercise)) %>% arrange(desc(freq))
overPrimExer$freq <- log(overPrimExer$freq)
wordcloud2(overPrimExer, size = 0.50, color=wes_palette("Rushmore"), shape = "diamond",rotateRatio = 0)

obese <- subset(dataHealth, bmiClass == "obese class I")
obesePrimExer <- plyr::count(obese, 'exerActType')
colnames(obesePrimExer) <- c("exercise", "freq")
obesePrimExer <- obesePrimExer %>% filter(!is.na(exercise)) %>% arrange(desc(freq))
obesePrimExer$freq <- log(obesePrimExer$freq)
wordcloud2(obesePrimExer, size = 0.50, color=wes_palette("Royal1"), shape = "diamond",rotateRatio = 0)

obese2 <- subset(dataHealth, bmiClass == "obese class II")
obesePrimExer2 <- plyr::count(obese2, 'exerActType')
colnames(obesePrimExer2) <- c("exercise", "freq")
obesePrimExer2 <- obesePrimExer2 %>% filter(!is.na(exercise)) %>% arrange(desc(freq))
obesePrimExer2$freq <- log(obesePrimExer2$freq)
wordcloud2(obesePrimExer2, size = 0.50, color=wes_palette("Chevalier"), shape = "diamond",rotateRatio = 0)

obese3 <- subset(dataHealth, bmiClass == "obese class III")
obesePrimExer3 <- plyr::count(obese3, 'exerActType')
colnames(obesePrimExer3) <- c("exercise", "freq")
obesePrimExer3 <- obesePrimExer3 %>% filter(!is.na(exercise)) %>% arrange(desc(freq))
obesePrimExer3$freq <- log(obesePrimExer3$freq)
wordcloud2(obesePrimExer3, size = 0.50, color=wes_palette("GrandBudapest2"), shape = "diamond",rotateRatio = 0)


library(png)
library(grid)
library(gridExtra)
rl = lapply(sprintf(c("normalExercise.png", "overweightExercise.png", "obeseExercise.png")), readPNG)
gl = lapply(rl, rasterGrob)
vp = viewport(height=unit(0.4, "npc"), width=unit(7, "inches"))
grid.arrange(arrangeGrob(grobs = gl[1], top = textGrob("Normal Weight Exercise"), vp=vp), 
             arrangeGrob(grobs = gl[2], top = textGrob("Overweight Exercise"), vp=vp),      
             arrangeGrob(grobs = gl[3], top = textGrob("Obese Class I Exercise"), vp=vp),  ncol = 3)
