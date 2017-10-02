### Diabetic Patient Behavior Analysis 
#Here I will load SAS-formatted survey data from CDC's 2015 Behaviorial Risk Factor Surveillance System and write a 
#readable csv for analysis. Next, I will convert height and weight to meters and kilograms plus label body mass index level. 

filename <- "E:/CDC/LLCP2015XPT.zip"
if (!file.exists("LLCP2015.XPT")) {
  unzip(filename)
}

#turn off scientific notation
options(stringsAsFactors=F, scipen = 999)

pkg = 'Hmisc'
if (!require(pkg, character.only = TRUE)) {
  library(pkg, character.only = TRUE)
}

#write from brfss sas 
brfss <- sasxport.get("LLCP2015.XPT")
write.csv(brfss, file = "brfss2015.csv")

#read brfss 
brfss <- read.csv("brfss2015.csv", encoding = "UTF-8")



# Convert height and weight to meters and kilograms
library(dplyr)
# Height transformation
height <- as.data.frame(as.matrix(brfss$height3))
metric <- TRUE
max_weight <- 999

in_m_filter <- height %>%
  filter(V1 < 712) %>%
  mutate(feet = substring(V1, 1, 1)) %>%
  mutate(inches = substring(V1, 2, nchar(V1))) %>%
  mutate(meters = ifelse(inches > 12, NaN, (as.numeric(substring(V1, 2, nchar(V1))) + 
                                              as.numeric(substring(V1, 1, 1)) * 12) * 0.0254)) 

m_filter <- height %>%
  filter(V1 < 9999 & V1 >= 9000 & metric) %>%
  mutate(meters = as.numeric( substring(V1, 2,2)) + (as.numeric(substring(V1, 3, nchar(V1))) * 0.01))  %>%
  mutate(meters = replace(meters, meters == 0, NaN))

#meters 
height$meters <- m_filter$meters[match(height$V1, m_filter$V1)]

colnames(height) <- c("compressed", "meters")


height$meters <- ifelse(is.na(height$meters), in_m_filter$meters[match(height$compressed, in_m_filter$V1)],
                        m_filter$meters[match(height$compressed, m_filter$V1)])

# Weight transformation
weight <- as.data.frame(as.matrix(brfss$weight2))

kg_filter <- weight %>%
  filter(V1 < max_weight) %>%
  mutate(kg = V1 * 0.453592)

kg_sub_filter <- weight %>%
  filter(V1 < 9999 & metric) %>%
  mutate(kg = substring(V1, 2, nchar(V1))) 

#kilograms
weight$kg <- ifelse(weight$V1 < max_weight, kg_filter$kg[match(weight$V1, kg_filter$V1)]  , 
                    ifelse( weight$V1 < 9999 & metric,kg_sub_filter$kg[match(weight$V1, kg_sub_filter$V1)],NaN))

colnames(weight) <- c("pounds (lbs)", "kg")


bmi <- as.data.frame(round(as.numeric(weight$kg) / (as.numeric(height$meters)^2), 1))
colnames(bmi) <- "bmiCalc"


#BMI strict weight-class labels (Note: BRFSS classifiers are not as strict (i.e. 18.5 as underwt, 25 as normal, 30 as ovet))

bmi$Class[bmi$bmiCalc < 10 | bmi$bmiCalc < 200] <-  NA
bmi$Class[bmi$bmiCalc >= 10 & bmi$bmiCalc <= 14.9 ] <- "very severely underweight"
bmi$Class[bmi$bmiCalc >= 15 & bmi$bmiCalc <= 15.9] <- "severely underweight"
bmi$Class[bmi$bmiCalc >= 16 & bmi$bmiCalc <= 18.4] <- "underweight"
bmi$Class[bmi$bmiCalc >= 18.5 & bmi$bmiCalc <= 24.9] <- "normal"
bmi$Class[bmi$bmiCalc >= 25 & bmi$bmiCalc <= 29.9] <- "overweight"
bmi$Class[bmi$bmiCalc >= 30 & bmi$bmiCalc <= 34.9] <- "obese class I"
bmi$Class[bmi$bmiCalc >= 35 & bmi$bmiCalc <= 39.9] <- "obese class II"
bmi$Class[bmi$bmiCalc >= 40] <- "obese class III"

bmi$Class <- replace(bmi$Class, is.na(bmi$Class), "overweight")

#cbind bmi into "clean" survey variables of interest
brfss <- cbind(brfss, bmi)

# Has a doctor advised you to reduce your sodium or salt intake ? (i.e. To lower blood pressure)
brfss$dradvise <- factor(brfss$dradvise, levels = c(1,2), labels = c('yes', 'no'))
brfss$dradvise[brfss$dradvise == 7] <- NA
brfss$dradvise[brfss$dradvise == 9] <- NA  

# How many times have you been to the doctor for diabetes within 1 year ?
brfss$doctdiab <- as.numeric(brfss$doctdiab)
brfss$doctdiab[brfss$doctdiab == 88] <- 0
brfss$doctdiab[brfss$doctdiab == 77] <- NA
brfss$doctdiab[brfss$doctdiab == 99] <- NA   

#Other than regular job, do you exercise any ?
brfss$exerany2 <- factor(brfss$exerany2, levels = c(1,2), labels = c('yes',  'no'))
brfss$exerany2[brfss$exerany2 == 7] <- NA
brfss$exerany2[brfss$exerany2 == 9] <- NA 

#Assuming each individual 65 yrs or older has Medicare coverage 
brfss$x.age65yr <- factor(brfss$x.age65yr, levels = c(1,2), labels = c('18-64', '65+'))
brfss$x.age65yr[brfss$x.age65yr == 3] <- NA

#Types of exercises and physical activities and their frequencies amongst targeted individuals
brfss$exract11 <- factor(brfss$exract11, 
                         levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
                                    31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,
                                    58,59,60,61,62,63,64,66,67,68,69,71,72,73,74,75,76,98), 
                         labels = c('interactive video games', 'aerobics', 'backpacking', 'badminton', 'basketball', 
                                    'bicycling machine','bicycling', 'boating', 'bowling', 'boxing' , 'calisthenics', 
                                    'canoeing/rowing competition', 'carpentry','dancing', 'elliptical', 'fishing' , 
                                    'frisbee', 'gardening', 'golf w/ cart', 'golf w/o cart', 'handball', 'hiking' , 
                                    'hockey', 'horseback riding', 'deer hunting', 'quail hunting', 'inline skating' , 
                                    'jogging', 'lacrosse','mountain climbing' , 'mowing lawn' , 'paddleball' , 
                                    'painting' , 'pilates', 'racquetball','raking lawn', 'running', 'rock climbing', 
                                    'rope skipping', 'rowing machine', 'rugby', 'scuba diving', 'skateboarding' , 
                                    'ice/roller skating' , 'sledding/tobagganing' , 'snorkeling', 'snow blowing', 
                                    'snow shoveling', 'snow skiiing', 'snowshoeing', 'soccer', 'softball/baseball',
                                    'squash', 'stair master', 'stream fising', 'surfing', 'swimming', 'swimming in laps', 
                                    'table tennis', 'tai chi', 'tennis' , 'touch football' , 'volleyball', 'walking', 
                                    'waterskiing', 'weight lifting', 'wrestling', 'yoga' , 'childcare', 'farm work' , 
                                    'house cleaning/repair', 'karate/mma', 'wheelchair sports','yard work', 'other'))

brfss$exract11[brfss$exract11 == 77] <- NA
brfss$exract11[brfss$exract11 == 99] <- NA 

brfss$x.race <- factor(brfss$x.race, levels = c(1,2,3,4,5,6,7,8),
                       labels = c('white', 'black', 'native american', 'asian', 'asian/pacific islander',
                                  'other', 'other/multiracial', 'hispanic'))
brfss$x.race[brfss$x.race == 9] <- NA 

brfss$sex <- factor(brfss$sex, levels = c(1,2), labels = c('male', 'female')) #sex


dat <- as.data.frame(with(brfss, cbind(x.age65yr, doctdiab, dradvise ,exerany2, Class)))
dat <- cbind(dat, brfss$exract11, brfss$x.race, brfss$sex)
dat65 <- subset(dat, x.age65yr == 2) # subset 65 years and older 
