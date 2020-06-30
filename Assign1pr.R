setwd("C:/Users/rfons/OneDrive - Langara College/DANA 4830/Assignment 1")

master = read.csv("Data-screening.csv", header = TRUE)

##Creating a new dataset to work with the variables without risk of wipe something 
master2 = master
##Removing unnecessary columns
master2 <- master2[,- c(2:16)]


##Checking  variables type of the data frame with sapply
sapply(master2,class)

##Checking Gender column
table(master2$Gender)
#we can see here that we have 4 and LGBT that doesn't supposed to be here

##Fixing the Gender column and assigning labels
master2$Gender = factor(master2$Gender,
                        levels = c(1,2),
                        labels = c("Male","Female"))
summary(master2$Gender)

##Checking unique values in Age column
unique(master2$Age)

##Correcting strange values in Age column
master2$Age [master2$Age == ' '|master2$Age == '1'|master2$Age == '0'|
               master2$Age == 'X'|master2$Age =='1985'|master2$Age =='1x'|
               master2$Age == '242'|master2$Age == 'Test'|master2$Age =='211'|
               master2$Age == '1970']= NA

master2$Age [master2$Age == '16 years-old '] = 16
View(master2$Age)

##Changing datatype of Age column
master2$Age <- as.numeric(master2$Age)

##Checking education column
table(master2$Education)
unique(master2$Education)

##Fixing the Education column and assigning labels
master2$Education = factor(master2$Education,
                        levels = c(1,2,3),
                        labels = c("Schools","Graduate","Post-graduate"))
summary(master2$Education)

##Checking occupation column
table(master2$Occupation)
unique(master2$Occupation)

##Fixing the Education column factoring and assigning labels
master2$Occupation = factor(master2$Occupation,
                            levels = c(4,1,2,3),
                            labels = c("Other","Student","White collar workers","Blue collar workers" ))
summary(master2$Occupation)

##Checking Income column
table(master2$Income)
unique(master2$Income)

##Fixing the Income column factoring and assigning labels
master2$Income [master2$Income == 30000]=1
master2$Income = factor(master2$Income,
                           levels = c(1,2,3,4),
                           labels = c("< 10 million d","10-30 million d",
                                      "30-40 million d","> 40 million d"))
summary(master2$Income)

##Checking q1K column
table(master2$q1k)
unique(master2$q1k)

##Fixing the column  factoring and assigning labels
master2$q1k = factor(master2$q1k,
                        levels = c(5,1,2,3,4),
                        labels = c("No Idea","Rubber","Fossil Fuel","Paper","Food"))
summary(master2$q1k)

##Checking q2K column
table(master2$q2k)
unique(master2$q2K)

##Fixing the column q2k factoring and assigning labels
master2$q2k = factor(master2$q2k,
                     levels = c(4,1,2,3),
                     labels = c("No Idea","Plastic Straw","Water supply tube","Kid Toys"))
summary(master2$q2k)

##Checking q3K column
table(master2$q3k)
unique(master2$q3K)

##Fixing the column q3k  factoring and assigning labels
master2$q3k = factor(master2$q3k,
                     levels = c(0,1,2,3),
                     labels = c("No Idea","Plastic Straw","Water supply tube","Kid Toys"))
summary(master2$q2k)

##Checking q4k column
table(master2$q4k)
unique(master2$q4k)


##Fixing the column q4k factoring and assigning labels
master2$q4k = factor(master2$q4k,
                     levels = c(2,1),
                     labels = c("I don't know","I know"))
summary(master2$q4k)


##Q6K
##Checking Q6K column
table(master2$Q6K)
unique(master2$Q6K)
#we can see here that we have 3 that doesn't supposed to be here


##Fixing the column Q6K factoring and assigning labels
master2$Q6K = factor(master2$Q6K,
                     levels = c(2,1),
                     labels = c("I don't know","I know"))
summary(master2$Q6K)

##Q7K
##Checking Q7K column
table(master2$Q7K)
unique(master2$Q7K)
#we can see here that we have 3 that doesn't supposed to be here


##Fixing the column Q7K factoring and assigning labels
master2$Q7K = factor(master2$Q7K,
                     levels = c(4,1,2,3),
                     labels = c("No Idea","Recycled","Dumped at landfill site","Litter to the environment"))
summary(master2$Q7K)

##Q8K
##Checking Q8K column
table(master2$Q8K)
unique(master2$Q8K)

##Fixing the column Q8K factoring and assigning labels
master2$Q8K = factor(master2$Q8K,
                     levels = c(4,1,2,3),
                     labels = c("No Idea","Top 5","Top 10","Top 50"))
summary(master2$Q8K)

##Q9K
##Checking Q9K column
table(master2$Q9K)
unique(master2$Q9K)

##Fixing the column Q9K factoring and assigning labels
master2$Q9K = factor(master2$Q9K,
                     levels = c(1,2),
                     labels = c("Yes","No"))
summary(master2$Q9K)

##Q10
##Checking Q10C1
table(master2$Q10C1)
unique(master2$Q10C1)

##Fixing the column Q10C1 factoring and assigning labels
master2$Q10C1 = factor(master2$Q10C1,
                     levels = c(4,1,2,3),
                     labels = c("No Idea","Very Concerned","Concerned a bit","Not concerned"))
summary(master2$Q10C1)

##Checking Q10C2
table(master2$Q10C2)
unique(master2$Q10C2)

##Fixing the column Q10C2 factoring and assigning labels
master2$Q10C2 = factor(master2$Q10C2,
                       levels = c(4,1,2,3),
                       labels = c("No Idea","Very Concerned","Concerned a bit","Not concerned"))
summary(master2$Q10C2)

##Checking Q10C3
table(master2$Q10C3)
unique(master2$Q10C3)

##Fixing the column Q10C3 factoring and assigning labels
master2$Q10C3 = factor(master2$Q10C3,
                       levels = c(4,1,2,3),
                       labels = c("No Idea","Very Concerned","Concerned a bit","Not concerned"))
summary(master2$Q10C3)

##Checking Q10C4
table(master2$Q10C4)
unique(master2$Q10C4)

##Fixing the column Q10C4 factoring and assigning labels
master2$Q10C4 = factor(master2$Q10C4,
                       levels = c(4,1,2,3),
                       labels = c("No Idea","Very Concerned","Concerned a bit","Not concerned"))
summary(master2$Q10C4)

##Checking Q10C5
table(master2$Q10C5)
unique(master2$Q10C5)

##Fixing the column Q10C5 factoring and assigning labels
master2$Q10C5 = factor(master2$Q10C5,
                       levels = c(4,1,2,3),
                       labels = c("No Idea","Very Concerned","Concerned a bit","Not concerned"))
summary(master2$Q10C5)

##Checking Q10C6
table(master2$Q10C6)
unique(master2$Q10C6)

##Fixing the column Q10C6 factoring and assigning labels
master2$Q10C6 = factor(master2$Q10C6,
                       levels = c(4,1,2,3),
                       labels = c("No Idea","Very Concerned","Concerned a bit","Not concerned"))
summary(master2$Q10C6)


##Checking Q10C7
table(master2$Q10C7)
unique(master2$Q10C7)

##Fixing the column Q10C7 factoring and assigning labels
master2$Q10C7 = factor(master2$Q10C7,
                       levels = c(4,1,2,3),
                       labels = c("No Idea","Very Concerned","Concerned a bit","Not concerned"))
summary(master2$Q10C7)

##Q11
##Checking Q11C1
table(master2$Q11C1)
unique(master2$Q11C1)
#the 5 and 4's doesn't seem to belong here let's investigate futher
master$Q11C1[master$Q11C1 > 3]

##Fixing the column Q11C1 factoring and assigning labels
master2$Q11C1 = factor(master2$Q11C1,
                       levels = c(1,2,3),
                       labels = c("Agree","Neutral","Disagree"))
summary(master2$Q11C1)

##Checking Q11C2
table(master2$Q11C2)
unique(master2$Q11C2)
#the 5 and 4's doesn't seem to belong here let's investigate futher
#again five and 4 could it be a typo
master$Q11C2[master$Q11C2 > 3]

##Fixing the column Q11C2 factoring and assigning labels
master2$Q11C2 = factor(master2$Q11C2,
                       levels = c(1,2,3),
                       labels = c("Agree","Neutral","Disagree"))
summary(master2$Q11C2)

##Checking Q11C3
table(master2$Q11C3)
unique(master2$Q11C3)


##Fixing the column Q11C3 factoring and assigning labels
master2$Q11C3 = factor(master2$Q11C3,
                       levels = c(1,2,3),
                       labels = c("Agree","Neutral","Disagree"))
summary(master2$Q11C3)

##Checking Q11C4
table(master2$Q11C4)
unique(master2$Q11C4)


##Fixing the column Q11C4 factoring and assigning labels
master2$Q11C4 = factor(master2$Q11C4,
                       levels = c(1,2,3),
                       labels = c("Agree","Neutral","Disagree"))
summary(master2$Q11C4)

##Checking Q11C5
table(master2$Q11C5)
unique(master2$Q11C5)


##Fixing the column Q11C5 factoring and assigning labels
master2$Q11C5 = factor(master2$Q11C5,
                       levels = c(1,2,3),
                       labels = c("Agree","Neutral","Disagree"))
summary(master2$Q11C5)


##Checking Q14C
table(master2$Q14C)
unique(master2$Q14C)


##Fixing the column Q14C factoring and assigning labels
master2$Q14C = factor(master2$Q14C,
                       levels = c(1,2,3),
                       labels = c("Supportive","Neutral","Not Supportive"))
summary(master2$Q14C)


##Q17
##Checking Q17P1
table(master2$Q17P1)
unique(master2$Q17P1)

##Fixing the column Q17P1 factoring and assigning labels
master2$Q17P1 = factor(master2$Q17P1,
                      levels = c(4,1,2,3),
                      labels = c("No","Frequently","Sometimes","Rarely"))
summary(master2$Q17P1)

##Checking Q17P2
table(master2$Q17P2)
unique(master2$Q17P2)

##Fixing the column Q17P2 factoring and assigning labels
master2$Q17P2 = factor(master2$Q17P2,
                       levels = c(4,1,2,3),
                       labels = c("No","Frequently","Sometimes","Rarely"))
summary(master2$Q17P2)

##Checking Q17P3
table(master2$Q17P3)
unique(master2$Q17P3)

##Fixing the column Q17P3 factoring and assigning labels
master2$Q17P3 = factor(master2$Q17P3,
                       levels = c(4,1,2,3),
                       labels = c("No","Frequently","Sometimes","Rarely"))
summary(master2$Q17P3)

##Checking Q17P4
table(master2$Q17P4)
unique(master2$Q17P4)

##Fixing the column Q17P4 factoring and assigning labels
master2$Q17P4 = factor(master2$Q17P4,
                       levels = c(4,1,2,3),
                       labels = c("No","Frequently","Sometimes","Rarely"))
summary(master2$Q17P4)

##Checking Q17P5
table(master2$Q17P5)
unique(master2$Q17P5)

##Fixing the column Q17P5 factoring and assigning labels
master2$Q17P5 = factor(master2$Q17P5,
                       levels = c(4,1,2,3),
                       labels = c("No","Frequently","Sometimes","Rarely"))
summary(master2$Q17P5)

##Q18
##Checking Q18I1
table(master2$Q18I1)
unique(master2$Q18I1)

##Fixing the column Q18I1 factoring and assigning labels
master2$Q18I1 = factor(master2$Q18I1,
                       levels = c(6,1,2,3,4,5),
                       labels = c("No Idea","Total Disagreed","Disagreed","Neutral","Agreed","Total Agreed"))
summary(master2$Q18I1)

##Checking Q18I2
table(master2$Q18I2)
unique(master2$Q18I2)

##Fixing the column Q18I2 factoring and assigning labels
master2$Q18I2 = factor(master2$Q18I2,
                       levels = c(6,1,2,3,4,5),
                       labels = c("No Idea","Total Disagreed","Disagreed","Neutral","Agreed","Total Agreed"))
summary(master2$Q18I2)

##Checking Q18I3
table(master2$Q18I3)
unique(master2$Q18I3)

##Fixing the column Q18I3 factoring and assigning labels
master2$Q18I3 = factor(master2$Q18I3,
                       levels = c(6,1,2,3,4,5),
                       labels = c("No Idea","Total Disagreed","Disagreed","Neutral","Agreed","Total Agreed"))
summary(master2$Q18I3)

##Checking Q18I4
table(master2$Q18I4)
unique(master2$Q18I4)

##Fixing the column Q18I4 factoring and assigning labels
master2$Q18I4 = factor(master2$Q18I4,
                       levels = c(6,1,2,3,4,5),
                       labels = c("No Idea","Total Disagreed","Disagreed","Neutral","Agreed","Total Agreed"))
summary(master2$Q18I4)


##Dealing with scale columns (q5,q12,q13,q15,q16,q19 originally Q22)
summary(master2)
#we use summary to spot any out of range values in the columns

#Q5
summary(master2$Q5K1)
summary(master2$Q5K2)
summary(master2$Q5K3)
summary(master2$Q5K4)
summary(master2$Q5K6)
summary(master2$Q5K7)
#Didn't spot any out of range values

##q12 (q12c1, q12c2,q12c3,q12c4,q12,c5,q12c6,q12c8)
##Q12C1
summary(master2$Q12C1)
table(master2$Q12C1)
#There are some out of range values the maximum should be 6
#Spotting the datapoints with problems
master2[master2$Q12C1 > 6, "Q12C1" ]
#Correcting the out of range values
master2$Q12C1[master2$Q12C1 == '7']= NA

##Q12C2
summary(master2$Q12C2)
table(master2$Q12C2)
#There are some out of range values the maximum should be 6
#Spotting the datapoints with problems
master2[master2$Q12C2 > 6, "Q12C2" ]
#Correcting the out of range values
master2$Q12C2[master2$Q12C2 == '7']= NA
master2$Q12C2[master2$Q12C2 == '8']= NA

##Q12C3
summary(master2$Q12C3)
table(master2$Q12C3)
#There are some out of range values the maximum should be 6
#Spotting the datapoints with problems
master2[master2$Q12C3 > 6, "Q12C3" ]
#Correcting the out of range values
master2$Q12C3[master2$Q12C3 == '7']= NA
master2$Q12C3[master2$Q12C3 == '8']= NA

#Q12C4
summary(master2$Q12C4)
table(master2$Q12C4)
#There are some out of range values the maximum should be 6
#Spotting the datapoints with problems
master2[master2$Q12C4 > 6, "Q12C4" ]
#Correcting the out of range values
master2$Q12C4[master2$Q12C4 == '7']= NA

##Q12C5
summary(master2$Q12C5)
table(master2$Q12C5)
#There are some out of range values the maximum should be 6
#Spotting the datapoints with problems
master2[master2$Q12C5 > 6, "Q12C5" ]
#Correcting the out of range values
master2$Q12C5[master2$Q12C5 == '7']= NA
master2$Q12C5[master2$Q12C5 == '8']= NA

##Q12C6
summary(master2$Q12C6)
table(master2$Q12C6)
#There are some out of range values the maximum should be 6
#Spotting the datapoints with problems
master2[master2$Q12C6 > 6, "Q12C6" ]
#Correcting the out of range values
master2$Q12C6[master2$Q12C6 == '7']= NA
master2$Q12C6[master2$Q12C6 == '8']= NA

##Q12C8
##This option don't exist in the questionnaire I'll drop this variable
master2 = subset(master2, select = -c(Q12C8))

#Q13(Q13C1,Q13C2,Q13C3,Q13C4,Q13C5,Q13C6)
summary(master2$Q13C1)
summary(master2$Q13C2)
summary(master2$Q13C3)
summary(master2$Q13C4)
summary(master2$Q13C5)

##there's a out of range value here 7
master2[master2$Q13C5 > 6, "Q13C5"]
master2$Q13C5[master2$Q13C5 == '7'] = NA


summary(master2$Q13C6)
##there's a out of range value here 7
master2[master2$Q13C6 > 6, "Q13C6"]
master2$Q13C6[master2$Q13C6 == '7'] = NA


#Q15
summary(master2$Q15C1)
summary(master2$Q15C2)
summary(master2$Q15C3)
summary(master2$Q15C4)
summary(master2$Q15C5)
summary(master2$Q15C6)
#Didn't spot any out of range values all of them are ones or blank spaces

#Q16
summary(master2$Q16P1)
summary(master2$Q16P2)
summary(master2$Q16P3)
summary(master2$Q16P4)
summary(master2$Q16P5)
#Didn't spot any out of range values all of them are ones or blank spaces


##Q19
##In the original dataset question 19 is wrongly named as question22 so first of all I'll rename the column

names(master2)[names(master2) == "Q22C1"] <- "Q19C1"
names(master2)[names(master2) == "Q22C2"] <- "Q19C2"
names(master2)[names(master2) == "Q22C3"] <- "Q19C3"
names(master2)[names(master2) == "Q22C4"] <- "Q19C4"
names(master2)[names(master2) == "Q22c5"] <- "Q19C5"
names(master2)[names(master2) == "Q22c6"] <- "Q19C6"
master2

#Q19(Q19C1,Q19C2,Q19C3,Q19C4,Q19C5,Q19C6)
summary(master2$Q19C1)
summary(master2$Q19C2)
summary(master2$Q19C3)
summary(master2$Q19C4)
summary(master2$Q19C5)
summary(master2$Q19C6)

##Dealing with Missing Values

summary(master2)
#Several NA's
#Look at the columns
names(master2)
#Categorical variables
##IDrespond ,Gender,Education, Occupation,Income,q1k,q2k,q3k,q4k,Q6K,
##Q7K,Q8K,Q9K,Q10C1,Q10C2,Q10C3,Q10C4,Q10C5,Q10C6,Q10C7,Q11C1,Q11C2,Q11C3,Q11C4,Q14C,Q11C5,Q17P1,
##Q17P2,Q17P3,Q17P4,Q17P5,Q18I1,Q18I2,Q18I3,Q18I4))

#Writing a function to return me the NA values quantity
percentmiss = function(x){sum(is.na(x))/length(x)*100}

##columns
apply(master2[,-c(1,2,4,5,6,7,8,9,10,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,46,
                  58,59,60,61,62,63,64,65,66)],2,percentmiss)
##rows
missing = apply(master2[,-c(1,2,4,5,6,7,8,9,10,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,46,
                            58,59,60,61,62,63,64,65,66)],1,percentmiss)
summary(missing)
table(missing)

unfactoredcolumns = subset(master2, select = c(34,35,36,37,38,39,40,41,67,68,69,70,71,72))

library(mice)
tempnomiss = mice(unfactoredcolumns)
View(unfactoredcolumns)
summary(unfactoredcolumns)
summary(tempnomiss)

#turning that list into a dataset
nomiss = complete(tempnomiss,1)
summary(nomiss)


# Add datasets 

newmaster = cbind(master2, nomiss)

#Drop duplicated columns
newmaster <- newmaster[,- c(34,35,36,37,38,39,40,41,67,68,69,70,71,72)]
names(newmaster)
# Reordering columns
newmaster[, c("IDrespond","Gender","Age","Education","Occupation","Income","q1k","q2k","q3k","q4k","Q5K1","Q5K2",
              "Q5K3","Q5K4","Q5K5","Q5K6","Q5K7","Q6K","Q7K","Q8K","Q9K","Q10C1","Q10C2","Q10C3","Q10C4","Q10C5",
             "Q10C6","Q10C7","Q11C1","Q11C2", "Q11C3", "Q11C4","Q11C5","Q12C1","Q12C2","Q12C3","Q12C4","Q12C5",
             "Q12C6","Q13C1","Q13C2","Q13C3","Q13C4","Q13C5","Q13C6","Q14C","Q15C1", "Q15C2","Q15C3","Q15C4","Q15C5",
             "Q15C6","Q16P1","Q16P2","Q16P3","Q16P4","Q16P5","Q17P1","Q17P2","Q17P3","Q17P4","Q17P5","Q18I1", "Q18I2",
             "Q18I3","Q18I4","Q19C1","Q19C2","Q19C3","Q19C4","Q19C5","Q19C6")]
View(newmaster)


## Misssing values in dummy variables

#Q5
newmaster$Q5K1[is.na(newmaster$Q5K1)] <- 0
newmaster$Q5K2[is.na(newmaster$Q5K2)] <- 0
newmaster$Q5K3[is.na(newmaster$Q5K3)] <- 0
newmaster$Q5K4[is.na(newmaster$Q5K4)] <- 0
newmaster$Q5K5[is.na(newmaster$Q5K5)] <- 0
newmaster$Q5K6[is.na(newmaster$Q5K6)] <- 0
newmaster$Q5K7[is.na(newmaster$Q5K7)] <- 0

#Q15
newmaster$Q15C1[is.na(newmaster$Q15C1)] <- 0
newmaster$Q15C2[is.na(newmaster$Q15C2)] <- 0
newmaster$Q15C3[is.na(newmaster$Q15C3)] <- 0
newmaster$Q15C4[is.na(newmaster$Q15C4)] <- 0
newmaster$Q15C5[is.na(newmaster$Q15C5)] <- 0
newmaster$Q15C6[is.na(newmaster$Q15C6)] <- 0

#Q16
newmaster$Q16P1[is.na(newmaster$Q16P1)] <- 0
newmaster$Q16P2[is.na(newmaster$Q16P2)] <- 0
newmaster$Q16P3[is.na(newmaster$Q16P3)] <- 0
newmaster$Q16P4[is.na(newmaster$Q16P4)] <- 0
newmaster$Q16P5[is.na(newmaster$Q16P5)] <- 0

summary(newmaster)

#missing values in Age column
# Replace with mean
newmaster$Age[is.na(newmaster$Age)] <- 30.4

summary(newmaster)

##Outliers
##mahalanobis

mahalist
newmaster[, c(3,34,35,36,37,39,40,41,67,68,69,70,71,72)]
newmastermahal = mahalanobis(newmaster[, c(3,34,35,36,37,39,40,41,67,68,69,70,71,72)],
                             colMeans(newmaster[, c(3,34,35,36,37,39,40,41,67,68,69,70,71,72)],na.rm = TRUE),
                             cov(newmaster[, c(3,34,35,36,37,39,40,41,67,68,69,70,71,72)], use= "pairwise.complete.obs"))

summary(newmastermahal)
#Chi-square
ctoff = qchisq(.999, ncol(newmaster[, c(3,34,35,36,37,39,40,41,67,68,69,70,71,72)]))
summary(newmastermahal < ctoff)

#Solving outliers
noout = newmaster[newmastermahal < ctoff, ]

##assumptions
cor(noout[,c(3,34,35,36,37,39,40,41,67,68,69,70,71,72)], use = "pairwise.complete.obs")
correlations =cor(noout[,c(3,34,35,36,37,39,40,41,67,68,69,70,71,72)], use = "pairwise.complete.obs")
symnum(correlations)

install.packages("corrplot")
library(corrplot)
install.packages("PerformanceAnalytics")

library("PerformanceAnalytics")
noout<-noout[, c(3,34,35,36,37,39,40,41,67,68,69,70,71,72)]
chart.Correlation(noout, histogram=TRUE, pch=19)

#exporting the dataset
write.table(newmaster,file="DataScreeningassign.csv",sep =",")


