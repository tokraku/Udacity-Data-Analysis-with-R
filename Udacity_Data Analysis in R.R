#Subsetting
statesInfo <- read.csv("stateData.csv")
stateSubset <- subset(statesInfo, state.region == 1)
#bracket method
stateBracketSubset <- statesInfo[statesInfo$state.region == 1, ]
stateSubset
stateBracketSubset

MurderOverTen <- subset(statesInfo, murder > 10)
MurderOverTen

IlliteracyOver2 <- subset(statesInfo, illiteracy > 2)
IlliteracyOver2

install.packages('knitr', dependencies = T) 
library(knitr)

#reddit
setwd("/Users/Therese/Downloads")
reddit <- read.csv("reddit.csv")
str(reddit)

levels(reddit$age.range)
library(ggplot2)
qplot(reddit$age.range)

#ordered factor = age and income
qplot(data=reddit, x=income.range)
#order varables

#data munging
#fake facebook data
pf <- read.csv('pseudo_facebook.tsv', sep = '\t')
names(pf)
ggplot(data = pf, binwidth =25, aes(x = dob_day)) + 
  geom_histogram(binwidth = 1) + 
  scale_x_continuous(breaks = 1:31) + 
  facet_wrap(~dob_month)

ggplot(data = pf, x = dob_day, bin=25) + 
  scale_x_continuous(breaks = 1:31) + 
  facet_wrap(~gender)

table(pf$gender)
#summary
by(pf$friend_count, pf$gender, summary)


friendCount <- pf$friend_count
qplot(friendCount, bins=10)
qplot(data = pf, x = friend_count)

qplot(x= tenure, data=pf, 
      color = I('black'), fill = I("#099DD9"))


qplot(x= tenure/365, data=pf, binwidth=1,
      ylab = "Number of users in sample",
      xlab = "Number of years using sample",
      color = I('black'), fill = I("#099DD9"))

ggplot(aes(x = tenure), data = pf) + 
  geom_histogram(binwidth = 30, color = 'black', fill = '#099DD9')

Age <- ()
qplot(data = pf, x=age, binwidth = 1, 
      ylab= "Number of users in sample",
      xlab= "Age") +
  scale_x_continuous(breaks = seq(0, 113, 5))

install.packages("gridExtra")
library(gridExtra)

p1 <- qplot(x=friend_count, data=pf)
p1
p2 <- qplot(x=log10(friend_count +1), data=pf)
p2
p3 <- qplot(x= sqrt(friend_count), data=pf)
p3

grid.arrange(p1, p2, p3, ncol =1)

p1 <- ggplot(aes(x= friend_count), data = pf) + geom_histogram()
p2 <- p1 + scale_x_log10()
p3 <- p1 + scale_x_sqrt()

#Add a Scaling Layer
logscale <- qplot(x= log10(friend_count), data = pf)
countScale <- ggplot(aes(x= friend_count), data = pf) + geom_histogram() + scale_x_log10()
grid.arrange(logscale, countScale, ncol = 2)