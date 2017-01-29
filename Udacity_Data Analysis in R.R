#Created 01.28.17 by Therese Kennelly Okraku
#Last edited 01.29.17 by Therese Kennelly Okraku

#Udacity's Data Analysis with R course

#Lesson 2: Review of R basics
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

#Import reddit survey dataset
setwd("/Users/Therese/Downloads")
reddit <- read.csv("reddit.csv")
str(reddit)

levels(reddit$age.range)
library(ggplot2)
qplot(reddit$age.range)

#ordered factor = age and income
qplot(data=reddit, x=income.range)
#order varables

#Lesson 3: Explore one variable
#Import fake facebook data
pf <- read.csv('pseudo_facebook.tsv', sep = '\t')
names(pf)
ggplot(data = pf, binwidth =25, aes(x = dob_day)) + 
  geom_histogram(binwidth = 1) + 
  scale_x_continuous(breaks = 1:31) + 
  facet_wrap(~dob_month)

ggplot(data = pf, x = dob_day, binwidth=25) + 
  scale_x_continuous(breaks = 1:31) + 
  facet_wrap(~gender)

table(pf$gender)
#summary
by(pf$friend_count, pf$gender, summary)

friendCount <- pf$friend_count
qplot(friendCount, binwidth=10)
qplot(data = pf, x = friend_count)

qplot(x= tenure, data=pf, 
      color = I('black'), fill = I("#099DD9"))

qplot(x= tenure/365, data=pf, binwidth=1,
      ylab = "Number of users in sample",
      xlab = "Number of years using sample",
      color = I('black'), fill = I("#099DD9"))

ggplot(aes(x = tenure), data = pf) + 
  geom_histogram(binwidth = 30, color = 'black', fill = '#099DD9')

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
logScale <- qplot(x= log10(friend_count), data = pf)
countScale <- ggplot(aes(x= friend_count), data = pf) + geom_histogram() + scale_x_log10()
grid.arrange(logScale, countScale, ncol = 2)

qplot(x = friend_count, data = pf) + scale_x_log10()
help("facet_wrap")

#Frequency polygons
qplot(x = friend_count, data = subset(pf, !is.na(gender)),
      binwidth = 10 +
        scale_x_continuous(lim = c(0, 1000), breaks = seq(0, 1000, 50)) + 
        facet_wrap(~gender))
      
qplot(x = friend_count, y = ..count../sum(..count..), 
      data = subset(pf, !is.na(gender)),
      xlab = "Friend count",
      ylab = "Proportion of users with that friend count",
      binwidth = 10, geom = "freqpoly", color = gender) +
      scale_x_continuous(lim = c(0, 1000), breaks = seq(0, 1000, 50))

ggplot(aes(x = friend_count, y = ..count../sum(..count..)), data = subset(pf, !is.na(gender))) + 
  geom_freqpoly(aes(color = gender), binwidth=10) + 
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) + 
  xlab('Friend Count') + 
  ylab('Percentage of users with that friend count')

#Likes by gender
table(pf$www_likes, pf$gender)
#summary
by(pf$www_likes, pf$gender, summary)
by(pf$www_likes, pf$gender, sum)

#Box plots
qplot(x=gender, y=friend_count, 
      data = subset(pf, !is.na(gender)),
      geom = 'boxplot')
#with limits
qplot(x=gender, y=friend_count, 
      data = subset(pf, !is.na(gender)),
      geom = 'boxplot', ylim = c(0,1000))

qplot(x=gender, y=friend_count, 
      data = subset(pf, !is.na(gender)),
      geom = 'boxplot') +
      scale_y_continuous(limits = c(0,1000))
#better solution bc it doesn't remove outliers
qplot(x=gender, y=friend_count, 
      data = subset(pf, !is.na(gender)),
      geom = 'boxplot') +
  coord_cartesian(ylim = c(0,1000))

qplot(x=gender, y=friend_count, 
      data = subset(pf, !is.na(gender)),
      geom = 'boxplot') +
  coord_cartesian(ylim = c(0,250))

by(pf$friend_count, pf$gender, summary)

#gender and friendships initiated
qplot(x=gender, y=friendships_initiated, 
      data = subset(pf, !is.na(gender)),
      geom = 'boxplot') +
  coord_cartesian(ylim = c(0,150))

#Logic - How many users checked in on mobile?
summary(pf$mobile_likes)
summary(pf$mobile_likes > 0)

mobile_check_in <- NA
pf$mobile_check_in <- ifelse(pf$mobile_likes > 0, 1, 0)
pf$mobile_check_in <- factor(pf$mobile_check_in)
summary(pf$mobile_check_in)

#% of users that used check-in feature
63947/ (63947 + 35056)
sum(pf$mobile_check_in == 1)/length(pf$mobile_check_in)

#Lesson 4
#Diamonds
data(diamonds)
str(diamonds)
table(diamonds$color)

#create histogram of the price of all the diamonds
qplot(data= diamonds, x=price)
summary(diamonds$price)

#less than $500
lessthan500 <- subset(diamonds, price < 500)
str(lessthan500)

#less than $250
lessthan250 <- subset(diamonds, price < 250)
lessthan250
str(lessthan250)

#equal or more than $15,000
morethan15000 <- subset(diamonds, price >= 15000)
morethan15000
str(morethan15000)

#histogram of price by carat
qplot(carat, price, data=diamonds)

#adjust the binwidth
qplot(price, data=diamonds, geom="histogram")
qplot(price, data=diamonds, geom="histogram", binwidth=10)
qplot(price, data=diamonds, geom="histogram", binwidth=5)
qplot(price, data=diamonds, geom="histogram", binwidth=500)
qplot(price, data=diamonds, geom="histogram", binwidth=100)

#zoom
qplot(price, data=diamonds,
        geom="histogram", xlim=c(0, 5000))

#Explore the largest peak in the price histogram
qplot(x = price, data = diamonds)
qplot(x = price, data = diamonds, binwidth=1000)
ggsave('priceHistogram.png')

qplot(price, data=diamonds,
      geom="histogram", xlim=c(0, 5000))
qplot(price, data=diamonds,
      geom="histogram", xlim=c(500, 1500))
qplot(price, data=diamonds,
      geom="histogram", xlim=c(500, 1500), binwidth=1)

str(diamonds)
qplot(cut, data=diamonds)
ggsave('cutHistogram.png')

qplot(y=price, x=cut, data=diamonds, geom="histogram")
ggsave('pricebycutHistogram.png')

qplot(x = price, data = diamonds) + facet_wrap(~cut)

#summary of prices of diamonds by cut
by(diamonds$price, diamonds$cut, summary)
#to solve rounding issue look at max
help("max")
by(diamonds$price, diamonds$cut, max)

#adjust facet wrap for free scales
help("facet_wrap")
qplot(x = price, data = diamonds) + facet_wrap(~cut, scales = "free")

# Create a histogram of price per carat
# and facet it by cut. You can make adjustments
# to the code from the previous exercise to get started.

by(diamonds$price, diamonds$carat, summary)
qplot(y=carat, x=price, data=diamonds)
qplot(y=carat, x=price, data=diamonds, xlim=c(500, 1500)

#Price by color box plots
qplot(x=color, y=price, 
      data = subset(diamonds, !is.na(color)),
      geom = 'boxplot')
#with limits
qplot(x=color, y=price, 
      data = subset(diamonds, !is.na(color)),
      geom = 'boxplot', ylim = c(500,5000))

#price range by color range
by(diamonds$price, diamonds$color, summary)

#Interquartile range (IQR)
help(IQR)
IQR(subset(diamonds, price <1000)$price)
IQR(subset(diamonds, color = "J")$color)
IQR(subset(diamonds, color = "D")$color)

# Investigate the price per carat of diamonds across
# the different colors of diamonds using boxplots.
ppc <- diamonds$price/diamonds$carat
by(ppc, diamonds$color, summary)

qplot(x=color, y=ppc, 
      data = subset(diamonds, !is.na(color)),
      geom = 'boxplot')

qplot(x=color, y=ppc, 
      data = subset(diamonds, !is.na(color)),
      geom = 'boxplot', ylim = c(1000,4000))

#Frequency polygons
qplot(x = carat, data = diamonds, geom = "freqpoly")
qplot(x = carat, data = diamonds, geom = "freqpoly",
      binwidth = 10)
qplot(x = carat, data = diamonds, geom = "freqpoly",
      binwidth = 1)

library("tidyr","dplyr")
