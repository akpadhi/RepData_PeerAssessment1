setwd("~/Documents/myStuff/courses/reproducibleResearch/week2/Project/submission/RepData_PeerAssessment1")
library(dplyr)
library(ggplot2)
library(xtable)
library(lubridate)
library(knitr)
library(markdown)

unzip("activity.zip")

activity<-tbl_df(read.csv("activity.csv", header = TRUE))
head(activity)
activityByDate<-activity %>%
    filter(!is.na(steps))%>%
    group_by(date)%>%
    summarise(totalStepsPerDay = sum(steps), 
              count = n(), 
              avgStepsPerDay = mean(steps), 
              mdnStepsPerDay = median(steps))

mean(activityByDate$totalStepsPerDay)

ggplot(activityByDate, aes(totalStepsPerDay))+
    geom_histogram(aes(fill = ..count..), binwidth =2500, col = "black")+
    scale_fill_gradient("Count", low = "green", high = "blue") +
    theme_bw() + 
    ylab("Frequency") + 
    xlab("Total Steps per day") +
    ggtitle("Histogram of total steps per day") +
    theme(
        plot.title = element_text(size = rel(1.5), color = "black", face = "bold", hjust = 0.5),
        plot.margin = unit(c(.5,1,1,1), "cm"),
        axis.title.x = element_text(color="black", size=rel(1.3), face="bold"),
        axis.title.y = element_text(color="black", size=rel(1.3), face="bold"),
        axis.text.x = element_text(color="black", size=12, angle = 90), 
        axis.text.y = element_text(color="black", size=12))

stepsSummary<-activity %>%
    filter(!is.na(steps))%>%
    group_by(date)%>%
    summarise(avgStepsPerDay = mean(steps), 
              mdnStepsPerDay = median(steps))

mean(activityImpByDate$totalStepsImpPerDay)
median(activityImpByDate$totalStepsImpPerDay)



xt<- xtable(stepsSummary)
print(xt, type = "html")

# 3 mean of steps perday

activityByInterval <- activity %>%
    filter(!is.na(steps)) %>%
    group_by(interval) %>%
    summarise(totalStepsPerInterval = sum(steps), 
              count = n(), 
              avgStepsPerInterval = mean(steps), 
              mdnStepsPerInterval = median(steps))

ggplot(activityByInterval, aes(interval, avgStepsPerInterval)) + 
    geom_line(col = "blue")+ 
    labs(title="Plot of everage steps per interval",
         y ="Average number of steps", 
         x = "Interval")+
    theme_bw() +
    theme(
        plot.title = element_text(size = rel(1.5), color = "black", face = "bold", hjust = 0.5),
        plot.margin = unit(c(.5,1,1,1), "cm"),
        axis.title.x = element_text(color="black", size=rel(1.3), face="bold"),
        axis.title.y = element_text(color="black", size=rel(1.3), face="bold"),
        axis.text.x = element_text(color="black", size=12, angle = 90), 
        axis.text.y = element_text(color="black", size=12))

# Number 4

sum(is.na(activity))

# Imputing

library(mice)
Dat1 <- subset(activity, select=c(steps, date, interval)) 
ini <- mice(Dat1, maxit=0, pri=F)
pred <- ini$pred
pred[,c("interval", "date")] <- 0
meth <- ini$meth
meth[c("interval", "date")] <- "" 

imp <- mice(Dat1, m=5, maxit=10, printFlag=TRUE, pred=pred, meth=meth, seed=2345)
Datimp <- complete(imp, "long", include=TRUE)

activityImp <- Datimp %>%
    filter(.imp == 5) %>%
    select(steps:interval)

activityImpByDate<-activityImp %>%
    group_by(date)%>%
    summarise(totalStepsImpPerDay = sum(steps), 
              count = n(), 
              avgStepsImpPerDay = mean(steps), 
              mdnStepsPerImpDay = median(steps))

stepsImpSummary<-activityImp %>%
    filter(!is.na(steps))%>%
    summarise(avgStepsImpPerDay = mean(steps), 
              mdnStepsImpPerDay = median(steps))

xt<- xtable(stepsImpSummary)
print(xt, type = "html")

ggplot(activityImpByDate, aes(totalStepsImpPerDay))+
    geom_histogram(aes(fill = ..count..), binwidth =2500, col = "black")+
    scale_fill_gradient("Count", low = "green", high = "blue") +
    theme_bw() + 
    ylab("Frequency") + 
    xlab("Total Steps per day") +
    ggtitle("Histogram of total steps per day") +
    theme(
        plot.title = element_text(size = rel(1.5), color = "black", face = "bold", hjust = 0.5),
        plot.margin = unit(c(.5,1,1,1), "cm"),
        axis.title.x = element_text(color="black", size=rel(1.3), face="bold"),
        axis.title.y = element_text(color="black", size=rel(1.3), face="bold"),
        axis.text.x = element_text(color="black", size=12, angle = 90), 
        axis.text.y = element_text(color="black", size=12))

# Weekends or Weekkdays

activityImpWeekdays <- activityImp %>%
    mutate(weekday = weekdays(mdy(date))) %>%
    mutate(WDorWE = ifelse(weekday == "Saturday" | weekday == "Sunday", "Weekend", "Weekday"))

activityWDorWEByInterval <- activityImpWeekdays %>%
    group_by(WDorWE,interval) %>%
    summarise(WDorWEtotalStepsPerInterval = sum(steps), 
              count = n(), 
              WDorWEavgStepsPerInterval = mean(steps), 
              WDorWEmdnStepsPerInterval = median(steps))

ggplot(activityWDorWEByInterval, aes(interval, WDorWEavgStepsPerInterval)) + 
    geom_line(col = "blue")+ 
    facet_grid(.~WDorWE) +
    labs(title="Plot of everage steps per interval",
         y ="Average number of steps", 
         x = "Interval")+
    theme_bw() +
    theme(
        plot.title = element_text(size = rel(1.5), color = "black", face = "bold", hjust = 0.5),
        plot.margin = unit(c(.5,1,1,1), "cm"),
        axis.title.x = element_text(color="black", size=rel(1.3), face="bold"),
        axis.title.y = element_text(color="black", size=rel(1.3), face="bold"),
        axis.text.x = element_text(color="black", size=12, angle = 90), 
        axis.text.y = element_text(color="black", size=12))
