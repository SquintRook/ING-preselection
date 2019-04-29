library(rlang)
library(readxl)
library(tidyverse)
library(lubridate)
library(e1071)
library(ggplot2)
library(ggpubr)
library(gridExtra)

df <- read.table("https://mandrillapp.com/track/click/30530312/evenea.pl?p=eyJzIjoiOS1JQ1d5RmJVd3BTS0xRU1BxUktfOGFWc0hJIiwidiI6MSwicCI6IntcInVcIjozMDUzMDMxMixcInZcIjoxLFwidXJsXCI6XCJodHRwczpcXFwvXFxcL2V2ZW5lYS5wbFxcXC9cXFwvZmlsZVxcXC91c2VyXFxcL2V2ZW50X2ZpbGVzXFxcLzgzNzIwNlxcXC80NzZlM2IzZGJiN2M3NmY2NjgwYzMzYmE1MDdjMjIzNi5jc3ZcIixcImlkXCI6XCJmZTNlNzNhY2ZkNTY0OWJhYjI2NDViZmYyOWVlYTE0M1wiLFwidXJsX2lkc1wiOltcImU0ODNmYTExZjA5NmYzMDAxYTQ5MDE1OGFkZWYwMWRkMGRiZmQ5NzhcIl19In0",
                 sep = ";")%>%
  .[-1,]%>%
  transmute(date = dmy(V1),
            working = V2,
            withdrawals = as.numeric(as.character(V3)))

df <- read.table("https://s3-eu-west-1.amazonaws.com/landingi-editor-uploads/Ko8IsoAM/withdrawals.csv", 
                 sep = ";")%>%
  .[-1,]%>%
  transmute(date = dmy(V1),
            working = V2,
            withdrawals = as.numeric(as.character(V3)))

#if neither of the links above works, you have to provide data set from local pc

df.na <- drop_na(df)
# assuming that, when there is a non-working day the withdrawals are NA not 0 
# ergo, i will analyse only working day 
tail(df)
#descriptive statistics

average <- mean(df.na$withdrawals)
media.an <- median(df.na$withdrawals)
standard.dev <- sd(df.na$withdrawals)
quartiles <- quantile(df.na$withdrawals)
skew.coef <- skewness(df.na$withdrawals)
kurt <- kurtosis(df.na$withdrawals)

#histogram of withdrawals

ggplot(data = df.na)+
  geom_histogram(aes(x = withdrawals, y = ..density..),col = "steelblue3", bins = 100)+
  geom_density(aes(y = ..density.., x = withdrawals), size = 0.7, col = "coral")+
  scale_x_continuous(breaks = seq(0,max(df.na$withdrawals), 200))+
  labs(title = "histogram of withdrawals from bank")+
  xlab("amounts of withdrawals")


# its clearly visible that non-outliers are under 600k zł

# outliers

filter(df.na, withdrawals > 600) # biggest withdrawals before christmas holidays
# the difference between biggest non-outlier and outlier is around 1200 k zł. 

filter(df, date > "2018-12-17" & date < "2018-12-29")
# december 21 is the last day with open branch of the bank (22th and 23th is closed ) to 
# withdraw cash for before-christmas shopping during the weekend.  

# december 24 is the last time its possible to withdraw cash before christmas (25th and 26th)

# normality test
shapiro.test(df.na$withdrawals) # p-value < 0.05 - we reject H0: withdrawals ~ N(m, sigma) 
#i.e. its not normal

df.na.out <- filter(df.na, withdrawals < 600) 
shapiro.test(df.na.out$withdrawals) # same inference on outlier-free dataset

ggqqplot(df.na$withdrawals) # qq plot confirms our inference

# day of the week impact

df.weekday <- mutate(df.na, week.day = weekdays(date))%>%
  group_by(week.day)%>%
  summarise(average = mean(withdrawals),
            median = median(withdrawals))%>%
  arrange(desc(average))
# the average is indeed higher for firday and monday but its due to the outliers
# if we look at the median, the difference virtually disappears.

# friday is slightly higher than the general median. It might be explained 
# by clients intentions to go for a shopping during upcoming weekend.

mutate(df.na.out, week.day = weekdays(date))%>% # the same but without outliers
  group_by(week.day)%>%
  summarise(average = mean(withdrawals),
            median = median(withdrawals))%>%
  arrange(desc(average))
# given the fact that standard deviation of withdrawals is 151, the difference between 
# withdrawals is not significant


ggplot(df.weekday)+
  geom_col(aes(x = week.day, y = median),width = 0.8, col = "steelblue3", size = 1.2 )+
  coord_cartesian(ylim = c(100, 300))

###### day of the month impact #########

df.monthday <- mutate(df.na, month.day = mday(date))%>%
  group_by(month.day)%>%
  summarise(average = mean(withdrawals),
            median = median(withdrawals))%>%
  as.data.frame()

df.monthday.out <- mutate(df.na.out, month.day = mday(date))%>%
  group_by(month.day)%>%
  summarise(average = mean(withdrawals),
            median = median(withdrawals))%>%
  as.data.frame()


month.day.avg<- ggplot(df.monthday)+
  geom_col(aes(x = month.day, y = average),width = 0.8, col = "steelblue3", size = 0.7 )

month.day.median<- ggplot(df.monthday)+
  geom_col(aes(x = month.day, y = median),width = 0.8, col = "steelblue3", size = 0.7 )

grid.arrange(month.day.avg, month.day.median, 
             top = "average withdrawals on each day of the month")

# first day of the month seems to be significantly higher than other days with next 3 days
# also higher but gradually closer to average

# this might be due to wages being paid out to employees with various days at the beggining
# of each month depending on payday or weather it is a weekend or not

# two bins are in solitude, for there are outliers within them

########### probability of withdrawal ########
avg.na.out <- mean(df.na.out$withdrawals)
sd.na.out <- sd(df.na.out$withdrawals)
lower.bound <- 220
upper.bound <- 250

dens.funct <- function(x){dnorm(x = x, mean  = avg.na.out, sd = sd.na.out)} # defined function

integrate(dens.funct, lower = lower.bound, upper = upper.bound) #integral of density function 
# probability that daily withdrawals will amount in a range of 220k and 250k is 14.2%
# assuming withdrawals ~ N(m, sigma)

dens<- tibble(x = seq(qnorm(0.0001,avg.na.out,sd.na.out),
                      qnorm(0.9999,avg.na.out,sd.na.out),0.1), 
              y = dens.funct(x))

ggplot(dens, aes(x = x, y = y))+
  geom_line()+
  geom_area(data = filter(dens, between(x,lower.bound,upper.bound)), fill = "steelblue3")+
  labs(title = "Choosen range of withdrawals",
       subtitle = paste("Between", lower.bound,"000 zł and", upper.bound,"000 zł"))+
  xlab(label = "Amount of Withdrawals")+
  ylab(label = "Probability density")+
  scale_x_continuous(breaks = seq(-100,500,100))


nrow(filter(df.na, withdrawals >lower.bound & withdrawals < upper.bound))/nrow(df.na)
# historically 13% of withdrawals were in a given range
# the difference is visible but not big


########### trend of the withdrawals ############

ggplot(data = df.na.out,aes(x = date, y = withdrawals))+
  geom_line()+
  geom_smooth(method = lm)

# increasing trend is clearly visible, as well as seasonality.

model <- lm( df.na.out$withdrawals ~ df.na.out$date)

summary(model) # model diagnostics. Variables are statistically significant. p-value<0.05
# variable coefficient equals 0.438. So, with every day the amount of withdrawal increases by
# 438 zł


      