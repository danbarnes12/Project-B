library(dplyr)
library(ggplot2)
library(tidyverse)
library(PerformanceAnalytics)

#####QUESTION 1 - STRATEGY SIMULATION#####

tickers = read.csv("Project B Data Pull.csv")
tickers$nm_tradedate = as.Date(as.character(tickers$nm_tradedate), format = "%m/%d/%Y")
smalltickers = subset(tickers, select = c("nm_tradedate","id","r"))

smalltickers = spread(smalltickers, key = id, value = r)
head(smalltickers)

smalltickers = mutate(smalltickers, avgr = rowMeans(smalltickers[,2:691]))
excessreturns = smalltickers[,FALSE]
excessreturns$nm_tradedate = smalltickers$nm_tradedate

#Calculate excess return and establish new dataframe
for (i in 2:691) {
  excessreturns = mutate(excessreturns, new = smalltickers[,i] - smalltickers$avgr)
  names(excessreturns)[i] = paste("ExcessReturn",i,"")
}

#defining user functions for summing over solely positive / negative values
sumneg = function(x) sum(x[x<0])
sumpos = function(x) sum(x[x>0])


#extract total excess positive and negative returns on a daily basis
excessreturns$totalpos = apply(excessreturns[,2:691],1,sumpos)
excessreturns$totalneg = apply(excessreturns[,2:691],1,sumneg)

#dataframe for weights
weights = excessreturns[,FALSE]
weights$nm_tradedate = excessreturns$nm_tradedate

#Assign weights according to proportional contribution to total excess positive and/or negative returns
for (i in 2:691) {
  weights = mutate(weights, new = 
                     ifelse(excessreturns[,i]>0,-excessreturns[,i]/excessreturns$totalpos,excessreturns[,i]/excessreturns$totalneg))
  names(weights)[i] = paste("Weight",i,"")
}

#SANITY CHECK - all positive weights should be 1 and all negative weights should be -1
apply(weights[,2:691],1,sumpos)
apply(weights[,2:691],1,sumneg)

#dataframe for daily returns
returns = weights[,FALSE]
returns$date = weights$nm_tradedate

dummyreturns = returns[FALSE,FALSE]

#iterate through each date - take the return times the prior day's weighting, sum it, and dump the result into the returns table

for (x in 2:1511){
  dummyreturns = returns[FALSE,FALSE]
  for (y in 2:691){
    dummyreturns[1,y-1] = smalltickers[x,y] * weights[x-1,y]
  }
  returns[x,2] = apply(dummyreturns, 1, sum)
}

summary(returns)

str(returns)

## Plot the returns for t
plot(returns$date, returns$V2, main = "Time series of daily portfolio returns", xlab = "Date", ylab = "Returns")


plot(smalltickers$nm_tradedate, smalltickers$avgr, main = "Time series of daily market returns", xlab = "Date", ylab = "Returns")


# Annualized Mean

#market
avg_daily_return = mean(smalltickers$avgr)*252
avg_daily_return

#strategy
returns_2 = na.omit(returns)
strat_avg_daily_return = mean(returns_2$V2)*252
strat_avg_daily_return


#  Volatility

#market volatility
sqrt(252) * sd(diff(smalltickers$avgr)) * 100

#strategy volatility
sqrt(252) * sd(diff(returns_2$V2))  * 100


# Sharp Ratio
# Assume the average risk free rate was 0% as portfolio is self-funded

#market sharpe
sharp_ratio_market = ((mean(smalltickers$avgr))/(sd(diff(smalltickers$avgr))))*sqrt(252)
sharp_ratio_market

#strategy sharpe

Sharp_ratio_strat = ((mean(returns_2$V2))/(sd(diff(returns_2$V2))))*sqrt(252)
Sharp_ratio_strat

# Consistency
mod1 = lm(V2~date, data = returns_2)
summary(mod1)

# Outliers

min(returns_2[,2], na.rm=T)
max(returns_2[,2], na.rm=T)


top_fifty <- returns_2 %>%
  filter(rank(desc(V2))<=50)
top_fifty

# Correlation between strategy and market returns
returns_2$market_average = smalltickers[match(returns_2$date, smalltickers$nm_tradedate),"avgr"] 
head(returns_2)
cor(returns_2$V2, returns_2$market_average)



#####QUESTION 2 - STRATEGY R&D: MOMENTUM TRADING STRATEGY#####

#determine momentum candidates (2 days straight positive or 2 days straight negative)
momentum = excessreturns[,FALSE]
momentum$date = excessreturns$nm_tradedate
for (x in 2:1511){
  for (y in 2:691){
    momentum[x,y] = ifelse(excessreturns[x-1,y]>0 && excessreturns[x,y]>0, 1, ifelse(excessreturns[x-1,y]<0 && excessreturns[x,y]<0,-1,0))
  }
}

momentum$totalpos = apply(momentum[,2:691],1,sumpos)
momentum$totalneg = apply(momentum[,2:691],1,sumneg)

#Calculate weights, again restricting total positive and negative weights to sum to 1 and -1, respectively
momentumweights = momentum[,FALSE]
momentumweights$date = momentum$date

for (i in 2:691){
  momentumweights = mutate(momentumweights, new = 
                             ifelse(momentum[,i]>0, momentum[,i]/momentum$totalpos, -momentum[,i]/momentum$totalneg))
  names(momentumweights)[i] = paste("Weight",i,"")
}

#sanity check weights
apply(momentumweights[,2:691],1,sumpos)
apply(momentumweights[,2:691],1,sumneg)

#returns
momentumreturns = momentumweights[,FALSE]
momentumreturns$date = momentumweights$date

dummyreturns = returns[FALSE,FALSE]

for (x in 3:1511){
  dummyreturns = returns[FALSE,FALSE]
  for (y in 2:691){
    dummyreturns[1,y-1] = smalltickers[x,y] * momentumweights[x-1,y]
  }
  momentumreturns[x,2] = apply(dummyreturns, 1, sum)
}

summary(momentumreturns)



#####QUESTION 3 - STRATEGY SIMULATION#####

#runs same loop for all returns k = 1 to k = 10
returnsfull = weights[,FALSE]
returnsfull$date = weights$nm_tradedate

#repeat process of calculating backtested returns for k = 1 to k = 10
for (k in 1:10){
  #print(k)
  for (x in (1+k):1511){
    dummyreturns = returnsfull[FALSE,FALSE]
    for (y in 2:691){
      dummyreturns[1,y-1] = smalltickers[x,y] * weights[x-k,y]
    }
    returnsfull[x,k+1] = apply(dummyreturns, 1, sum)
  }
}

head(returnsfull)
summary(returnsfull)
tail(returnsfull)

#test: confirm loop works for k = 2
returns2 = weights[,FALSE]
returns2$date = weights$nm_tradedate

dummyreturns = returns2[FALSE,FALSE]

#test: iterate for k = 2
for (x in 3:1511){
  dummyreturns = returns2[FALSE,FALSE]
  for (y in 2:691){
    dummyreturns[1,y-1] = smalltickers[x,y] * weights[x-2,y]
  }
  returns2[x,2] = apply(dummyreturns, 1, sum)
}

summary(returns2)

# Remove N/A values
returnsk1 = na.omit(returnsfull$V2)
returnsk2 = na.omit(returnsfull$V3)
returnsk3 = na.omit(returnsfull$V4)
returnsk4 = na.omit(returnsfull$V5)
returnsk5 = na.omit(returnsfull$V6)
returnsk6 = na.omit(returnsfull$V7)
returnsk7 = na.omit(returnsfull$V8)
returnsk8 = na.omit(returnsfull$V9)
returnsk9 = na.omit(returnsfull$V10)
returnsk10 = na.omit(returnsfull$V11)

# Annualized Mean for k = 1 to k = 10

k1_avgreturn = mean(returnsk1)*252
k2_avgreturn = mean(returnsk2)*252
k3_avgreturn = mean(returnsk3)*252
k4_avgreturn = mean(returnsk4)*252
k5_avgreturn = mean(returnsk5)*252
k6_avgreturn = mean(returnsk6)*252
k7_avgreturn = mean(returnsk7)*252
k8_avgreturn = mean(returnsk8)*252
k9_avgreturn = mean(returnsk9)*252
k10_avgreturn = mean(returnsk10)*252

k1_avgreturn #217.5%
k2_avgreturn #47.6%
k3_avgreturn #19.1%
k4_avgreturn #12.8%
k5_avgreturn #2.3%
k6_avgreturn #22.9%
k7_avgreturn #-0.5%
k8_avgreturn #12.2%
k9_avgreturn #5.7%
k10_avgreturn #1.7%

#  Volatility
k1_sd = sd(diff(returnsk1))*sqrt(252)*100
k2_sd = sd(diff(returnsk2))*sqrt(252)*100
k3_sd = sd(diff(returnsk3))*sqrt(252)*100
k4_sd = sd(diff(returnsk4))*sqrt(252)*100
k5_sd = sd(diff(returnsk5))*sqrt(252)*100
k6_sd = sd(diff(returnsk6))*sqrt(252)*100
k7_sd = sd(diff(returnsk7))*sqrt(252)*100
k8_sd = sd(diff(returnsk8))*sqrt(252)*100
k9_sd = sd(diff(returnsk9))*sqrt(252)*100
k10_sd = sd(diff(returnsk10))*sqrt(252)*100

k1_sd #27.8%
k2_sd #19.7%
k3_sd #19.1%
k4_sd #17.3%
k5_sd #17.5%
k6_sd #17.8%
k7_sd #16.1%
k8_sd #16.1%
k9_sd #16.7%
k10_sd #16.1%

# Sharp Ratio

sharpek1 = k1_avgreturn/k1_sd*100
sharpek2 = k2_avgreturn/k2_sd*100
sharpek3 = k3_avgreturn/k3_sd*100
sharpek4 = k4_avgreturn/k4_sd*100
sharpek5 = k5_avgreturn/k5_sd*100
sharpek6 = k6_avgreturn/k6_sd*100
sharpek7 = k7_avgreturn/k7_sd*100
sharpek8 = k8_avgreturn/k8_sd*100
sharpek9 = k9_avgreturn/k9_sd*100
sharpek10 = k10_avgreturn/k10_sd*100

sharpek1 #7.8
sharpek2 #2.4
sharpek3 #1.0
sharpek4 #0.7
sharpek5 #0.1
sharpek6 #1.3
sharpek7 #-0.0
sharpek8 #0.8
sharpek9 #0.3
sharpek10 #0.1

###Begin to compile final data frame
#create weights data frame with IDs as column names
weightsnew = weights
for (i in 2:691){
  colnames(weightsnew)[i] = tickers$id[i-1]
}

#remove dates
weightsnew = weightsnew[2:691]

#create shifted weights dataframes for k = 1 to k = 10
#for k = 1 we will not use 12/31/2001 weight, for k = 2 we will not use 12/28 or 12/31/2001 weights, etc.
weightsk1 = head(weightsnew, nrow(weightsnew) - 1)
weightsk2 = head(weightsnew, nrow(weightsnew) - 2)
weightsk3 = head(weightsnew, nrow(weightsnew) - 3)
weightsk4 = head(weightsnew, nrow(weightsnew) - 4)
weightsk5 = head(weightsnew, nrow(weightsnew) - 5)
weightsk6 = head(weightsnew, nrow(weightsnew) - 6)
weightsk7 = head(weightsnew, nrow(weightsnew) - 7)
weightsk8 = head(weightsnew, nrow(weightsnew) - 8)
weightsk9 = head(weightsnew, nrow(weightsnew) - 9)
weightsk10 = head(weightsnew, nrow(weightsnew) - 10)

#add in dates for each weights data frame, starting with 1/3/1996 for k = 1, 1/4/1996 for k = 2, etc.
weightsk1$nm_tradedate = weights$nm_tradedate[2:1511]
weightsk2$nm_tradedate = weights$nm_tradedate[3:1511]
weightsk3$nm_tradedate = weights$nm_tradedate[4:1511]
weightsk4$nm_tradedate = weights$nm_tradedate[5:1511]
weightsk5$nm_tradedate = weights$nm_tradedate[6:1511]
weightsk6$nm_tradedate = weights$nm_tradedate[7:1511]
weightsk7$nm_tradedate = weights$nm_tradedate[8:1511]
weightsk8$nm_tradedate = weights$nm_tradedate[9:1511]
weightsk9$nm_tradedate = weights$nm_tradedate[10:1511]
weightsk10$nm_tradedate = weights$nm_tradedate[11:1511]

#gather weights and add k values for k = 1 to k = 10; convert ids to num

gatherweightsk1 = weightsk1 %>% gather(id, w, 1:690)
gatherweightsk1$k = 1
gatherweightsk1$id = as.numeric(gatherweightsk1$id)

gatherweightsk2 = weightsk2 %>% gather(id, w, 1:690)
gatherweightsk2$k = 2
gatherweightsk2$id = as.numeric(gatherweightsk2$id)

gatherweightsk3 = weightsk3 %>% gather(id, w, 1:690)
gatherweightsk3$k = 3
gatherweightsk3$id = as.numeric(gatherweightsk3$id)

gatherweightsk4 = weightsk4 %>% gather(id, w, 1:690)
gatherweightsk4$k = 4
gatherweightsk4$id = as.numeric(gatherweightsk4$id)

gatherweightsk5 = weightsk5 %>% gather(id, w, 1:690)
gatherweightsk5$k = 5
gatherweightsk5$id = as.numeric(gatherweightsk5$id)

gatherweightsk6 = weightsk6 %>% gather(id, w, 1:690)
gatherweightsk6$k = 6
gatherweightsk6$id = as.numeric(gatherweightsk6$id)

gatherweightsk7 = weightsk7 %>% gather(id, w, 1:690)
gatherweightsk7$k = 7
gatherweightsk7$id = as.numeric(gatherweightsk7$id)

gatherweightsk8 = weightsk8 %>% gather(id, w, 1:690)
gatherweightsk8$k = 8
gatherweightsk8$id = as.numeric(gatherweightsk8$id)

gatherweightsk9 = weightsk9 %>% gather(id, w, 1:690)
gatherweightsk9$k = 9
gatherweightsk9$id = as.numeric(gatherweightsk9$id)

gatherweightsk10 = weightsk10 %>% gather(id, w, 1:690)
gatherweightsk10$k = 10
gatherweightsk10$id = as.numeric(gatherweightsk10$id)

#create data frame to send to access - note table will have 10,387,950 rows as there is one fewer 
finalmatrix = setNames(data.frame(matrix(ncol = 6, nrow = 10387950)), 
                       c("pid", "d", "id","k","w","vid"))

#set constant values and ensure date columns in correct format
finalmatrix$pid = 100003
finalmatrix$vid = 0
finalmatrix$d = as.Date(as.character(finalmatrix$d), format = "%m/%d/%Y")

#k = 1
finalmatrix$k[1:1041900] = gatherweightsk1$k
finalmatrix$id[1:1041900] = gatherweightsk1$id
finalmatrix$d[1:1041900] = gatherweightsk1$nm_tradedate
finalmatrix$w[1:1041900] = gatherweightsk1$w

#k = 2
finalmatrix$k[1041901:2083110] = gatherweightsk2$k
finalmatrix$id[1041901:2083110] = gatherweightsk2$id
finalmatrix$d[1041901:2083110] = gatherweightsk2$nm_tradedate
finalmatrix$w[1041901:2083110] = gatherweightsk2$w

#k = 3
finalmatrix$k[2083111:3123630] = gatherweightsk3$k
finalmatrix$id[2083111:3123630] = gatherweightsk3$id
finalmatrix$d[2083111:3123630] = gatherweightsk3$nm_tradedate
finalmatrix$w[2083111:3123630] = gatherweightsk3$w

#k = 4
finalmatrix$k[3123631:4163460] = gatherweightsk4$k
finalmatrix$id[3123631:4163460] = gatherweightsk4$id
finalmatrix$d[3123631:4163460] = gatherweightsk4$nm_tradedate
finalmatrix$w[3123631:4163460] = gatherweightsk4$w

#k = 5
finalmatrix$k[4163461:5202600] = gatherweightsk5$k
finalmatrix$id[4163461:5202600] = gatherweightsk5$id
finalmatrix$d[4163461:5202600] = gatherweightsk5$nm_tradedate
finalmatrix$w[4163461:5202600] = gatherweightsk5$w

#k = 6
finalmatrix$k[5202601:6241050] = gatherweightsk6$k
finalmatrix$id[5202601:6241050] = gatherweightsk6$id
finalmatrix$d[5202601:6241050] = gatherweightsk6$nm_tradedate
finalmatrix$w[5202601:6241050] = gatherweightsk6$w

#k = 7
finalmatrix$k[6241051:7278810] = gatherweightsk7$k
finalmatrix$id[6241051:7278810] = gatherweightsk7$id
finalmatrix$d[6241051:7278810] = gatherweightsk7$nm_tradedate
finalmatrix$w[6241051:7278810] = gatherweightsk7$w

#k = 8
finalmatrix$k[7278811:8315880] = gatherweightsk8$k
finalmatrix$id[7278811:8315880] = gatherweightsk8$id
finalmatrix$d[7278811:8315880] = gatherweightsk8$nm_tradedate
finalmatrix$w[7278811:8315880] = gatherweightsk8$w

#k = 9
finalmatrix$k[8315881:9352260] = gatherweightsk9$k
finalmatrix$id[8315881:9352260] = gatherweightsk9$id
finalmatrix$d[8315881:9352260] = gatherweightsk9$nm_tradedate
finalmatrix$w[8315881:9352260] = gatherweightsk9$w

#k = 10
finalmatrix$k[9352261:10387950] = gatherweightsk10$k
finalmatrix$id[9352261:10387950] = gatherweightsk10$id
finalmatrix$d[9352261:10387950] = gatherweightsk10$nm_tradedate
finalmatrix$w[9352261:10387950] = gatherweightsk10$w

#confirm columsn in correct formats, no NA values, head and tail appear correct
str(finalmatrix)
summary(finalmatrix)
head(finalmatrix)
tail(finalmatrix)

#export dataframe to csv
memory.limit()
write.csv(finalmatrix, file = "ProjectBData.csv",row.names = FALSE)
