(.061-.068-20000)/20000
(.939-.932-20000)/20000
((1878*345)-(2000*345)-20000)/20000


-2*log(10)
-2*log(20)
-2*log(25)
-2*log(50)


9887+3879+2108 +2075+1234+1065

R1=9887+3879+2108-534+167

R2= 2075+1234+1065+832+612
R1+R2

TotalBaseline = 9887+3879+2075+1234

Totalwith15discount = TotalBaseline

Totalwith30discount = TotalBaseline+167-534+832+612

TotalBaselineMarch = 9887-4187+2075-1556

Totalwith30discount = TotalBaselineMarch - 534 + 832





##---Question Group C------

dataketchup<-read.csv('ketchup_data.csv')
head(dataketchup)

lm1<-lm(log(sales_heinz64)~log(price_heinz64)+log(price_heinz20)+log(price_hunts20),data=dataketchup)
lm2<-lm(log(sales_heinz20)~log(price_heinz64)+log(price_heinz20)+log(price_hunts20),data=dataketchup)
summary(lm1)
summary(lm2)
mean(dataketchup$price_heinz20)
mean(dataketchup$price_heinz64)
mean(dataketchup$price_hunts20)


##----Question Group D-------
DF_in<-read.csv('donations_data.csv')
DF<-DF_in
DF$Id<-NULL
DF$Revenue_0<-NULL
DF$Revenue_1<-NULL
DF$Revenue_2<-NULL
DF$Revenue_3<-NULL
DF$Revenue_4<-NULL
DF$Revenue_5<-NULL

library(cluster)
distMat_gower = daisy(DF,metric='gower',warnType = FALSE)


##Different Segments
clu_gower_2 = kmeans(distMat_gower, centers=2, nstart=10)
DF_in$clu_gower_2 = clu_gower_2$cluster

#3
clu_gower_3 = kmeans(distMat_gower, centers=3, nstart=10)
DF_in$clu_gower_3 = clu_gower_3$cluster

#4
clu_gower_4 = kmeans(distMat_gower, centers=4, nstart=10)
DF_in$clu_gower_4 = clu_gower_4$cluster

#5
clu_gower_5 = kmeans(distMat_gower, centers=5, nstart=10)
DF_in$clu_gower_5 = clu_gower_5$cluster


#6
clu_gower_6 = kmeans(distMat_gower, centers=6, nstart=10)
DF_in$clu_gower_6 = clu_gower_6$cluster

#7
clu_gower_7 = kmeans(distMat_gower, centers=7, nstart=10)
DF_in$clu_gower_7 = clu_gower_7$cluster

#8
clu_gower_8 = kmeans(distMat_gower, centers=8, nstart=10)
DF_in$clu_gower_8 = clu_gower_8$cluster



#9
clu_gower_9 = kmeans(distMat_gower, centers=9, nstart=10)
DF_in$clu_gower_9 = clu_gower_9$cluster

#10
clu_gower_10 = kmeans(distMat_gower, centers=10, nstart=10)
DF_in$clu_gower_10 = clu_gower_10$cluster






Nclus = 10 # max number of clusters to test
wss = rep(0,Nclus) # list to hold within-cluster sum-of-squares
for (i in 1:Nclus) { # loop over cluster
  res = kmeans(distMat_gower, centers=i, nstart=10)
  wss[i] = sum(res$withinss) # within-cluster sum-of-squares, summed over clusters
}
plot(1:Nclus, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="k-means elbow plot: Gower distance")


table(DF_in$clu_gower_4)/dim(DF_in)[1]


round(aggregate(
  cbind(Age,Male,Income)~clu_gower_4,
  data=DF_in,FUN=mean),3)


.5-.21





##CLV
##step 1 determine customer per period profit
rmargin=1
M = rmargin*mean(DF_in$Revenue_0)


##step 2 determine retention rate
alpha = sum(DF_in$Revenue_1>0)/sum(DF_in$Revenue_0>0)


## step 3 CLV simple
# CLV_simple(M,alpha,r,T)
# M = profit per period
# alpha = retention rate per period [0,1]
# r = discount rate [0,1]
# T = # future time periods for CLV calculation (T+1 total, including period 0)
CLV_simple = function(M,alpha,r,T) {
  clv = 0
  for (t in 0:T) {
    clv = clv + M*(alpha/(1+r))^t
  }
  return(clv)
}
# set discount rate, time horizon
r = .1 # discount rate
T = 5 # CLV horizon: 5 future years, 6 total, indexed 0 to 5
CLV_simple(M,alpha,r,T)
696.3055-25




##cohort method
rmargin = 1
# M: customer profit/year matrix
# (indexing removes first column in dataframe, the customer id)
M = rmargin*DF_in[,2:dim(DF_in)[2]]
colMeans(M)



CLV_cohort = function(M,r) {
  # CLV_cohort(M,r)
  # M = NxT+1 matrix/dataframe of profits (N customers over periods 0 to T)
  # r = discount rate [0,1]
  N = dim(M)[1] # # of customers
  T = dim(M)[2]-1 # # of future time periods
  # compute average profits by year
  avgRev = colMeans(M)
  # compute CLV
  clv = 0
  for (t in 0:T) {
    clv = clv + avgRev[t+1]/((1+r)^t)
  }
  return(clv)
}

CLV_cohort(M,r)


##segment 1
DF_in_1<-subset(DF_in,clu_gower_4==1)
rmargin=1
M = rmargin*mean(DF_in_1$Revenue_0)


##step 2 determine retention rate
alpha = sum(DF_in_1$Revenue_1>0)/sum(DF_in_1$Revenue_0>0)


## step 3 CLV simple
# CLV_simple(M,alpha,r,T)
# M = profit per period
# alpha = retention rate per period [0,1]
# r = discount rate [0,1]
# T = # future time periods for CLV calculation (T+1 total, including period 0)
CLV_simple = function(M,alpha,r,T) {
  clv = 0
  for (t in 0:T) {
    clv = clv + M*(alpha/(1+r))^t
  }
  return(clv)
}
# set discount rate, time horizon
r = .1 # discount rate
T = 5 # CLV horizon: 5 future years, 6 total, indexed 0 to 5
segment1 = CLV_simple(M,alpha,r,T)

##segment 2
DF_in_2<-subset(DF_in,clu_gower_4==2)
rmargin=1
M = rmargin*mean(DF_in_2$Revenue_0)


##step 2 determine retention rate
alpha = sum(DF_in_2$Revenue_1>0)/sum(DF_in_2$Revenue_0>0)


## step 3 CLV simple
# CLV_simple(M,alpha,r,T)
# M = profit per period
# alpha = retention rate per period [0,1]
# r = discount rate [0,1]
# T = # future time periods for CLV calculation (T+1 total, including period 0)
CLV_simple = function(M,alpha,r,T) {
  clv = 0
  for (t in 0:T) {
    clv = clv + M*(alpha/(1+r))^t
  }
  return(clv)
}
# set discount rate, time horizon
r = .1 # discount rate
T = 5 # CLV horizon: 5 future years, 6 total, indexed 0 to 5
segment2 = CLV_simple(M,alpha,r,T)

##segment 3

DF_in_3<-subset(DF_in,clu_gower_4==3)
rmargin=1
M = rmargin*mean(DF_in_3$Revenue_0)


##step 2 determine retention rate
alpha = sum(DF_in_3$Revenue_1>0)/sum(DF_in_3$Revenue_0>0)


## step 3 CLV simple
# CLV_simple(M,alpha,r,T)
# M = profit per period
# alpha = retention rate per period [0,1]
# r = discount rate [0,1]
# T = # future time periods for CLV calculation (T+1 total, including period 0)
CLV_simple = function(M,alpha,r,T) {
  clv = 0
  for (t in 0:T) {
    clv = clv + M*(alpha/(1+r))^t
  }
  return(clv)
}
# set discount rate, time horizon
r = .1 # discount rate
T = 5 # CLV horizon: 5 future years, 6 total, indexed 0 to 5
segment3 = CLV_simple(M,alpha,r,T)




##segment 4
DF_in_4<-subset(DF_in,clu_gower_4==4)
rmargin=1
M = rmargin*mean(DF_in_4$Revenue_0)


##step 2 determine retention rate
alpha = sum(DF_in_4$Revenue_1>0)/sum(DF_in_4$Revenue_0>0)


## step 3 CLV simple
# CLV_simple(M,alpha,r,T)
# M = profit per period
# alpha = retention rate per period [0,1]
# r = discount rate [0,1]
# T = # future time periods for CLV calculation (T+1 total, including period 0)
CLV_simple = function(M,alpha,r,T) {
  clv = 0
  for (t in 0:T) {
    clv = clv + M*(alpha/(1+r))^t
  }
  return(clv)
}
# set discount rate, time horizon
r = .1 # discount rate
T = 5 # CLV horizon: 5 future years, 6 total, indexed 0 to 5
segment4 = CLV_simple(M,alpha,r,T)
