
# Chapter 4

# Simulate CRM data
# optional instead of creating data:
#   cust.df <- read.csv(paste("http://r-marketing.r-forge.r-project.org/",
#                          "data/rintro-chapter4.csv", sep=""))
# OR
# cust.df <- read.csv("http://goo.gl/PmPkaG")
#
# and then:
#   cust.df$cust.id <- factor(cust.df$cust.id)

set.seed(21821)
ncust <- 1000
cust.df <- data.frame(cust.id=factor(1:ncust))

cust.df$age <- rnorm(n=ncust, mean=35, sd=5)
cust.df$credit.score <- rnorm(n=ncust, mean=3*cust.df$age+620, sd=50)
cust.df$email <- factor(sample(c("yes", "no"), size=ncust, replace=TRUE, 
                               prob=c(0.8, 0.2)))
cust.df$distance.to.store <- exp(rnorm(n=ncust, mean=2, sd=1.2)) 
summary(cust.df)

# Try it!: plot(hist(distance.to.store))

cust.df$online.visits <- rnbinom(ncust, size=0.3, 
                                 mu = 15 + ifelse(cust.df$email=="yes", 15, 0) 
                                 - 0.7 * (cust.df$age-median(cust.df$age))) 

cust.df$online.trans <- rbinom(ncust, size=cust.df$online.visits, prob=0.3)
cust.df$online.spend <- exp(rnorm(ncust, mean=3, sd=0.1)) * 
  cust.df$online.trans

cust.df$store.trans <- rnbinom(ncust, size=5, 
                               mu=3 / sqrt(cust.df$distance.to.store))
cust.df$store.spend <- exp(rnorm(ncust, mean=3.5, sd=0.4)) * 
  cust.df$store.trans
summary(cust.df)

sat.overall <- rnorm(ncust, mean=3.1, sd=0.7)
summary(sat.overall)

sat.service <- floor(sat.overall + rnorm(ncust, mean=0.5, sd=0.4))
sat.selection <- floor(sat.overall + rnorm(ncust, mean=-0.2, sd=0.6))
summary(cbind(sat.service, sat.selection))


####Nice strategy for recoding variables: 
sat.service[sat.service > 5] <- 5
sat.service[sat.service < 1] <- 1
sat.selection[sat.selection > 5] <- 5
sat.selection[sat.selection < 1] <- 1
summary(cbind(sat.service, sat.selection))

no.response <- as.logical(rbinom(ncust, size=1, prob=cust.df$age/100))
sat.service[no.response] <- NA
sat.selection[no.response] <- NA
summary(cbind(sat.service, sat.selection))

cust.df$sat.service <- sat.service
cust.df$sat.selection <- sat.selection
summary(cust.df)

rm(ncust, sat.overall, sat.service, sat.selection, no.response)

str(cust.df)


#####
## 4.2 Exploring Associations Between Variables with Scatterplots

#Basic scatterplot

plot(cust.df$age, cust.df$credit.score)
plot(cust.df$age, cust.df$credit.score,
     col="darkblue",
     xlim = c(15,55),
     ylim= c(500,900),
     main="Active Customers as of June 2014",
     xlab="Customer Age(Years)",
     ylab= "Customer Credit Score")
abline(h=mean(cust.df$credit.score), col="blue", lty="dotted")
abline(v=mean(cust.df$age), col="blue", lty="dotted")

methods(plot) # to see different type of plots available


plot(cust.df$store.spend, cust.df$online.spend,
     col="darkblue",
     main="Money Spent by Customers from June 2013 to June 2014",
     xlab="Store Spent($)",
     ylab= "Online Spent ($)",
     cex=0.7) # to make the points smaller, so we can see them more clearly

# we see a typical scatterplot with very skewed variables, as we can see in the histograms
hist(cust.df$store.spend,
     breaks=(0:ceiling(max(cust.df$store.spend)/10))*10,
     main="Money Spent by Customers from June 2013 to June 2014",
     xlab="Store Spent($)",
     ylab= "Frequency")

#We add colors and shapes for a third variable of email, but first we specify the colors and shapes we want
my.col<- c("black","green3")
my.pch<- c(1,19)


plot(cust.df$store.spend, cust.df$online.spend,
     col=my.col[cust.df$email],
     pch=  my.pch[cust.df$email],
     main="Money Spent by Customers from June 2013 to June 2014",
     xlab="Store Spent($)",
     ylab= "Online Spent ($)",
     cex=0.7) # to make the points smaller, so we can see them more clearly

#adding a legend
legend(x="topright", legend=paste("email on file:", levels(cust.df$email)), col=my.col, pch=my.pch)


#we add logarightmic scales for both variables, because both are skewed

plot(cust.df$store.spend + 1     , cust.df$online.spend +1,
     col=my.col[cust.df$email],
     pch=  my.pch[cust.df$email],
     main="Money Spent by Customers from June 2013 to June 2014",
     xlab="Store Spent($)",
     ylab= "Online Spent ($)",
     cex=0.7,
     log="xy"
     ) # log xy means that log scales apply to both x and y axes, x would be only for x axes. 

#adding a legend
legend(x="topright", legend=paste("email on file:", levels(cust.df$email)), col=my.col, pch=my.pch)








####
##COMNBINING PLOTS IN A SINGLE GRAPHIC OBJECT
# we are going to use distance to store, which is also skewed
hist(log(cust.df$distance.to.store))
par(mfrow=c(2,2))

plot(cust.df$distance.to.store    , cust.df$store.spend,
     main="Store",
     xlab="Distance",
     ylab= "Money Spent",
     cex=0.7
)


plot(cust.df$distance.to.store    , cust.df$online.spend,
     main="Online",
     xlab="Distance",
     ylab= "Money Spent",
     cex=0.7
)



plot(cust.df$distance.to.store    , cust.df$store.spend +1,
     main="Store Log",
     xlab="Distance",
     ylab= "Money Spent",
     cex=0.7,
     log="xy"
)


plot(cust.df$distance.to.store    , cust.df$online.spend +1,
     main="Online Log",
     xlab="Distance",
     ylab= "Money Spent",
     cex=0.7,
     log="xy"
)

par(mfrow=c(1,1))



#####
### SCATTERPLOT MATRICES

pairs(formula = ~ age + credit.score + email+ distance.to.store+ online.visits+ online.trans+ online.spend+ store.trans+ store.spend, 
      data=cust.df)

#other ways to get the matrix
pairs(cust.df[ ,c(2:10)]) 

pairs(cust.df[ ,2:10])

#and with the car package, which also give us the distributions
library(car)
scatterplotMatrix(formula = ~ age + credit.score + email+ distance.to.store+ online.visits+ online.trans+ online.spend+ store.trans+ store.spend, 
                  data=cust.df)

# 4.5 CORRELATION TESTS

#covariance
cov(cust.df$age, cust.df$credit.score)

#Pearson correlation
cor(cust.df$age, cust.df$credit.score)
cov(cust.df$age, cust.df$credit.score) /(sd(cust.df$age)*sd(cust.df$credit.score)) #another way

cor.test(cust.df$age, cust.df$credit.score) #for test of significance

#Correlation matrices
cor(cust.df[,c(2,3,5:12)])
library(psych)
corr.test(cust.df[,c(2,3,5:12)]) # for test of significance we need the corr.test function from psych

# we can build graphs with the correlation matrices
install.packages(c("corrplot", "gplots"))
library(corrplot)
library(gplots)
corrplot.mixed(corr=cor(cust.df[,c(2,3,5:12)], use= "complete.obs"), 
                        upper="ellipse",
                        tl.post="lt",
                        upper.col= colorpanel(50,"red", "gray60", "blue4"))

#4.5.3. TRANSFORMING VARIABLES

#no linear relations might lead to confusing r
x<-runif(1000, min=-10, max=10)
hist(x)
plot(x, x^2)
cor(x, x^2)

#also an example in our data
cor(cust.df$store.spend,cust.df$distance.to.store)
cor(cust.df$store.spend,cust.df$distance.to.store, method= "spearman")
cor(log(cust.df$store.spend+1), log(cust.df$distance.to.store))

plot(cust.df$distance.to.store, cust.df$store.spend)
plot(1/sqrt(cust.df$distance.to.store), cust.df$store.spend)

##Box-cox transformations with library car
powerTransform(cust.df$age)#to find lambda, if close to 1 no transformation is needed
powerTransform(cust.df$distance.to.store) 
lambda<- coef(powerTransform(1/cust.df$distance.to.store))# the 1/ appears to be an inverse transformation to get a positive value
lambda

bcPower(cust.df$distance.to.store, lambda)


hist(cust.df$distance.to.store)
hist(bcPower(cust.df$distance.to.store, lambda))

#we apply the transformations and calculate correlations

l.dist<- coef(powerTransform(cust.df$distance.to.store)) # I do not undestand why before used 1/ to extract the coefficient in positive terms, the result is the same. 
l.spend<- coef(powerTransform(cust.df$store.spend+1))

cor(bcPower(cust.df$distance.to.store, l.dist), bcPower(cust.df$store.spend +1, l.spend))


####
##ASSOCIATIONS IN  SURVEY RESPONSES
#version without jitter
plot(cust.df$sat.service, cust.df$sat.selection,
     xlab= "satisfaction with service",
     ylab="satisfaction with product selection",
     main= "Relations between satisfaction types")

#version con jitter
plot(jitter(cust.df$sat.service), jitter(cust.df$sat.selection),
     xlab= "satisfaction with service",
     ylab="satisfaction with product selection",
     main= "Relations between satisfaction types")

###POLICHORIC CORRELATIONS

#Without polichoric correlations
resp<- !is.na(cust.df$sat.service)
cor(cust.df$sat.service[resp], cust.df$sat.selection[resp])
cor.test(cust.df$sat.service, cust.df$sat.selection, use= "complete.obs") #another way


#With polichoric correlations of psych package

polychoric(cbind(cust.df$sat.service[resp], cust.df$sat.selection[resp]))

#with Spearman correlation
cor(cust.df$sat.service, cust.df$sat.selection,method="spearman", use= "complete.obs") #another way

