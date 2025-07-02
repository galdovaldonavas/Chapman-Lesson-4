
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
