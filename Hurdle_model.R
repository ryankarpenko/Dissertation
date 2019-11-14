library(R2jags)
library(BCEA)
library(ggplot2)
library(haven)
library(bayestestR)
library(openxlsx)

#ONGOING QUESTIONS
#What to do with potential site/hospital effect?

#Data file is confidential
improve <- as.data.frame(read_dta(file = "************************"))

#Function to transform QALYs to (0-1)
qalyTransform <- function(x, min=NA, max=NA) {
  if(is.na(min))
    min <- min(x, na.rm=TRUE) - 0.01
  else
    min <- min - 0.01
  if(is.na(max))
    max <- max(x, na.rm=TRUE) + 0.01
  else
    max <- max + 0.01
  result <- rep(NA, length(x))
  for(i in 1:length(x)){
    if(!is.na(x[i]))
      result[i] <- (x[i] - min) / (max - min)
  }
  round(result, 7)
  return(result)
}

#Function to transform QALYs back to original scale
qalyUntransform <- function(x, min=NA, max=NA){
  if(is.na(min))
    min <- min(x, na.rm=TRUE) - 0.01
  else
    min <- min - 0.01
  if(is.na(max))
    max <- max(x, na.rm=TRUE) + 0.01
  else
    max <- max + 0.01
  result <- rep(NA, length(x))
  for(i in 1:length(x)){
    if(!is.na(x[i]))
      result[i] <- (x[i]*(max - min)) + min
  }
  round(result, 7)
  return(result)
}

#Transform QALYs, create d vector etc
{
  #Adjusted QALYs
  minq <- min(improve$qaly, na.rm=TRUE)
  maxq <- max(improve$qaly, na.rm=TRUE)
  
  qt <- qalyTransform(improve$qaly)
  spike <- qalyTransform(0,minq,maxq)
  
  ilogit <- function(x) {return(exp(x)/(1+exp(x)))}
  
  untransform <- (qt*(maxq - minq + 0.02)) + (minq - 0.01)
  
  #d[i] vector for hurdle model
  d <- rep(0, length(improve$patientid))
  #because they weren't dead at 1 year. Only missing val in death3mnth
  improve$death3mnth[136] <- 0
  d[improve$death3mnth==1] <- 1
  
  improve$qt <- qt
  
  #Percent of participants who are female, for weighted mean
  femPct <- rep(NA, 2)
  femPct[1] <- sum(improve$isex==2 & improve$arm==0) / sum(improve$arm==0)
  femPct[2] <- sum(improve$isex==2 & improve$arm==1) / sum(improve$arm==1)
  
  #Percent of participants on spike value, for weighted mean
  spikePct <- rep(NA, 2)
  spikePct[1] <- sum(d==1 & improve$arm==0) / sum(improve$arm==0)
  spikePct[2] <- sum(d==1 & improve$arm==1) / sum(improve$arm==1)
}

#Data to input into JAGS function
data <- list(e = as.vector(improve$qt),
             c = as.vector(improve$total_cost),
             N = length(improve$patientid),
             d = as.vector(d),
             miss = as.vector(is.na(improve$qaly)),
             twoSD = 2*sd(improve[!is.na(improve$qaly) & improve$qaly!=0,"qaly"], na.rm=TRUE),
             spikePct = as.vector(spikePct),
             missPct = as.vector(c(sum(is.na(improve$qaly)&improve$arm==0)/sum(improve$arm==0),sum(is.na(improve$qaly)&improve$arm==1)/sum(improve$arm==1))),
             arm = as.vector(improve$arm),
             max = max(improve$qaly, na.rm=TRUE),
             min = min(improve$qaly, na.rm=TRUE),
             maxhard = 4,
             spike = qalyTransform(0,minq,maxq),
             cage = as.vector(improve$age - mean(improve$age)),
             sex = as.vector(improve$isex - 1),
             hardman = as.vector(improve$hardman_new + 1),
             femPct = as.vector(sum(improve$isex==2)/length(improve$isex)),
             wtp = as.vector(c(20000,25000,30000,35000,40000)),
             Nwtp = 5)

#Parameters to save in JAGS function
params <- c("e","c","d","hardman",
            "phi.e","tau.e",
            "alpha0","alpha.age","alpha.sex","alpha.hard",
            "s2.e","s.e","s.c",
            "phi.c","tau.c",
            "beta0","beta.age","beta.sex","beta.hard","beta.q",
            "q","delta",
            "P","mu.e","mu.c","mu.q",
            "kappa0","kappa.age","kappa.sex",
            "delta.e","delta.c","delta.q",
            "mu.p","mu.i",
            "n.hi.4","n.hi.3","n.hi.2","n.hi.1",
            "p.hi.4","p.hi.3","p.hi.2","p.hi.1",
            "inb","p.ce")

#JAGS file name
filein <- "Hurdle_JAGS.txt"

#convert data to bugsData file, in case want to run manually.
#bugs.data(data, dir = getwd(), digits = 5, data.file = "dataTemp.txt")

#Hardman 1-4 initial values
{
  #NA's mean that the value is pre-specified in JAGS code, and does not need initialization
  
  # Mathematically, must be apporox < 0.47
  s.e.init1 <- matrix(data=c(0.1, 0.2, NA, NA), nrow=2, ncol=2, byrow=TRUE)
  s.e.init2 <- matrix(data=c(0.2, 0.1, NA, NA), nrow=2, ncol=2, byrow=TRUE)
  
  s.c.init1 <- c(10000, 20000)
  s.c.init2 <- c(8000, 17000)
  
  alpha0.init1 <- matrix(data = c(0.1, -0.1, NA, NA), nrow=2, ncol=2, byrow = TRUE)
  alpha0.init2 <- matrix(data = c(-0.1, 0.1, NA, NA), nrow=2, ncol=2, byrow=TRUE)
  
  alpha.age.init1 <- matrix(data = c(-0.02, 0.01, NA, NA), nrow=2, ncol=2, byrow=TRUE)
  alpha.age.init2 <- matrix(data = c(0.02, -0.01, NA, NA), nrow=2, ncol=2, byrow=TRUE)
  
  alpha.sex.init1 <- matrix(data = c(0.1, -0.1, NA, NA), nrow=2, ncol=2, byrow=TRUE)
  alpha.sex.init2 <- matrix(data = c(-0.1, 0.1, NA, NA), nrow=2, ncol=2, byrow=TRUE)
  
  alpha.hard.init1 <- array(data = c(NA,NA,NA,NA,
                                     0.1,NA,-0.1,NA,
                                     -0.1,NA,0.1,NA,
                                     0.1,NA,-0.1,NA),
                            dim=c(2, 2, 4))
  alpha.hard.init2 <- array(data = c(NA,NA,NA,NA,
                                     -0.1,NA,0.1,NA,
                                     0.1,NA,-0.1,NA,
                                     -0.1,NA,0.1,NA),
                            dim=c(2, 2, 4))
  
  beta0.init1 <- c(-0.05, 0.05)
  beta0.init2 <- c(0.1, -0.1)
  
  beta.age.init1 <- c(-0.02, 0.02)
  beta.age.init2 <- c(0.02, -0.02)
  
  beta.sex.init1 <- c(-0.02, 0.01)
  beta.sex.init2 <- c(0.02, -0.02)
  
  beta.q.init1 <- c(0.1, -0.1)
  beta.q.init2 <- c(-0.1, 0.1)
  
  beta.hard.init1 <- matrix(data=c(NA, -0.1, 0.1, -0.1,
                                   NA, 0.1, -0.1, 0.1),
                            nrow=2, ncol=4, byrow=TRUE)
  beta.hard.init2 <- matrix(data=c(NA, 0.1, -0.1, 0.1,
                                   NA, -0.1, 0.1, -0.1),
                            nrow=2, ncol=4, byrow=TRUE)
  
  hard.init1 <- rep(NA, length(data$hardman))
  hard.init1[is.na(data$hardman)] <-  sample(1:4, sum(is.na(data$hardman)), prob=c(164/539, 254/539, 94/539, 27/539), replace = TRUE)
  
  hard.init2 <- rep(NA, length(data$hardman))
  hard.init2[is.na(data$hardman)] <-  sample(1:4, sum(is.na(data$hardman)), prob=c(164/539, 254/539, 94/539, 27/539), replace = TRUE)
  
  #Initial values for JAGS function
  inits <- list(list(s.e=s.e.init1, s.c=s.c.init1,
                     alpha0=alpha0.init1, alpha.age=alpha.age.init1,
                     alpha.sex=alpha.sex.init1, alpha.hard=alpha.hard.init1,
                     beta0=beta0.init1, beta.age=beta.age.init1,
                     beta.sex=beta.sex.init1, beta.q=beta.q.init1,
                     beta.hard = beta.hard.init1),
                list(s.e=s.e.init2, s.c=s.c.init2,
                     alpha0=alpha0.init2, alpha.age=alpha.age.init2,
                     alpha.sex=alpha.sex.init2, alpha.hard=alpha.hard.init2,
                     beta0=beta0.init2, beta.age=beta.age.init2,
                     beta.sex=beta.sex.init2, beta.q=beta.q.init2,
                     beta.hard = beta.hard.init2))
}

#Collapse hardman values of 4+ for better convergence
for(i in 1:length(data$hardman)){
  if(!is.na(data$hardman[i]) & data$hardman[i] == 5){
    data$hardman[i] <- 4
  }
}

#Run Bayesian model, save in model object
mh <- R2jags::jags(data = data, inits = inits, parameters.to.save = params,
                   model.file = filein, n.chains=2, n.iter = 10000, n.burnin=7500, n.thin=1,
                   jags.module = c("glm","dic","basemod","bugs"))


#write to excel
summ <- m3.2$BUGSoutput$summary
write.xlsx(summ, 'BG_Summary_10k.xlsx', rowNames=TRUE)

#Understanding data format
#sims.array splits each variable by chain
#sims.matrix does not split by chain
#sims.list does not even split the variables by index
