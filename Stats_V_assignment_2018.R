# Clear workspace:  
rm(list=ls()) 

# Set working directories:
WD <- 'C:/Users/Anne Willems/Documents/KUL/MASTER/VAKKEN/MASTER 1/Statistics V/ASSIGNMENT'
setwd(WD)

# Setting the seed
set.seed(123)

# Load packages
list.of.packages <- c("runjags","coda", "mcmcplots", "foreign", "lme4")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){install.packages(new.packages,repos = "http://cran.us.r-project.org")}
lapply(list.of.packages, require, character.only = TRUE)

# Read data
data_set <- read.csv("soep_statV.csv", header = TRUE, sep = ",", dec = ".")


################################################################  PREPARATIONS FOR ANALYSES  ###################################################################

# ADDING VARIABLES TO DATASET
#-----------------------------

# Adding a person identifier variable that starts from 0 and goes to 1236
s <- rep(1:length(unique(data_set$id)), times = table(data_set$id))

# Centering age
age.c <- data_set$age - 55

# Making dummies for the categorical variable - Employment status at age 55
data_set <- cbind(data_set, full = rep(data_set$emplStat55))
data_set <- cbind(data_set, part = rep(data_set$emplStat55))
data_set$full <- ifelse(data_set$full == 1, 1, 0)
data_set$part <- ifelse(data_set$part == 2, 1, 0)

# Attaching all new variables to the dataset
data_set <- cbind(data_set, s, age.c)
attach(data_set)

#______________________________________________________________________________________________________________________________________________________________


# STORING USEFUL NUMBERS
#------------------------

Nsubj = length(unique(s)) # Number of subjects
Ntotal = length(s) # Total number of datapoints
waves <- c(55:60)
Nwaves = length(55:60) # Maximal number of waves per subject

#_______________________________________________________________________________________________________________________________________________________________


# PREPARING JAGS  
#-------------------------

# Data
y = lifeSat
x1 = age.c
x2 = healthSat
x3 = livtog
x4 = full
x5 = part

x.matrix <- matrix(c(x1, x2, x3, x4, x5), ncol = 5)

# Standardize the data:

ym <- mean(y)
ysd <- sd(y)
zy <- NULL
zx <- matrix(c(1:30120), ncol= 5)
xm <- NULL
xsd <- NULL

for ( i in 1:Ntotal ) {
  zy[i] <- ( y[i] - ym ) / ysd
}

for ( j in 1:dim(x.matrix)[2] ) {
  xm[j]  <- mean(x.matrix[,j])
  xsd[j] <-   sd(x.matrix[,j])
  for ( i in 1:Ntotal ) {
    zx[i,j] <- ( x.matrix[i,j] - xm[j] ) / xsd[j]
  }
}

# Bookkeeping:
nchains = 3
nadapt = 1000
nburn = 1000
niter = 10000
myinits <- NULL


#######################################################  LIFE SATISFACTION TRAJECTORIES IN LATE LIFE  ##########################################################


# EXPLORING THE DATA
#--------------------

# Descriptive statistics
mean_lifeSat <- tapply(lifeSat, age, mean) # Life satisfaction appears to be increasing slightly with age
sd_lifeSat <- tapply(lifeSat, age, sd) # The sd of life satisfaction decreases with age - people appear to become more homogeneous in life satisfaction 
                                             # when they get older


# Mean evolution
plot(x = waves, y = mean_lifeSat, type="b", xlim=c(55,60),ylim=c(0,10), pch = 20, xlab="Age (in years)",ylab="Life Satisfaction",axes=F,main="Mean evolution of life satisfaction")
axis(side=1,at=c(55:60),labels=c(55:60))
axis(side=2,at=seq(0,10,1))
box()
points(x = waves, y = mean_lifeSat, type="b", pch = 20)
  # When looking at the mean evolution over time, changes in life satisfaction appear to be fairly minimal


# Boxplot
boxplot(lifeSat ~ age, data = data_set, xlab = "Age (in years)", ylab = "Life Satisfaction", main = "Boxplots of life satisfaction by age" )
  # But, when looking at the boxplots of life satisfaction given age, you can see that even though the mean life satisfaction scores don't appear to differ
  # that much across age, there appears a lot of interindividual differences in life satisfaction 
  # at any particular age.At all ages, life satisfaction scores span across the entire range (0-10), therefore it is also advisable to look at the individual
  # lifesatisfaction trajectories, and if possible, try to identify factors that could be at the base of these interindividual differences


# Spaghetti plots
cols<- rainbow(100)
samp <- round(runif(100, 1,1236)) # sample 100 subjects from the total sample to plot their individual life satisfaction trajectories
plot(x = age[s==1],y = lifeSat[s==1], type="n",xlim=c(55,60),ylim=c(0,10),xlab="Age (in years)",ylab="Life Satisfaction",axes=F, main = "Individual life satisfaction trajectories")
axis(side=1,at=c(55:60),labels=c(55:60))
axis(side=2,at=seq(0,10,1))
box()
for (i in 1:length(samp)){
  points(x = age[s==samp[i]], y = lifeSat[s==samp[i]],type="l", col = cols[i])
}
  # again, huge interindividual differences.


# Individual regression lines 
simple.reg <- matrix(0,Nsubj,2) 
for (i in 1:Nsubj){ 
  regline <- lm(lifeSat[s==i] ~ age.c[s==i])
  simple.reg[i,1:2]<-regline$coefficients
}
simple.reg[is.na(simple.reg)] <-0
plot(x = age.c[s==1], y = lifeSat[s==1], type = "n", xlim =c(0,6), ylim = c(0,10), xlab = "Age (in years)", ylab = "Life Satisfaction", axes = F, main = "Individual regression lines (of 100 random participants)")
axis(side = 1, at = c(0:6), labels = c(0:6))
axis(side = 2, at = c(0:10), labels = seq(0,10,1))
box()
samp2 <- round(runif(100,1,1236))
for (j in 1:length(samp2)){
  abline(a = simple.reg[samp2[j],1], b = simple.reg[samp2[j],2], col = "grey")
}

# Histogram of intercepts and slopes
par(mfrow = c(2,1))
hist(simple.reg[,1], xlab = "Intercept", col = "grey", main = "Histogram of individual intercepts")
hist(simple.reg[,2], xlab = "Slope", col = "grey", main= "Histogram of individual slopes")

# Correlation individual intercepts and slopes
cor.intslope <- round(cor(simple.reg[,1], simple.reg[,2]),2)
plot(simple.reg[,1],simple.reg[,2], pch = 20, xlab="Intercept",ylab="Slope", main = "Relation between individual intercepts and slopes")
text(10, 6,"r = - 0.42")
    # The intercepts and slopes seem to be related


#______________________________________________________________________________________________________________________________________________________________


# SIMPLE MODEL - BAYESIAN
#-------------------------

# No correlation intercept - slope - DIC = 20366.29 for normal data, DIC = 12 947.73 for standardized data

simple_list = list(
  y = y,
  x = x, 
  s = s,
  Ntotal = Ntotal, 
  Nsubj = Nsubj
)
out.simple <- run.jags(model = "simple.txt", monitor = c("beta0mu", "beta1mu", "dic"), data = simple_list, inits = myinits, n.chains = nchains, adapt = nadapt, burnin = nburn, sample = niter)

zsimple_list = list(
  zy = zy,
  zx = zx[,1],
  s = s,
  Ntotal = Ntotal,
  Nsubj = Nsubj,
  xm = xm[1], 
  xsd = xsd[1],
  ym = ym,
  ysd = ysd
)

out.zsimple <- run.jags(model = "zsimple.txt", monitor = c("beta0mu", "beta1mu", "dic"), data = zsimple_list, inits = myinits, n.chains = nchains, adapt = nadapt, burnin = nburn, sample = niter)


# correlation intercept-slope - DIC = 20 275.46
simple.cor_list = list(
  y = y,
  x = x1,
  s = s,
  Ntotal = Ntotal,
  Nsubj = Nsubj,
  Omega = diag(c(1,1)),
  mean = c(0,0),
  df = 3,
  prec = diag(c(1.0E-6, 1.0E-6))
)

out.simple.cor <- run.jags(model = "simple.cor.txt", monitor = c("betamu", "cor", "dic"), data = simple.cor_list, inits = myinits, n.chains = nchains, adapt = nadapt, burnin = nburn, sample = niter) 


#______________________________________________________________________________________________________________________________________________________________


# SIMPLE MODEL - CLASSICAL
#--------------------------

# No correlation intercept-slope
simple <- lmer(lifeSat ~ 1 + age.c + (1|s) + (0 + age.c|s), data = data_set)
summary(simple)

# Correlation intercept-slope
simple.cor <- lmer(lifeSat ~ 1 + age.c + (1 + age.c|s), data = data_set)
summary(simple.cor)

#CONCLUSION
#----------

#overall, age does not seem to influence overall life Satisfaction much in this population. Therefore we need to look at other factors that maybe influence
# life satisfaction on top of age related changes.



#####################################################  FACTORS INFLUENCING LIFE SATISFACTION TRAJECTORY  ######################################################


# EXPLORING THE DATA - POSSIBLE FACTORS INFLUENCING LIFE SATISFACTION
#---------------------------------------------------------------------

# Descriptive statistics

# By employment status alone 
mean_empl <- tapply(lifeSat, emplStat55, mean)
sd_empl <- tapply(lifeSat, emplStat55, sd)

# By age and employment status
mean_by_empl <- tapply(lifeSat, list(age,emplStat55), mean)
sd_by_empl <- tapply(lifeSat, list(age, emplStat55), sd)

# By living situation 
mean_livtog <- tapply(lifeSat, livtog, mean)
sd_livtog <- tapply(lifeSat, livtog, sd)

# By age and living arrangements
mean_by_livtog <- tapply(lifeSat, list(age, livtog), mean)
sd_by_livtog <- tapply(lifeSat, list(age, livtog), sd)

# By health satisfaction
mean_health <- tapply(lifeSat, healthSat, mean)
sd_health <- tapply(lifeSat, healthSat, sd)

# By age and health satisfaction
mean_by_health <- tapply(lifeSat, list(age,healthSat), mean)
sd_by_health <- tapply(lifeSat, list(age, healthSat), sd)

# By health satisfaction, employment status and living situation
mean_all <- tapply(lifeSat, list(healthSat, emplStat55, livtog), mean)


# Mean evolution plots

par(mfrow = c(1,1))
# By employment status
plot(x = waves, y = mean_lifeSat, type="n", xlim=c(55,60),ylim=c(0,10), xlab="Age (in years)",ylab="Life Satisfaction",axes=F,main="Mean evolution of life satisfaction by employment status")
axis(side=1,at=c(55:60),labels=c(55:60))
axis(side=2,at=seq(0,10,1))
box()
col.empl <- c("black", "red", "green")
for (j in 1:length(col.empl)){
  points(x = waves, y = mean_by_empl[,j], type = "b", pch = 20, col = col.empl[j])
}
legend("bottomleft", c("Full Time Employment", "Part Time Employment", "Unemployed"), col = col.empl, lty = "solid")
  
# By living arrangements
plot(x = waves, y = mean_lifeSat, type="n", xlim=c(55,60),ylim=c(0,10), xlab="Age (in years)",ylab="Life Satisfaction",axes=F,main="Mean evolution of life satisfaction by living arrangements")
axis(side=1,at=c(55:60),labels=c(55:60))
axis(side=2,at=seq(0,10,1))
box()
col.livtog <- c("black", "red")
for (k in 1:length(col.livtog)){
  points(x = waves, y = mean_by_livtog[,k], type = "b", pch = 20, col = col.livtog[k])
}
legend("bottomleft", c("Married/Living with Partner", "Single/Divorced/Widow"), col = col.livtog, lty = "solid")

# By health satisfaction
plot(x = waves, y = mean_lifeSat, type="n", xlim=c(55,62),ylim=c(0,10), xlab="Age (in years)",ylab="Life Satisfaction",axes=F,main="Mean evolution of life satisfaction by health satisfaction")
axis(side=1,at=c(55:62),labels=c(55:62))
axis(side=2,at=seq(0,10,1))
box()
col.health <- rainbow(11)
for (l in 1:length(col.health)){
  points(x = waves, y = mean_by_health[,l], type = "b", pch = 20, col = col.health[l])
}
legend("bottomright", c("0","1","2","3", "4", "5", "6", "7", "8","9","10"), col = col.health, lty = "solid")


# Plot life satisfaction by health satisfaction
plot(x = c(0:10), y = mean_health, type = "p", pch = 20, xlim = c(0,10), ylim = c(0,10), xlab = "Health Satisfaction", ylab = "Life Satisfaction", axes = F, main = "Life Satisfaction by health satisfaction")
axis(side = 1, at = c(0:10), labels = c(0:10))
axis(side = 2, at = seq(0,10,1))
box()

    #At a more individual level: a person's mean life satisfaction score (between the ages of 55 and 60), seems to be related to how he/she would rate his/her health
mean.lifesat <- NULL
mean.healthsat <- NULL
sd.healthsat <- NULL
for(i in 1:Nsubj){
  mean.lifesat[i] = mean(lifeSat[s==i])
  mean.healthsat[i] = mean(healthSat[s==i])
  sd.healthsat[i] = sd(healthSat[s==i])
}
plot(mean.lifesat ~ mean.healthsat, type = "p", pch = 20, xlim = c(0,10), ylim = c(0,10), xlab = "Mean individual health satisfaction", ylab = "Mean individual life satisfaction", main = "Life satisfaction by health satisfaction ")
plot(mean.lifesat ~ sd.healthsat, type = "p", pch = 20, xlim = c(0,10), ylim = c(0,10), xlab = "Variation in individual health satisfaction", ylab = "Mean individual life satisfaction", main = "Life satisfaction by health satisfaction ")



#______________________________________________________________________________________________________________________________________________________________


# BAYESIAN MODELS
#-----------------

# MODEL 1 # By age and employment status - DIC = 20 263.25

x1.matrix <- matrix(c(x1, x4, x5), ncol = 3)

model1_list= list(
  x = x1.matrix ,
  y = y ,
  s = s ,
  Ntotal = Ntotal ,
  Nx = 3 ,
  NxO = 2,
  Nsubj = Nsubj,
  Omega = diag(c(1,1)), 
  mean = c(0,0),
  df=3,
  prec = diag(c(1.0E-6,1.0E-6))
)

out.model1 <- run.jags(model = "interind2.cor.txt", monitor = c("betamu", "betax", "cor", "dic" ), data = model1_list, inits = myinits, n.chains = nchains, adapt = nadapt, burnin = nburn, sample = niter)
out.model1


# MODEL 2 # By age and employment status, allowing part to interact with age - DIC = 20 256.43 (cf. plot - working part time, allows older people to enjoy life more - people used to be retired by then!)

ageXpart = age.c*part
x2.matrix=cbind(x1.matrix,ageXpart)

model2_list = list(
  x = x2.matrix ,
  y = y ,
  s = s ,
  Ntotal = Ntotal ,
  Nsubj = Nsubj,
  Nx=4,
  NxO= 3,
  Omega = diag(c(1,1)), 
  mean = c(0,0),
  df=3,
  prec = diag(c(1.0E-6,1.0E-6)) 
)

out.model2 <- run.jags(model = "interind2.cor.txt", monitor = c("betamu", "betax", "cor", "dic"), data = model2_list, inits = myinits, n.chains = nchains, adapt = nadapt, burnin = nburn, sample = niter)
out.model2


# MODEL 3 # By age and living together - dic = 20257.81

x3.matrix <- matrix(c(x1,x3), ncol = 2)

model3_list = list( 
  x = x3.matrix,
  y = y,
  s = s,
  Ntotal = Ntotal,
  Nsubj = Nsubj,
  Nx = 2,
  Omega = diag(c(1,1,1)),
  mean = c(0,0,0),
  df = 4,
  prec = diag(c(1.0E-6,1.0E-6,1.0E-6))
  )

out.model3 <- run.jags(model = "interind3.cor.txt", monitor = c("betamu", "cor", "dic"), data = model3_list, inits = myinits, n.chains = nchains, adapt = nadapt, burnin = nburn, sample = niter)
out.model3

# MODEL 4 # By age and living together, allowing for the interaction - 20260.16

ageXlivtog = age.c*livtog
x4.matrix <- cbind(x3.matrix, ageXlivtog)

model4_list = list( 
  x = x4.matrix,
  y = y,
  s = s,
  Ntotal = Ntotal,
  Nsubj = Nsubj,
  Nx = 3,
  Omega = diag(c(1,1,1,1)),
  mean = c(0,0,0,0),
  df = 5,
  prec = diag(c(1.0E-6,1.0E-6,1.0E-6,1.0E-6))
)

out.model4 <- run.jags(model = "interind4nonxo.cor.txt", monitor = c("betamu", "cor", "dic"), data = model4_list, inits = myinits, n.chains = nchains, adapt = nadapt, burnin = nburn, sample = niter)
out.model4


# MODEL 5 # By age and health - dic = 19551.02

x5.matrix <- matrix(c(x1,x2), ncol = 2)

model5_list = list( 
  x = x5.matrix,
  y = y,
  s = s,
  Ntotal = Ntotal,
  Nsubj = Nsubj,
  Nx = 2,
  Omega = diag(c(1,1,1)),
  mean = c(0,0,0),
  df = 4,
  prec = diag(c(1.0E-6,1.0E-6,1.0E-6))
)

out.model5 <- run.jags(model = "interind3.cor.txt", monitor = c("betamu", "cor", "dic"), data = model5_list, inits = myinits, n.chains = nchains, adapt = nadapt, burnin = nburn, sample = niter)
out.model5

# MODEL 6 # By age, health and allowing for them to interactb - dic = 19607.85

ageXhealth = age.c*healthSat
x6.matrix <- cbind(x5.matrix, ageXhealth)

model6_list = list( 
  x = x6.matrix,
  y = y,
  s = s,
  Ntotal = Ntotal,
  Nsubj = Nsubj,
  Nx = 3,
  Omega = diag(c(1,1,1,1)),
  mean = c(0,0,0,0),
  df = 5,
  prec = diag(c(1.0E-6,1.0E-6,1.0E-6,1.0E-6))
)

out.model6 <- run.jags(model = "interind4nonxo.cor.txt", monitor = c("betamu", "cor", "dic" ), data = model6_list, inits = myinits, n.chains = nchains, adapt = nadapt, burnin = nburn, sample = niter)
out.model6

# MODEL 7 # By all -dic = 19493.96

x7.matrix <- matrix(c(x1, x2, x3, x4, x5), ncol = 5)

model7_list = list( 
  x = x7.matrix,
  y = y,
  s = s,
  Ntotal = Ntotal,
  Nsubj = Nsubj,
  Nx = 5,
  NxO = 2,
  Omega = diag(c(1,1,1,1)), 
  mean = c(0,0,0,0),
  df=5,
  prec = diag(c(1.0E-6,1.0E-6,1.0E-6,1.0E-6))
  )

out.model7 <- run.jags(model = "interind4.cor.txt", monitor = c("betamu", "betax", "cor", "dic"), data = model7_list, inits = myinits, n.chains = nchains, adapt = nadapt, burnin = nburn, sample = niter)
out.model7

# MODEL 8 # By all except age - dic = 19571.38

x8.matrix <- matrix(c(x2, x3, x4, x5), ncol = 4)


model8_list = list( 
  x = x8.matrix,
  y = y,
  s = s,
  Ntotal = Ntotal,
  Nsubj = Nsubj,
  Nx = 4,
  NxO = 2,
  Omega = diag(c(1,1,1)), 
  mean = c(0,0,0),
  df=4,
  prec = diag(c(1.0E-6,1.0E-6,1.0E-6))
)

out.model8 <- run.jags(model = "interind3nxo.cor.txt", monitor = c("betamu", "betax", "cor", "dic"), data = model8_list, inits = myinits, n.chains = nchains, adapt = nadapt, burnin = nburn, sample = niter)
out.model8


# MODEL 9 # By all except age, but with interactions - dic = 19532.84

x9.matrix <- matrix(c(x2, x3, ageXlivtog, x4, x5, ageXpart), ncol = 6)

model9_list = list( 
  x = x9.matrix,
  y = y,
  s = s,
  Ntotal = Ntotal,
  Nsubj = Nsubj,
  Nx = 6,
  NxO = 3,
  Omega = diag(c(1,1,1,1)), 
  mean = c(0,0,0,0),
  df=5,
  prec = diag(c(1.0E-6,1.0E-6,1.0E-6,1.0E-6))
)

out.model9 <- run.jags(model = "interind4.cor.txt", monitor = c("betamu", "betax", "cor", "dic"), data = model9_list, inits = myinits, n.chains = nchains, adapt = nadapt, burnin = nburn, sample = niter)
out.model9

# MODEL 10 # Model with lowest DIC was model 7 (including all parameters), to make it even lower standerdized version (but without cor)

model10_list = list(
  zx=zx,
  zy=zy,
  s=s,
  Ntotal=Ntotal,
  Nsubj=Nsubj,
  Nx=5,
  NxO=2,
  ym=ym,
  ysd=ysd,
  xm=xm,
  xsd=xsd
)

out.model10 <- run.jags(model = "final.zmodel.nocor2.txt", monitor = c("beta0mu", "beta1mu", "beta2mu", "beta3mu", "betax", "dic"), data = model10_list, inits = myinits, n.chains=nchains, adapt = nadapt, burnin = nburn, sample = niter)
out.model10

# MODEL 11 - dic = 20 212.01

model11_list = list(
  x = x.matrix ,
  y = y ,
  s = s ,
  Ntotal = Ntotal ,
  Nx = 5 ,
  NxO = 2,
  Nsubj = Nsubj
)

out.model11 <- run.jags(model = "interind4nocor.txt", monitor = c("beta0mu", "beta1mu", "beta2mu", "beta3mu", "beta", "dic"), data = model11_list, inits = myinits, n.chains=nchains, adapt = nadapt, burnin = nburn, sample = niter)
out.model11


  
#_______________________________________________________________________________________________________________________________________________________________


# CLASSICAL MODELS
#------------------

# MODEL 1 # by age and employment status

cmodel1 <- lmer(lifeSat ~ 1 + age.c + full + part + (1 + age.c|s))
summary(cmodel1)

# MODEL 2 # by age, employment status + interaction

cmodel2 <- lmer(lifeSat ~ 1 + age.c + full + part + ageXpart + (1 + age.c|s))
summary(cmodel2)

# MODEL 3 # by age and living arrangements

cmodel3 <- lmer(lifeSat ~ 1 + age.c + livtog + (1 + age.c + livtog|s))
summary(cmodel3)

# MODEL 4 # by age, living arrangements + interaction

cmodel4 <- lmer(lifeSat ~ 1 + age.c + livtog + ageXlivtog + (1 + age.c + livtog + ageXlivtog|s))
summary(cmodel4)

# MODEL 5 # by age and health satisfaction

cmodel5 <- lmer(lifeSat ~ 1 + age.c + healthSat + (1 + age.c + healthSat|s))
summary(cmodel5)

# MODEL 6 # by age, health satisfaction and interaction

cmodel6 <- lmer(lifeSat ~ 1 + age.c + healthSat + ageXhealth + (1 + age.c + healthSat + ageXhealth|s))
summary(cmodel6)

# MODEL 7 # by all 

cmodel7 <- lmer(lifeSat ~ 1 + age.c + healthSat + livtog + full + part + (1 + age.c + livtog + healthSat|s))
summary(cmodel7)

# MODEL 8 # by all except age

cmodel8 <- lmer(lifeSat ~ 1 + healthSat + livtog + full + part + (1 + livtog + healthSat|s))
summary(cmodel8)

# MODEL 9 # by all except age, but keeping the interactions with age

cmodel9 <- lmer(lifeSat ~ 1 + healthSat + livtog + full + part + ageXpart + ageXlivtog + (1 + livtog + healthSat + ageXlivtog|s))
summary(cmodel9)

# MODEL 10 # by all standerdized and uncorrelated

cmodel10 <- lmer(lifeSat ~ 1 + age.c + healthSat + livtog + full + part + (1| s) + (age.c|s) + (livtog|s) + (healthSat|s))
summary(cmodel10)


#______________________________________________________________________________________________________________________________________________________________


# RESULTS
#---------

# When comparing the DIC of the models, the zsimple model seems to fit the data best. Still, we should keep in mind that it is the only model for which we were
# able to write the model with standardized data. When you look at the unstandardized DIC of the simple model, you see that it is even higher than the correlated
# simple one. Therefore we presume that if you were able to use standardized data for our final model (including all variables and allowing them to correlate)
# that the DIC would be the lowest of all the models we have tested. 


outcoda.model7=as.mcmc.list(out.model7) #keeping information about the chain
outcomb.model7=combine.mcmc(out.model7) #without chain information

#Convergence check
gelman.diag(outcoda.model7) #Gelman-rubin
plot(outcoda.model7,density=FALSE) #Traceplots
autocorr.plot(outcoda.model7) # autocorrelations --> a lot of AC, especially in the betax - the parameter of the employment predictor, maybe also interesting to exclude only these?
autocorr.plot(outcomb.model7)

#Inspect posterior
out.model7 # mean and HDI of parameters
plot(density(outcomb.model7[,"betamu[1]"]), main = "Density betamu[1]")
plot(density(outcomb.model7[,"betamu[2]"]), main = "Density betamu[2]")
plot(density(outcomb.model7[,"betamu[3]"]), main = "Density betamu[3]")
plot(density(outcomb.model7[,"betamu[4]"]), main = "Density betamu[4]")
plot(density(outcomb.model7[,"cor[1]"]), main = "Density cor[1]")
plot(density(outcomb.model7[,"cor[2]"]), main = "Density cor[2]")
plot(density(outcomb.model7[,"cor[3]"]), main = "Density cor[3]")
plot(density(outcomb.model7[,"cor[4]"]), main = "Density cor[4]")
plot(density(outcomb.model7[,"cor[5]"]), main = "Density cor[5]")
plot(density(outcomb.model7[,"cor[6]"]), main = "Density cor[6]")
plot(density(outcomb.model7[,"betax[1]"]), main = "Density betax[1]")
plot(density(outcomb.model7[,"betax[2]"]), main = "Density betax[2]")


# TESTING NEW MODEL - ALL PREDICTORS EXCEPT EMPLOYMENT + ALLOWING FOR CORRELATION

# MODEL 12 # Bayesian - DIC 19 507.72 (not so different from our chosen model)

x12.matrix <- matrix(c(x1, x2, x3), ncol = 3)

model12_list = list( 
  x = x12.matrix,
  y = y,
  s = s,
  Ntotal = Ntotal,
  Nx = 3,
  Nsubj = Nsubj,
  Omega = diag(c(1,1,1,1)),
  mean = c(0,0,0,0),
  df = 5,
  prec = diag(c(1.0E-6,1.0E-6,1.0E-6,1.0E-6))
)

out.model12 <- run.jags(model = "interind4nonxo.cor.txt", monitor = c("betamu", "cor", "dic" ), data = model12_list, inits = myinits, n.chains = nchains, adapt = nadapt, burnin = nburn, sample = niter)
out.model12

# MODEL 12 # by all except employment and correlated - classical 

cmodel12 <- lmer(lifeSat ~ 1 + age.c + healthSat + livtog + (1 + age.c + livtog + healthSat|s))
summary(cmodel12)

outcoda.model12=as.mcmc.list(out.model12) #keeping information about the chain
outcomb.model12=combine.mcmc(out.model12) #without chain information

#Convergence check
gelman.diag(outcoda.model12) #Gelman-rubin
plot(outcoda.model7,density=FALSE) #Traceplots
autocorr.plot(outcoda.model12) # autocorrelations --> a lot of AC, especially in the betax - the parameter of the employment predictor, maybe also interesting to exclude only these?
autocorr.plot(outcomb.model12)

#Inspect posterior
out.model7 # mean and HDI of parameters
plot(density(outcomb.model12[,"betamu[1]"]), main = "Density betamu[1]")
plot(density(outcomb.model12[,"betamu[2]"]), main = "Density betamu[2]")
plot(density(outcomb.model12[,"betamu[3]"]), main = "Density betamu[3]")
plot(density(outcomb.model12[,"betamu[4]"]), main = "Density betamu[4]")
plot(density(outcomb.model12[,"cor[1]"]), main = "Density cor[1]")
plot(density(outcomb.model12[,"cor[2]"]), main = "Density cor[2]")
plot(density(outcomb.model12[,"cor[3]"]), main = "Density cor[3]")
plot(density(outcomb.model12[,"cor[4]"]), main = "Density cor[4]")
plot(density(outcomb.model12[,"cor[5]"]), main = "Density cor[5]")
plot(density(outcomb.model12[,"cor[6]"]), main = "Density cor[6]")



# visualizing model - extra? what do we still need to do? 
# Shrinkage

# Sensitivity analysis

# limitations: high autocorrelation, did it truely converge (? - cf traceplot), no standerdized data

