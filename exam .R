load("/Users/victoria/Downloads/Household_Pulse_data_w48.RData")
View(Household_Pulse_data)
summary(Household_Pulse_data)

xtabs(~ TWDAYS + EEDUC, data = Household_Pulse_data)
              
#From the data, im going to hypothesize that those who have a higher degree work the most remote
attach(Household_Pulse_data)
use_varb <- ( TWDAYS == 3-4 & (EEDUC == "adv deg" ) 
dat_use <- subset(Household_Pulse_data,use_varb) # 
detach()

NNobs <- length(TWDAYS)
set.seed(12345) 
graph_obs <- (runif(NNobs) < 0.1) 
dat_graph <-subset(dat_use,graph_obs)  

plot(TWDAYS ~ jitter(EEDUC, factor = "adv deg")

model_temp1 <- lm( TWDAYS ~ EEDUC)
summary(model_temp1)
     
#I had trouble running the hypothesis due to errors, but looking at the data those that had no remote job had most of the advance degree compared to those that did have a remote job.   


xtabs(~ HADCOVIDRV + RECVDVACC, data = Household_Pulse_data )
#Im going to hypothesize that those who did not get vaxx got  covid 
attach(Household_Pulse_data)
use_varb <- (HADCOVIDRV == "yes tested + or doc told had covid" ) & (RECVDVACC == "no")
dat_use <- subset(Household_Pulse_data)
detach()

NNobs <- length(HADCOVIDRV)
set.seed(12345) 
graph_obs <- (runif(NNobs) < 0.1) 
dat_graph <-subset(dat_use,graph_obs)  

model_temp1 <- lm( HADCOVIDRN ~ RECVDVACC)
summary(model_temp1)

#Again i wasnt able to run the hypothesis, but those who  did get vaxx were less likely to get covid

xtabs(~ LONGCOVID + RECVDVACC, data = Household_Pulse_data)
# From the data, im going to hypothesize that those who got vaxx were less likely to have long covid compared those who did not get vaxxed
attach(Household_Pulse_data)
use_varb <- ( RECVDVACC == "yes" & (LONGCOVID == "no" ) 
dat_use <- subset(Household_Pulse_data,use_varb) # 
detach()

NNobs <- length(RECVDAVACC)
set.seed(12345) 
graph_obs <- (runif(NNobs) < 0.1) 
dat_graph <-subset(dat_use,graph_obs)  

model_temp1 <- lm( RECVACC ~ LONGCOVID )
summary(model_temp1)

# Again i was unable to perform the hypothesis but looking at the data thsoe that did get vaxxed did in fact not have long covid.


xtabs( ~RRACE + HADCOVIDRV), data = Household_Pulse_data 
# There was a study that people of color were more likely to get covid, so i wanted to test out there theory. My hypotheis is that people of color  were most likely to get covid.
attach(Household_Pulse_data)
use_varb <- ( RRACE == "Black" & (HADCOVIDRV == "yes" ) 
dat_use <- subset(Household_Pulse_data,use_varb) # 
detach()

NNobs <- length(RRACE == "Black")
set.seed(12345) 
graph_obs <- (runif(NNobs) < 0.1) 
dat_graph <-subset(dat_use,graph_obs)

model_temp1 <- lm( RRACE~ HADCOVIDRV )
summary(model_temp1)

# I was running out of time so i wasnt able to finish but i  wanted to start the question.
