library(dplyr)

source("fun_mle.R")
source("fun_mcmc.R")
source("fun_R0.R")
source("fun_SEIRstochastic.R")
source("fun_SEIRdeterministic.R")


data_state = read.csv(url("https://api.covid19india.org/csv/latest/state_wise_daily.csv"))
head(data_state)
states = c("MH", "TN", "DL","DL")
data_delhi = data_state[, c("Date", "Status", "DL")]
daily_confirmed = filter(data_delhi, Status %in% "Confirmed")[,3]
daily_recovered = filter(data_delhi, Status %in% "Recovered")[,3]
daily_deceased = filter(data_delhi, Status %in% "Deceased")[,3]
date = as.character(filter(data_delhi, Status %in% "Confirmed")[,1])
data_delhi = data.frame(date = date, "Daily.Confirmed" = daily_confirmed, 
                      "Daily.Recovered" = daily_recovered, "Daily.Deceased" = daily_deceased)


date_15_march = which(data_delhi$date == "15-Mar-20")
date_30_june = which(data_delhi$date == "30-Jun-20")


data = data_delhi[date_15_march:date_30_june, ]
obsP <- data[,"Daily.Confirmed"] ## Daily Positive
obsR <- data[,"Daily.Recovered"] ## Daily Recovered
obsD <- data[,"Daily.Deceased"] ##  Daily Deaths
obsP_current<- rep(0,times= length(obsP))

## Initial conditions

yinit = readRDS("initial_values.RDS")$yinit
r0 = readRDS("initial_values.RDS")$r0
f = readRDS("initial_values.RDS")$f
N = readRDS("initial_values.RDS")$N

## estimates of the pars c(b1, b2, b3, b4, b5, b6, r1, r2, r3, r4, r5, r6)
mcmc_pars_estimate <- read.table("Results_mcmc_pars.txt", header = T)
myfix_pars = readRDS("myfix_pars.RDS")

## PRediction assuming the parameters remain same as in last period after train period
source("outbreaksimu1.R")

prediction<- apply(mcmc_pars_estimate[1:1000, 1:14], 1, function(x) fun_seir(init_obs = yinit,myfix_pars = myfix_pars, pars = x,t_simu=200))
saveRDS(prediction,"Prediction.rds")