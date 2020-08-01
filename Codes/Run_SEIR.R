## delhi

library(dplyr)

source("fun_mle.R")
source("fun_mcmc.R")
source("fun_R0.R")
source("fun_SEIRstochastic.R")
source("fun_SEIRdeterministic.R")

## Load data

data_state = read.csv(url("https://api.covid19india.org/csv/latest/state_wise_daily.csv"))
head(data_state)
states = c("MH", "TN", "DL","GJ","DL")
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
obsP <- abs(data[,"Daily.Confirmed"]) ## Daily Positive
obsR <- abs(data[,"Daily.Recovered"]) ## Daily Recovered
obsD <- abs(data[,"Daily.Deceased"]) ##  Daily Deaths
obsP_current<- rep(0,times= length(obsP))

View(data)

## Hyperparamters
N = 198e5

## Initial conditions

#initial number of ascertained cases based on the reports
P0 = 7
#initial number of false negative cases
f = 0
F0 = round(f/(1-f) * P0) 
#initial number of unascertained infectious cases
r0 <- 0.15
U0 =round( (P0 + F0) * (1-r0)/r0)
#initial number of recovered cases
RR0 = 0
RU0 = round(((1-r0)/r0+f/(1-f)) * RR0)
#initial number of deceased cases
DR0 = 0
DU0 = round(((1-r0)/r0+f/(1-f)) * DR0)
#initial number of latent cases, De=5.3
E0 = 3*(U0 + P0 + F0)
#initial number of susceptible individuals
S0 = N - (E0 + U0 + P0 + F0 + RU0 + RR0 + DU0 + DR0)
# initial states c(S = S0, E = E0, U = U0, P = P0, F = F0, RU = RU0, RR = RR0, DU = DU0, DR = DR0)
yinit = c("S" = S0, "E" = E0, "U" = U0, "P" = P0, "F" = F0, "RU" = RU0, "RR" = RR0, "DU" = DU0, "DR" = DR0)
saveRDS(list("yinit" = yinit, "r0" = r0, "f" = f, "N" = N), "initial_values.RDS")

## MCMC parameters estimatation

## fixed parameters
par(mfrow = c(1,1))
plot((cumsum(obsD) / cumsum(obsD+obsR))[round(length(obsD)/2):length(obsD)], type = 'l')
mcfr = tail((1+cumsum(obsD)) / (1+cumsum(obsD+obsR)),1)

plot(cumsum(obsP))
myfix_pars = c(alpha_p = 0.5, alpha_u = 0.7, beta_1 = 0.6, beta_2 = 0.7, delta_1 = 0.3, delta_2 = 0.7, 
               lambda = 1 / (69.416 * 365), mu = 1 / (69.416 * 365), mu_c = mcfr/17.8, De = 5.2, 
               Dr = 17.8/(1-mcfr), f = f, N = N) 
saveRDS(myfix_pars, "myfix_pars.RDS")

## initial parameters c(b1, b2, b3, b4, b5, b6, b7, r1, r2, r3, r4, r5, r6, r7)
pars_start <- c(c(1,0.8,0.6,0.4,0.4,0.4,0.1), c(0.1,0.1,0.1,0.1,0.1,0.1,0.1))

# Run MCMC for parameter estimation
pars_estimate <- fun_mcmc(init_pars = pars_start, step_pars = pars_start / 200, init_state_num = yinit,
                          observed_num = cbind(obsP, obsR, obsD, obsP_current), niter = 1e5, BurnIn = 5e4, trace_num = 100,
                          runMLE = FALSE, myfix_pars = myfix_pars, opt_num = 3)

## Trace plot
fgname <- colnames(pars_estimate)
par(mfrow=c(2, 3))
par(mar = c(3, 3, 1, 1))
for(i in 1:12) {
  plot(1:nrow(pars_estimate$mcmc_estimates), pars_estimate$mcmc_estimates[, i], xlab = "iter", ylab = fgname[i], 
       main="", type="l", cex = 0.1, pch = 20, col = "red")
}

mcmc_pars_estimate <- round(pars_estimate$mcmc_estimates, 3)
write.table(mcmc_pars_estimate, "Results_mcmc_pars.txt", quote = FALSE, row.names = FALSE, sep = "\t")
saveRDS(myfix_pars, "myfix_pars.rds")