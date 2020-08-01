## Simulation based on deterministic SEIR model
## 7 periods: March 15-24, March 25-April14, April15-May3, May4-May17, May18-May31, June1-June19, June20-June30
#' @param init_obs                a vetor of initial states c(S = S0, E = E0, U = U0, P = P0, F = F0, RU = RU0, RR = RR0, DU = DU0, DR = DR0)
#' @param times                   the time period to simulate,for example, length(observed_num) + 16 means from Mar 15 to May 31
#' @param pars                    a vetor of pars: c(b1, b2, b3, b4, b5, b6, b7, r1, r2, r3, r4, r5, r6, r7)
#' @param fix_pars                pars are the same for the five periods, c(alpha_p, alpha_u, beta_1, beta_2, delta_1, delta_2, lambda, mu, mu_c, De, Dr, f, N)
#' @param alpha_p                 ratio of transmission rates of ascertained cases over false negative cases
#' @param alpha_u                 ratio of transmission rates of unascertained cases over false negative cases
#' @param beta_1                  ratio of time till recovery of unascertained cases over ascertained cases
#' @param beta_2                  ratio of time till recovery of ascertained cases over false negative cases
#' @param delta_1                 ratio of death rate of unascertained cases over ascertained cases
#' @param delta_2                 ratio of death rate recovery of ascertained cases over false negative cases
#' @param lambda                  natural birth rate
#' @param mu                      natural death rate
#' @param mu_c                    death rate of ascertained cases
#' @param De                      latent period / incubation period
#' @param Dr                      Average days till recovery for ascertained cases
#' @param f                       false negative rate
#' @param N                       population size

#' @param stage_pars              pars are different for the different periods, c(b, r)
#' @param b                       transmission rate of ascertained cases
#' @param r                       ascertainment rate  

fun_pred <- function(init_obs, times, pars, myfix_pars) {
  ## myode function
  myode <- function(stage_pars, fix_pars, old_values) {
    ## stage pars
    b = stage_pars[1]
    r = stage_pars[2]
    ## fixed pars
    alpha_p = fix_pars[1]  
    alpha_u = fix_pars[2] 
    beta_1 = fix_pars[3] 
    beta_2 = fix_pars[4]
    delta_1 = fix_pars[5]
    delta_2 = fix_pars[6]
    lambda = fix_pars[7]
    mu = fix_pars[8]
    mu_c = fix_pars[9]
    De = fix_pars[10]
    Dr = fix_pars[11]
    f = fix_pars[12]
    N = fix_pars[13]
    ## old values
    S = old_values[1]
    E = old_values[2]
    U = old_values[3]
    P = old_values[4]
    F = old_values[5]
    RU = old_values[6]
    RR = old_values[7]
    DU = old_values[8]
    DR = old_values[9] 
    ## new values
    S_new = S - b * S * (alpha_p * P + alpha_u * U + F) / N + lambda * N - mu * S
    E_new = E + b * S * (alpha_p * P + alpha_u * U + F) / N - E / De - mu * E
    U_new = U + (1 - r) * E / De - U / (beta_1 * Dr) - delta_1 * mu_c * U - mu * U
    P_new = P + r * (1 - f) * E / De - P / Dr - mu_c * P - mu * P
    F_new = F + r * f * E / De - F * beta_2 / Dr - mu_c * F / delta_2 - mu * F
    RU_new <- RU + U / (beta_1 * Dr) + F * beta_2 / Dr - mu * RU
    RR_new <- RR + P / Dr - mu * RR
    DU_new <- DU + delta_1 * mu_c * U + mu_c * F / delta_2
    DR_new <- DR + mu_c * P 
    est_P_new_n <- E
    est_P_new_prob <- r * (1 - f) / De 
    est_RD_new_n <- P
    est_RD_new_prob_R <- 1 / Dr 
    est_RD_new_prob_D <- mu_c 
    
    return(c(S_new, E_new, U_new, P_new, F_new, RU_new, RR_new, DU_new, DR_new, 
             est_P_new_n, est_P_new_prob,
             est_RD_new_n, est_RD_new_prob_R, est_RD_new_prob_D))
  }
  ## matrix for save the results
  ymat = matrix(0, length(times), length(init_obs) + 6)
  ymat[, 1] = times
  colnames(ymat) <- c("time", names(init_obs), "est_p_n", "est_p_prob", "est_RD_n", "est_RD_prob_R", "est_RD_prob_D")
  ## stage 1
  mystage_pars <- c(b = pars[1], r = pars[8])
  for(i in 1:10) {  
    if(i == 1) {
      myold_values <- init_obs
    } else {
      myold_values <- ymat[i - 1, 2:10]
    }
    ymat[i, 2:15] <- myode(stage_pars = mystage_pars, fix_pars = myfix_pars, old_values = myold_values)
  }
  ## stage 2
  mystage_pars <- c(b = pars[2], r = pars[9])
  for(i in 11:31) { 
    myold_values <- ymat[i - 1, 2:10]
    ymat[i, 2:15] <- myode(stage_pars = mystage_pars, fix_pars = myfix_pars, old_values = myold_values)
  }
  ## stage 3
  mystage_pars <- c(b = pars[3], r = pars[10])
  for(i in 32:50) {
    myold_values <- ymat[i - 1, 2:10]
    ymat[i, 2:15] <- myode(stage_pars = mystage_pars, fix_pars = myfix_pars, old_values = myold_values)
  }
  ## stage 4
  mystage_pars <- c(b = pars[4], r = pars[11])
  for(i in 51:64) {
    myold_values <- ymat[i - 1, 2:10]
    ymat[i, 2:15] <- myode(stage_pars = mystage_pars, fix_pars = myfix_pars, old_values = myold_values)
  }
  ## stage 5
  mystage_pars <- c(b = pars[5], r = pars[12])
  for(i in 65:78) {
    myold_values <- ymat[i - 1, 2:10]
    ymat[i, 2:15] <- myode(stage_pars = mystage_pars, fix_pars = myfix_pars, old_values = myold_values)
  }
  ## stage 6
  mystage_pars <- c(b = pars[6], r = pars[13])
  for(i in 79:97) {
    myold_values <- ymat[i - 1, 2:10]
    ymat[i, 2:15] <- myode(stage_pars = mystage_pars, fix_pars = myfix_pars, old_values = myold_values)
  }
  ## stage 7
  mystage_pars <- c(b = pars[7], r = pars[14])
  for(i in 98:length(times)) {
    myold_values <- ymat[i - 1, 2:10]
    ymat[i, 2:15] <- myode(stage_pars = mystage_pars, fix_pars = myfix_pars, old_values = myold_values)
  }
  
  return(ymat)
}