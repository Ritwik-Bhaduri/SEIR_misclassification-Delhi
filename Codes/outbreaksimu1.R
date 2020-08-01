## Simulation based on deterministic SEIR model
## six periods: March 15-24, March 25-April 14, April 15-May 3, May 4-May 17, May 18- May 31, June 1-June 25
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


fun_seir <- function(init_obs, myfix_pars, pars, t_simu) {
  ## fun_sample function
  fun_sample <- function(stage_pars, fix_pars, old_values) {
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
    ##
    
    pS_vec <- c( b * (alpha_p * P + alpha_u * U + F) / N , mu, 1 -  b * (alpha_p * P + alpha_u * U + F) / N - mu)
    sample_S <- rmultinom(1, size = S, prob = pS_vec)
    ##
    pE_vec <- c((1-r) / De, r * (1-f) / De, r*f/De, mu,1 - 1 / De - mu)
    sample_E <- rmultinom(1, size = E, prob = pE_vec)
    ##
    pU_vec <- c(1/(beta_1*Dr),delta_1*mu_c,mu,1-1/(beta_1*Dr)-delta_1*mu_c-mu)
    
    sample_U <- rmultinom(1, size = U, prob = pU_vec)
    ##
    pP_vec <- c(1 / Dr, mu_c,mu, 1 - 1 / Dr - mu-mu_c)
    sample_P <- rmultinom(1, size = P, prob = pP_vec)
    ##
    pF_vec <- c(beta_2 / Dr, mu_c/delta_2,mu,1 -beta_2 / Dr- mu_c/delta_2-mu)
    sample_F <- rmultinom(1, size = F, prob = pF_vec)
    ##
    pRU_vec <- c( mu,1-mu)
    sample_RU <- rmultinom(1, size = RU, prob = pRU_vec)
    ##
    pRR_vec <- c(mu,1-mu)
    sample_RR <- rmultinom(1, size = RR, prob = pRR_vec)
    ## new values
    S_new <- sample_S[3] + lambda*N
    E_new <- sample_E[5] + sample_S[1]
    U_new <- sample_U[4] + sample_E[1]
    P_new <- sample_P[4] + sample_E[2]
    F_new <- sample_F[4] + sample_E[3]
    RU_new <- sample_RU[2] + sample_U[1]+sample_F[1]
    RR_new <- sample_RR[2] + sample_P[1]
    DU_new <- DU + sample_U[2]+sample_F[2]
    DR_new <- DR + sample_P[2]
    est_P_new<- sample_E[2]
    est_RR_new<- sample_P[1]
    est_DR_new <- sample_P[2]
    
    
    ##
    return(c(S_new, E_new, U_new, P_new, F_new, RU_new, RR_new, DU_new, DR_new, 
             est_P_new,est_RR_new,est_DR_new))
  }
  ########  ## 7 periods: March 15-24, March 25-April14, April15-May3, May4-May17, May18-May31, June1-June19, June20-June30 ########
  ## fixed pars
  ## fixed parameters
  results = matrix(nrow = 1:t_simu, ncol = 1:12)
  mystage_pars <- c(b = pars[1], r = pars[8])
  for(i in 1:10) {  
    if(i == 1) {
      myold_values <- init_obs
      results = matrix(c(init_obs,32,0,0), nrow = 1)
    } else {
      myold_values <- mynow_values[1:9]
    }
    mynow_values <- fun_sample(stage_pars = mystage_pars, fix_pars = myfix_pars, old_values = myold_values)
    results = rbind(results, mynow_values)
  }
  ## stage 2
  mystage_pars <- c(b = pars[2], r = pars[9])
  for(i in 11:31) { 
    myold_values <- mynow_values[1:9]
    mynow_values <- fun_sample(stage_pars = mystage_pars, fix_pars = myfix_pars, old_values = myold_values)
    results = rbind(results, mynow_values)
  }
  ## stage 3
  mystage_pars <- c(b = pars[3], r = pars[10])
  for(i in 32:50) {
    myold_values <- mynow_values[1:9]
    mynow_values <- fun_sample(stage_pars = mystage_pars, fix_pars = myfix_pars, old_values = myold_values)
    results = rbind(results, mynow_values)
  }
  ## stage 4
  mystage_pars <- c(b = pars[4], r = pars[11])
  for(i in 51:64) {
    myold_values <- mynow_values[1:9]
    mynow_values <- fun_sample(stage_pars = mystage_pars, fix_pars = myfix_pars, old_values = myold_values)
    results = rbind(results, mynow_values)
  }
  ## stage 5
  mystage_pars <- c(b = pars[5], r = pars[12])
  for(i in 65:78) {
    myold_values <- mynow_values[1:9]
    mynow_values <- fun_sample(stage_pars = mystage_pars, fix_pars = myfix_pars, old_values = myold_values)
    results = rbind(results, mynow_values)
  }
  ## stage 6
  mystage_pars <- c(b = pars[6], r = pars[13])
  for(i in 79:97) {
    myold_values <- mynow_values[1:9]
    mynow_values <- fun_sample(stage_pars = mystage_pars, fix_pars = myfix_pars, old_values = myold_values)
    results = rbind(results, mynow_values)
  }
  ## stage 7
  mystage_pars <- c(b = pars[7], r = pars[14])
  for(i in 98:t_simu) {
    myold_values <- mynow_values[1:9]
    mynow_values <- fun_sample(stage_pars = mystage_pars, fix_pars = myfix_pars, old_values = myold_values)
    results = rbind(results, mynow_values)
  }
  return(results)
}