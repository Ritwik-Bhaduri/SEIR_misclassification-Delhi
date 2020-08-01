## Ro for the 7 period
## 7 periods: March 15-24, March 25-April14, April15-May3, May4-May17, May18-May31, June1-June19, June20-June30
#' @param predat            data frame of the prediction (time, S, E, U, P, F, RU, RR, DU, DR, est_P_new, est_RR_new, est_DR_new) using fun_seir/fun_pred
#' @param estpar            estimates of the parameters (b1, b2, b3, b4, b5, b6, b7 , r1, r2, r3, r4, r5, r6, r7)
#' @param alpha_p           ratio of transmission rates of ascertained cases over false negative cases
#' @param alpha_u           ratio of transmission rates of unascertained cases over false negative cases
#' @param beta_1            ratio of time till recovery of unascertained cases over ascertained cases
#' @param beta_2            ratio of time till recovery of ascertained cases over false negative cases
#' @param delta_1           ratio of death rate of unascertained cases over ascertained cases
#' @param delta_2           ratio of death rate recovery of ascertained cases over false negative cases
#' @param lambda            natural birth rate
#' @param mu                natural death rate
#' @param mu_c              death rate of ascertained cases
#' @param De                latent period / incubation period
#' @param Dr                Average days till recovery for ascertained cases
#' @param f                 false negative rate
#' @param b                 transmission rate of ascertained cases
#' @param r                 ascertainment rate

fun_R0 <- function(estpar, myfix_pars) {
  alpha_p = myfix_pars["alpha_p"]
  alpha_u = myfix_pars["alpha_u"]
  beta_1 = myfix_pars["beta_1"]
  beta_2 = myfix_pars["beta_2"]
  delta_1 = myfix_pars["delta_1"]
  delta_2 = myfix_pars["delta_2"]
  lambda = myfix_pars["lambda"]
  mu = myfix_pars["mu"]
  mu_c = myfix_pars["mu_c"]
  De = myfix_pars["De"]
  Dr = myfix_pars["Dr"]
  f = myfix_pars["f"]
  N = myfix_pars["N"]
 
  
  ## 15 March - 24 March
  
  b <- estpar[1]
  r <- estpar[8]
  R01 <- (b / (mu * De + 1)) * (alpha_u * (1 - r) / (1 / (beta_1 * Dr) + delta_1 * mu_c + mu)  + 
                                alpha_p * r * (1 - f) / (1 / Dr + mu_c + mu) + 
                                r * f / (beta_2 / Dr + mu_c / delta_2 + mu))
    
  ## 25 March - 14 April
 
  b <- estpar[2]
  r <- estpar[9]
  R02 <- (b / (mu * De + 1)) * (alpha_u * (1 - r) / (1 / (beta_1 * Dr) + delta_1 * mu_c + mu)  + 
                                  alpha_p * r * (1 - f) / (1 / Dr + mu_c + mu) + 
                                  r * f / (beta_2 / Dr + mu_c / delta_2 + mu))
  
  ## 15 April - 3 May
 
  b <- estpar[3]
  r <- estpar[10]
  R03 <- (b / (mu * De + 1)) * (alpha_u * (1 - r) / (1 / (beta_1 * Dr) + delta_1 * mu_c + mu)  + 
                                  alpha_p * r * (1 - f) / (1 / Dr + mu_c + mu) + 
                                  r * f / (beta_2 / Dr + mu_c / delta_2 + mu))
  
  ## 4 May - 17 May
  
  b <- estpar[4]
  r <- estpar[11]
  R04 <- (b / (mu * De + 1)) * (alpha_u * (1 - r) / (1 / (beta_1 * Dr) + delta_1 * mu_c + mu)  + 
                                  alpha_p * r * (1 - f) / (1 / Dr + mu_c + mu) + 
                                  r * f / (beta_2 / Dr + mu_c / delta_2 + mu))
  ## 18 May - 31 May
  
  b <- estpar[5]
  r <- estpar[12]
  R05 <- (b / (mu * De + 1)) * (alpha_u * (1 - r) / (1 / (beta_1 * Dr) + delta_1 * mu_c + mu)  + 
                                  alpha_p * r * (1 - f) / (1 / Dr + mu_c + mu) + 
                                  r * f / (beta_2 / Dr + mu_c / delta_2 + mu))
  ## 1 June - 19 June
  
  b <- estpar[6]
  r <- estpar[13]
  R06 <- (b / (mu * De + 1)) * (alpha_u * (1 - r) / (1 / (beta_1 * Dr) + delta_1 * mu_c + mu)  + 
                                  alpha_p * r * (1 - f) / (1 / Dr + mu_c + mu) + 
                                  r * f / (beta_2 / Dr + mu_c / delta_2 + mu))
  ## 20 June - 30 June
  
  b <- estpar[7]
  r <- estpar[14]
  R07 <- (b / (mu * De + 1)) * (alpha_u * (1 - r) / (1 / (beta_1 * Dr) + delta_1 * mu_c + mu)  + 
                                  alpha_p * r * (1 - f) / (1 / Dr + mu_c + mu) + 
                                  r * f / (beta_2 / Dr + mu_c / delta_2 + mu))
  
  
  #
  R0 <- c(mean(R01), mean(R02), mean(R03), mean(R04), mean(R05), mean(R06), mean(R07))
  return(R0)
}