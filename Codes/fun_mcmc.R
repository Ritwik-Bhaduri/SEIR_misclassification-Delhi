## mcmc using the Metropolis-Hastings algorithm
#' @param init_pars               initial parameters c(b1, b2, b3, b4, b5, b6, r1, r2, r3, r4, r5, r6)
#' @param step_pars               a vector of sd of the parameter moving steps
#' @param init_state_num          a vetor of initial states c(S = S0, E = E0, U = U0, P = P0, F = F0, RU = RU0, RR = RR0, DU = DU0, DR = DR0)
#' @param observed_num            case numbers with onset date from March 15 to June 25
#' @param niter                   number of mcmc iterations after burn in
#' @param BurnIn                  number of mcmc iterations for burn in
#' @param trace_num               step size of mcmc              
#' @param runMLE                  whether run MLE to provide initial values of the parameters

fun_mcmc <- function(init_pars, step_pars, init_state_num, observed_num, niter = 1000000, BurnIn = 200000, trace_num = 100, runMLE = T, myfix_pars = myfix_pars, opt_num = 3){
  ## use mle to set the initial pars for mcmc
  if(runMLE) {
    mle_optim <- fun_mle(init_pars = init_pars, init_state_num = init_state_num, observed_num = observed_num, opt_num = opt_num)
    mle_pars <- mle_optim
    init_pars <- mle_pars[3:10]
    cat("The MLE estimates: ", round(init_pars, digits=4), fill = T)
  } else {
    mle_pars <- NULL
    init_pars <- init_pars
  }
  ## function for pars
  ## pars = c(b1, b2, b3, b4, b5, b6, b7, r1, r2, r3, r4, r5, r6, r7)
  Prior_func <- function(pars){
    if(any(pars < 0) || any(pars[7:12] > 1)){
      return (0)
    }else{
      return (1) ## Set a non-informative prior
    }
  }
  ## function for log likelihood
  LL_func <- function(pars){
    ypred <- fun_pred(init_obs = init_state_num, times = ftime, pars = pars, myfix_pars = myfix_pars)
    p_new_pred_n <- round(ypred[, 11])
    p_new_pred_prob <- ypred[, 12]
    rd_new_pred_n <- round(ypred[, 13])
    r_new_pred_prob <- ypred[, 14]
    d_new_pred_prob <- ypred[, 15]
    p <- c()
    for(i in 1:dim(ypred)[1]){
      p[i] = dbinom(observed_num[i, "obsP"], round(p_new_pred_n[i]) , p_new_pred_prob[i], log = T)
      obs_size = observed_num[max(i - 1, 1), "obsP_current"]
      pred_size = round(ypred[max(i - 1, 1),"P"])
      if(sum(observed_num[i, c("obsR", "obsD")]) > pred_size){
        p[i] = -Inf
      }
      else{
        p[i] = p[i] + dmultinom(c(round(observed_num[i, c("obsR", "obsD")]), pred_size - sum(round(observed_num[i, c("obsR", "obsD")]))),
                                pred_size, c(r_new_pred_prob[i], d_new_pred_prob[i], 1-r_new_pred_prob[i]-d_new_pred_prob[i]),
                                log = T)
      }
    }
    if(any(p == -Inf) || any(is.nan(p))){
      logL <- -Inf
    }else{
      logL <- sum(p)
    }
    return(logL)
  }

  ## R0 estimation
  ftime <- 1:dim(observed_num)[1]
  
  ## We are predicting for our train period plus 16 days extra
  R0_est <- fun_R0(estpar = init_pars, myfix_pars = myfix_pars)
  ## build the matrix to store the results
  np <- length(init_pars)
  pmat <- matrix(0, ((niter + BurnIn) / trace_num) + 1, np+7) ## parameters + R0 for 7 periods
  colnames(pmat) <- c("b1", "b2", "b3", "b4","b5","b6","b7","r1", "r2", "r3", "r4","r5","r6","r7", "R01", "R02", "R03", "R04","R05","R06","R07")
  pmat[1, 1:np] <- init_pars
  pmat[1, (np+1):(np+7)] <- R0_est

  ## Begin MCMC
  pars_now <- init_pars
  
  cat("MCMC:", fill = T) 
  for(i in 2:(niter + BurnIn)){
    pars_new <- rep(0, np)
    for(j in 1:np){
      pars_new[j] <- rnorm(1, mean = pars_now[j], sd = step_pars[j])
    }
    A <- 0
    if(Prior_func(pars_new) > 0){ 
      ll_pars_new <- LL_func(pars = pars_new)
      if(ll_pars_new != -Inf) {
        ll_pars_now <- LL_func(pars = pars_now)
        A <-  exp(1)^(ll_pars_new - ll_pars_now) # Prior_func(pars_new) / Prior_func(pars_now) * 10^(ll_pars_new - ll_pars_now)
      }else{
        cat(i, " logL =", ll_pars_new, fill = T)
      }
    }
    
    if(runif(1) < A){
      pars_now <- pars_new
    }
    if(i %% trace_num == 0) {
      R0_est <- fun_R0(estpar = pars_now, myfix_pars = myfix_pars)
      pmat[(i / trace_num) + 1, 1:np] <- pars_now
      pmat[(i / trace_num) + 1, (np+1):(np+7)] <- R0_est
      
    }
    if(i%%(niter/10) == 0) cat("Iter", i, " A =", round(A, digits=5), " : ", round(pars_now, digits=4), fill = T)
    
  }
  est_list <- list(mle_estimates = mle_pars, mcmc_estimates = pmat[-c(1:(BurnIn / trace_num + 1)), ])
  ## print output
  
  mcmc_estimates = pmat[-c(1:(BurnIn / trace_num + 1)), ]
  cat("########################   Brief Summary    #####################################", fill = T)
  cat("##           b1    b2    b3    b4   b5   b6   b7   r1    r2    r3    r4    r5   r6   r7   R01    R02    R03    R04    R05    R06    R07", fill = T)
  cat("##  Mean :", round(apply(mcmc_estimates, 2, mean), 3), fill = T)
  cat("##  2.5% :", round(apply(mcmc_estimates, 2, function(x) quantile(x, 0.025)), 3), fill = T)
  cat("## 97.5% :", round(apply(mcmc_estimates, 2, function(x) quantile(x, 0.975)), 3), fill = T)
  cat("###############################   Done    ########################################", fill = T)
  return(est_list)
} 
