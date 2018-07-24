



extract.sarlm2 <- function(model, imp, include.nobs = TRUE, include.loglik = TRUE,
                          include.aic = TRUE, include.lr = TRUE, include.wald = TRUE, ...) {
  s <- summary(model, ...)


  names <- rownames(s$Coef)
  cf <- s$Coef[, 1]
  se <- s$Coef[, 2]
  p <- s$Coef[, ncol(s$Coef)]
  

    
    
  
  # replace coef, se and pvalues with impacts
    cf[2:length(cf)] <- imp$res$total # total impacts
    p[2:length(cf)] <- imp$pzmat[,3]  # total impacts, p-values
    
    z_values <- imp$zmat[,3] # total impacts, Z-values
    
    se[2:length(cf)] <- imp$res$total  / z_values # SE total impacts
    
    -0.043003971030 / -3.005433145
    0.01430874318
    
    -0.041796705005 / -3.626841824
    0.01152427016
    
    
    # > -0.041796705005 / -3.788060081
    # [1] 0.01103380203
    # > -0.002014654315 / -5.933739385
    # [1] 0.0003395252444
    
    
  if (model$type != "error") {  # include coefficient for autocorrelation term
    rho <- model$rho
    cf <- c(cf, rho)
    names <- c(names, "$\\rho$")
    if (!is.null(model$rho.se)) {
      if (!is.null(model$adj.se)) {
        rho.se <- sqrt((model$rho.se^2) * model$adj.se)
      } else {
        rho.se <- model$rho.se
      }
      rho.pval <- 2 * (1 - pnorm(abs(rho / rho.se)))
      se <- c(se, rho.se)
      p <- c(p, rho.pval)
    } else {
      se <- c(se, NA)
      p <- c(p, NA)
    }
  }
  
  if (!is.null(model$lambda)) {
    cf <-c(cf, model$lambda)
    names <- c(names, "$\\lambda$")
    if (!is.null(model$lambda.se)) {
      if (!is.null(model$adj.se)) {
        lambda.se <- sqrt((model$lambda.se^2) * model$adj.se)
      } else {
        lambda.se <- model$lambda.se
      }
      lambda.pval <- 2 * (1 - pnorm(abs(model$lambda / lambda.se)))
      se <- c(se, lambda.se)
      p <- c(p, lambda.pval)
    } else {
      se <- c(se, NA)
      p <- c(p, NA)
    }
  }
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  
  if (include.nobs == TRUE) {
    rs <- summary(model, Nagelkerke=T) %>% .$NK # NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW
    n <- length(s$fitted.values)
    param <- s$parameters
    gof <- c(gof, rs, n, param)                                         # changed
    gof.names <- c(gof.names, "Pseudo R2", "Num.\ obs.", "Parameters")  # changed
    gof.decimal <- c(gof.decimal, TRUE, FALSE, FALSE)                   # changed
  }
  
  if (include.loglik == TRUE) {
    ll <- s$LL
    gof <- c(gof, ll)
    gof.names <- c(gof.names, "Log Likelihood")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.aic == TRUE) {
    aic <- AIC(model)
    aiclm <- s$AIC_lm.model
    gof <- c(gof, aiclm, aic)
    gof.names <- c(gof.names, "AIC (Linear model)", "AIC (Spatial model)")
    gof.decimal <- c(gof.decimal, TRUE, TRUE)
  }
  if (include.lr == TRUE && !is.null(s$LR1)) {
    gof <- c(gof, s$LR1$statistic[[1]], s$LR1$p.value[[1]])
    gof.names <- c(gof.names, "LR test: statistic", "LR test: p-value")
    gof.decimal <- c(gof.decimal, TRUE, TRUE)
  }
  if (include.wald == TRUE && !is.null(model$Wald1)) {
    waldstat <- model$Wald1$statistic
    waldp <- model$Wald1$p.value
    gof <- c(gof, waldstat, waldp)
    gof.names <- c(gof.names, "Wald test: statistic", "Wald test: p-value")
    gof.decimal <- c(gof.decimal, TRUE, TRUE)
  }
  
  tr <- createTexreg(
    coef.names = names[-1], # remove intercept
    coef = cf[-1],          # remove intercept
    se = se[-1],
    pvalues = p[-1],
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}


# refs
# https://github.com/leifeld/texreg
# https://www.jstatsoft.org/article/view/v055i08
# https://stackoverflow.com/questions/38894044/print-beautiful-tables-for-h2o-models-in-r
# 
# https://stackoverflow.com/questions/45971419/texreg-table-for-impacts-of-spatial-lag-model








