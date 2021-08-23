# BoundedReg: an R package for modelling continuous bounded data --------------
# Author: Ricardo Rasmussen Petterle/UFPR -------------------------------------
<<<<<<< HEAD
# Date: 23/08/2021 -----------------------------------------------------------
=======
# Date: 23/08/2021 ------------------------------------------------------------
>>>>>>> 32163e0dcfc4bde8e4f5df6fa036525a56f8c66b

# Distributions ---------------------------------------------------------------
# Kumaraswamy -----------------------------------
dkumar_br <- function(x, mu, phi, log = FALSE){
  b <- log(0.5)/log(1 - mu^phi) 
  out <- extraDistr::dkumar(x = x, a = phi, b = b, log = log)
  return(out)
}
# -----------------------------------------------
pkumar_br <- function(q, mu, phi, lower.tail = TRUE, log.p = FALSE){
  b <- log(0.5)/log(1 - mu^phi) 
  out <- extraDistr::pkumar(q = q, a = phi, b = b, lower.tail = lower.tail, log.p = log.p)
  return(out)
}
# -----------------------------------------------
qkumar_br <- function(p, mu, phi, lower.tail = TRUE, log.p = FALSE){
  b <- log(0.5)/log(1 - mu^phi) 
  out <- extraDistr::qkumar(p = p, a = phi, b = b, lower.tail = lower.tail, log.p = log.p)
  return(out)
}
# -----------------------------------------------
rkumar_br <- function(n, mu, phi){
  b <- log(0.5)/log(1 - mu^phi) 
  out <- extraDistr::rkumar(n = n, a = phi, b = b)
  return(out)
}
# -----------------------------------------------


# Random number generator: unit gamma1
rUG <- function(n, mu, sigma){
  if (any(mu <= 0) || any(mu >= 1)) 
    stop(paste("mu must be between 0 and 1", "\n", ""))
  if (any(sigma < 0)) stop(paste("sigma must be positive", "\n", "")) 
  if (any(n <= 0)) stop(paste("n must be a positive integer", "\n", "")) 
  alpha <- (mu^(1/sigma))/(1 - mu^(1/sigma))
  rg <- rgamma(n, shape = sigma, scale = 1/alpha)
  fy <- exp(-rg)
  return(fy)
}

# Link functions --------------------------------------------------------------
# Logit
inv_logit <- function(eta){
  mu <- 1 / ( 1 + exp(-eta))
  return(mu)
}

# Probit
inv_probit <- function(eta){
  mu <- pnorm(eta)
  return(mu)
}

# Clog-log
inv_cloglog <- function(eta){
  mu <- 1 - exp(-exp(eta))
  return(mu)
}

# Log
inv_log <- function(eta){
  mu <- exp(eta)
  return(mu)
}

# Log-Log
inv_loglog <- function(eta){
  mu <- exp(-exp(-eta))
  return(mu)
}

# Link functions for mu
link_mu <- function(par, y, X, link.mu){
  n_beta <- ncol(X)
  beta <- par[1:n_beta]
  eta <- X%*%beta
  if(link.mu == "logit") mu <- inv_logit(eta)
  if(link.mu == "probit") mu <- inv_probit(eta)
  if(link.mu == "loglog") mu <- inv_loglog(eta)
  if(link.mu == "cloglog") mu <- inv_cloglog(eta)
  if(link.mu == "cauchit") mu <- VGAM::cauchit(eta, inverse = TRUE)
  return(mu)
}

# Link functions for phi
link_phi <- function(par, y, X, Z, link.phi){
  n_beta <- ncol(X)
  n_phi <- ncol(Z)
  phi <- par[1:n_phi + n_beta]
  eta <- Z%*%phi
  if(link.phi == "log") phi <- inv_log(eta)
  if(link.phi == "logit") phi <- inv_logit(eta)
  return(phi)
}
# -----------------------------------------------------------------------------

model_print <- function(object){
  if(object$model == "beta" || object$model == "BE") model <- "Beta"
  if(object$model == "simplex" || object$model == "SIM") model <- "Simplex"
  if(object$model == "unitgamma1" || object$model == "UG1") model <- "Unit gamma 1"
  if(object$model == "unitgamma2" || object$model == "UG2") model <- "Unit gamma 2"
  if(object$model == "kumaraswamy" || object$model == "KW") model <- "Kumaraswamy"
  if(object$model == "unitweibull" || object$model == "UW") model <- "Unit Weibull"
  if(object$model == "bessel" || object$model == "BS") model <- "Bessel"
  if(object$model == "johnsonSB" || object$model == "JSB") model <- "Johnson SB"
  if(object$model == "kumaraswamy2" || object$model == "KW2") model <- "Kumaraswamy 2"
  if(object$model == "cunitweibull" || object$model == "CUW") model <- "Complementary unit Weibull"
  if(object$model == "unitgompertz" || object$model == "UGO") model <- "Unit Gompertz"
  if(object$model == "cunitgompertz" || object$model == "CUGO") model <- "Complementary unit Gompertz"
  if(object$model == "cunitgamma1" || object$model == "CUG1") model <- "Complementary unit gamma 1"
  if(object$model == "unitburrXII" || object$model == "UBXII") model <- "Unit Burr-XII"
  if(object$model == "unitgammaMode" || object$model == "UGMode") model <- "Unit gamma mode" 
  if(object$model == "vasicek" || object$model == "VAS") model <- "Vasicek" 
  if(object$model == "leeg" || object$model == "LEEG") model <- "Log-extended exponential-geometric" 
  return(model)
}


print.boundedReg <- function(x,
                             digits = max(3, getOption("digits") - 3),
                             ...) {
  cat("\n ", sep = "")
  fcall <- gsub(", ", ",\n           ",
                deparse(x$call, width.cutoff = 500))
  cat("\nCall:  ",
      paste(fcall, sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  cat(paste("Mu coefficients (with ", x$link.mu, " link):", "\n", sep = ""))
  print.default(format(x$mu_coefs, digits = digits),
                print.gap = 2, quote = FALSE)
  cat("\n")
  if (x$phi.formula == ~ 1) {
    cat("Phi coefficient: log(phi) = ",
        format(x$phi_coefs, digits = digits),
        "\n", sep = "")
  } else {
    # cat("Phi coefficients (with log link):", "\n", sep = "")
    cat(paste("Phi coefficients (with ", x$link.phi, " link):", "\n", sep = ""))
    print.default(format(x$phi_coefs, digits = digits),
                  print.gap = 2, quote = FALSE)
  }
  cat("\n")
  cat("Model: ", model_print(x), "\n", sep = "")
  cat("Number of observation: ", x$nobs, "\n", sep = "")
  cat("LogLik: ", x$loglik, "\n", sep = "")
  cat("AIC: ", x$AIC, "\n", sep = "")
  cat("BIC: ", x$BIC, "\n", sep = "")
  invisible(x)
}
# -----------------------------------------------------------------------------

print.summary.boundedReg <- function(x,
                                     digits = max(3, getOption("digits") - 3),
                                     ...) {
  cat("\n ",
      sep = "")
  fcall <- gsub(", ", ",\n           ",
                deparse(x$call, width.cutoff = 500))
  cat("\nCall:  ",
      paste(fcall, sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  cat(paste("Mu coefficients (with ", x$link.mu, " link):", "\n", sep = ""))
  printCoefmat(x$coeftable$mean,
               digits = digits,
               has.Pvalue = TRUE)
  cat("\n")
  # cat("Phi coefficients (with log link):", "\n", sep = "")
  cat(paste("Phi coefficients (with ", x$link.phi, " link):", "\n", sep = ""))
  printCoefmat(x$coeftable$phi,
               digits = digits,
               has.Pvalue = TRUE)
  cat("\n")
  cat("Model: ", model_print(x), "\n", sep = "")
  cat("Number of observation: ", x$nobs, "\n", sep = "")
  cat("LogLik: ", x$loglik, "\n", sep = "")
  cat("AIC: ", x$AIC, "\n", sep = "")
  cat("BIC: ", x$BIC, "\n", sep = "")
  invisible(x)
}
# -----------------------------------------------------------------------------

summary.boundedReg <- function(object, ...) {
  if (is.null(object$vcov))
    stop(paste("Refit the model with `hessian=TRUE` to compute",
               "the standard errors."))
  #------------------------------------------
  n_beta <- ncol(object$data$X)
  n_phi <- ncol(object$data$Z)
  #------------------------------------------
  estimates  <- object$coefficients
  std_error   <- sqrt(diag(object$vcov))
  z_value     <- estimates/std_error
  p_value     <- 2 * pnorm(-abs(z_value))
  ctableall  <- cbind("Estimate"    = estimates,
                      "Std. Error"  = std_error,
                      "Z value"     = z_value,
                      "Pr(>|Z|)"    = p_value)
  ctable <- list(mean = ctableall[1:n_beta, ,drop = FALSE],
                 phi = ctableall[1:n_phi + n_beta, ,drop = FALSE])
  rownames(ctable$mean) <- colnames(object$data$X)
  rownames(ctable$phi) <- colnames(object$data$Z)
  out <- list(coeftable   = ctable,
              model       = object$model, 
              link.mu     = object$link.mu,
              link.phi     = object$link.phi,
              loglik      = object$loglik,
              AIC         = object$AIC, 
              BIC         = object$BIC,
              nobs        = object$nobs,
              call        = object$call)
  class(out) <- "summary.boundedReg"
  return(out)
}

# LogLik for boundedReg object
#logLik.boundedReg <- function(object, ...) {
#  if (!missing(...))
#    warning("extra arguments discarded")
#  ll <- object$loglik
#  attr(ll, "df") <- object$nobs - object$df.residual
#  attr(ll, "nobs") <- object$nobs
#  class(ll) <- "logLik"
#  return(ll)
#}

# LogLik for boundedReg object ------------------------------------------------
logLik.boundedReg <- function(object, ...) {
  structure(object$loglik, df = sum(sapply(object$coefficients, length)), class = "logLik")
}

# AIC for boundedReg object
AIC.boundedReg <- function(object, ...) {
  if (!missing(...))
    warning("extra arguments discarded")
  aic <- object$AIC
  class(aic) <- "AIC"
  return(object$AIC)
}

# BIC for boundedReg object ---------------------------------------------------
BIC.boundedReg <- function(object, ...) {
  if (!missing(...))
    warning("extra arguments discarded")
  bic <- object$BIC
  class(bic) <- "BIC"
  return(object$BIC)
}

# vcov for boundedReg object --------------------------------------------------
vcov.boundedReg <- function(object, ...) {
  if (!missing(...))
    warning("extra arguments discarded")
  return(object$vcov)
}

# Goodness-of-Fit for boundedReg object ---------------------------------------
br_gof <- function (object, digits = 3) {
  ll <- logLik(object)
  AIC <- AIC(object)
  BIC <- BIC(object)
  df <- sum(sapply(object$coefficients, length))
  nobs <- object$nobs
  out <- data.frame(LogLik = ll, 
                    AIC = AIC, 
                    BIC = BIC,
                    Df = df, 
                    Nobs = nobs)
  cat("\nGoodness-of-Fit for", model_print(object), "regression model \n")
  return(round(out, digits = digits))
}

# Compare models for boundedReg object ----------------------------------------
br_compare <- function(...){
  names <- as.character(match.call())[-1]
  
  objects <- list(...)
  
  df <- sapply(objects, function(x) attr(logLik(x), "df"))
  llik <- sapply(objects, logLik)
  aic <- sapply(objects, AIC)
  bic <- sapply(objects, BIC)
  out <- data.frame(Df = df, LogLik = llik, AIC = aic, BIC = bic, row.names = names)
  return(out)
}

# Model matrix for boundedReg object ------------------------------------------
model.matrix.boundedReg <- function(object, ...) {
  if (!missing(...))
    warning("extra arguments discarded")
  list(X = object$data$X, Z = object$data$Z)
}

# Coef for boundedReg object --------------------------------------------------
coef.boundedReg <- function(object,
                        what = c("mean", "phi", "all"), ...) {
  if (!missing(...))
    warning("extra arguments discarded")
  what <- match.arg(what)
  out <- switch(what,
                "all"        = list(
                  mean       = object$mu_coefs,
                  phi        = object$phi_coefs),
                 "mean"       = object$mu_coefs, 
                 "phi"        = object$phi_coefs)
  return(out)
}

# Randomized quantile residual ------------------------------------------------
br_randomized_quantile <- function(object) {
  X <- object$data$X
  Z <- model.matrix(object)$Z
  # Fitted mu -----------------------------------
  link.mu <- object$link.mu
  mu <- link_mu(par = object$coefficients, 
                y = object$data$y, 
                X = X, link.mu = link.mu)
  # Fitted phi ----------------------------------
  link.phi <- object$link.phi
  fitted_phi <- link_phi(par = object$coefficients, 
                         y = object$data$y, X = X, 
                         Z = Z, link.phi = link.phi)
  y <- object$data$y
  model <- object$model
  if(model == "beta" || model == "BE") out <- qnorm(pbeta(y, mu*fitted_phi, (1-mu)*fitted_phi, lower.tail = TRUE, log = FALSE))
  if(model == "simplex" || model == "SIM") out <- qnorm(rmutil::psimplex(y, m = mu, s = fitted_phi))
  if(model == "unitgamma1" || model == "UG1") out <- qnorm(pUG(y, mu = mu, sigma = fitted_phi, lower.tail = TRUE, log.p = FALSE))
  if(model == "unitgamma2" || model == "UG2") out <- qnorm(pUG2(y, mu = mu, alpha = fitted_phi, lower.tail = TRUE, log.p = FALSE))
  if(model == "kumaraswamy" || model == "KW") out <- qnorm(pkumar_br(q = y, mu = mu, phi = fitted_phi, lower.tail = TRUE, log.p = FALSE))
  return(out)
}
# -----------------------------------------------------------------------------

# Cox-snell residuals ---------------------------------------------------------
br_cox_snell <- function(object) {
  X <- object$data$X
  Z <- model.matrix(object)$Z
  # Fitted mu -----------------------------------
  link.mu <- object$link.mu
  mu <- link_mu(par = object$coefficients, 
                y = object$data$y, 
                X = X, link.mu = link.mu)
  # Fitted phi ----------------------------------
  link.phi <- object$link.phi
  fitted_phi <- link_phi(par = object$coefficients, 
                         y = object$data$y, X = X, 
                         Z = Z, link.phi = link.phi)
  y <- object$data$y
  model <- object$model
  if(model == "beta" || model == "BE") out <- - pbeta(y, mu*fitted_phi, (1-mu)*fitted_phi, lower.tail = FALSE, log = TRUE)
  if(model == "simplex" || model == "SIM") out <- - log(1-rmutil::psimplex(y, m = mu, s = fitted_phi))
  if(model == "unitgamma1" || model == "UG1") out <- - pUG(y, mu = mu, sigma = fitted_phi, lower.tail = FALSE, log.p = TRUE)
  if(model == "unitgamma2" || model == "UG2") out <- - pUG2(y, mu = mu, alpha = fitted_phi, lower.tail = FALSE, log.p = TRUE)
  if(model == "kumaraswamy" || model == "KW") out <- - pkumar_br(q = y, mu = mu, phi = fitted_phi, lower.tail = FALSE, log.p = TRUE)
  return(out)
}
# -----------------------------------------------------------------------------

# (Half-)normal plots with simulation envelopes for boundedReg object ---------
HNplot <- function(object, envelope = "normal", 
                        xlab, ylab, main, repl, conf = 0.95,
                        pch, col, cex, result = FALSE){
  # Number of repetition ------------------------
  if(missing(repl)) {
    repl <- 100
  }
  # Configuration --------------------------------
  alfa <- (1 - conf)/2
  X <- model.matrix(object)$X
  Z <- model.matrix(object)$Z
  n <- nrow(X)
  p <- ncol(X)
  cs <- br_cox_snell(object)
  temp <- matrix(0,n,repl)
  # Fitted mu ------------------------------------
  link.mu <- object$link.mu
  fitted_mu <- link_mu(par = object$coefficients, 
                       y = object$data$y, X = X, 
                       link.mu = link.mu)
  # Fitted phi -----------------------------------
  link.phi <- object$link.phi
  fitted_phi <- link_phi(par = object$coefficients, 
                         y = object$data$y, X = X, 
                         Z = Z, link.phi = link.phi)
  # Model ----------------------------------------
  br_model <- function(object) {
    X <- object$data$X
    link.mu <- object$link.mu
    mu <- link_mu(par = object$coefficients, 
                  y = object$data$y, X = X, 
                  link.mu = link.mu)
    y <- object$data$y
    model <- object$model
    if(model == "beta" || model == "BE") y_sim <- rbeta(n, fitted_mu*fitted_phi, (1-fitted_mu)*fitted_phi)
    if(model == "simplex" || model == "SIM") y_sim <- rmutil::rsimplex(n, m = fitted_mu, s = fitted_phi)
    if(model == "unitgamma1" || model == "UG1") y_sim <- rUG(n, mu = fitted_mu, sigma = fitted_phi)
    if(model == "unitgamma2" || model == "UG2") y_sim <- rUG2(n, mu = fitted_mu, alpha = fitted_phi)
    if(model == "kumaraswamy" || model == "KW") y_sim <- rkumar_br(n, mu = fitted_mu, phi = fitted_phi)
    return(y_sim)
  }
  # Simulating -----------------------------------
  for(i in 1:repl){
    y_sim <- br_model(object)
    fit <- boundedReg(as.formula(paste("y_sim", 
                                       paste("~", 
                                             paste(object$formula[3], sep = "+")))), 
                      model = object$model, link.mu = link.mu, data = object$data2)
    temp[,i] <- sort(br_cox_snell(fit))
  }
  # ----------------------------------------------
  Z <- array(dim = c(n, 1))
  for(i in 1:n){Z[i] <- qnorm((i + n - 1/8)/(2 * n + 1/2))}
  if(envelope == "Hnormal") {
    temp <- abs(temp)
  } else if(envelope == "normal") temp <- temp
  temp1 <- numeric(n)
  temp2 <- numeric(n)
  # ----------------------------------------------
  for(i in 1:n){
    temp_ord<- sort(temp[i,])
    temp1[i] <- quantile(temp_ord, alfa)   
    temp2[i] <- quantile(temp_ord, 1-alfa) 
    }
  # ----------------------------------------------
  med <- apply(temp,1,median)
  faixa <- range(cs,temp1,temp2)
  
  if(envelope == "normal") {
    # Plot configuration - normal ------------------------------------------------
    par(mar=c(2.5, 2.6, 1.2, 0.5), mgp = c(1.6, 0.6, 0))
    if(missing(xlab)) {
      xlab <- "Theoretical quatilies"
    } 
    if(missing(ylab)) {
      ylab <- "Cox-Snell residuals"
    }
    if(missing(main)) {
      main <- ""
    }
    if(missing(pch)) {
      pch <- 4
    }
    if(missing(col)) {
      col <- "grey50"
    }
    if(missing(cex)) {
      cex <- 0.5
    }
    qqnorm(cs, xlab = xlab, ylab = ylab, ylim = faixa, pch = pch, main = main, col = col, cex = cex)
    par(new=TRUE)
    qqnorm(temp1, axes = F, xlab="", ylab="", type="l", ylim = faixa, lty = 1, lwd = 1.2, main = main)
    par(new=TRUE)
    qqnorm(temp2, axes = F, xlab = "", ylab = "", type = "l", ylim = faixa, lty = 1, lwd = 1.2, main = "")
    par(new=TRUE)
    qqnorm(med, axes = F, xlab = "", ylab = "", type = "l", ylim = faixa, lty = 2, main = "")
    out <- as.data.frame(cbind(cs, temp1, med, temp2))
    colnames(out) <- c("Cox.Snell.res", "lower", "median", "upper")
    if(result == TRUE) return(out)
  } else  if(envelope == "Hnormal") {
    # Plot configuration - Hnormal ----------------------------------------------
    par(mar=c(2.5, 2.6, 1.2, 0.5), mgp = c(1.6, 0.6, 0))
    if(missing(xlab)) {
      xlab <- "Theoretical quatilies"
    } 
    if(missing(ylab)) {
      ylab <- "Cox-Snell residuals"
    }
    if(missing(main)) {
      main <- ""
    }
    if(missing(pch)) {
      pch <- 4
    }
    if(missing(col)) {
      col <- "grey50"
    }
    if(missing(cex)) {
      cex <- 0.5
    }
    plot(Z,sort(cs), xlab = xlab, ylab = ylab, ylim = faixa, main = main, pch = pch, col = col, cex = cex)
    par(new=TRUE)
    lines(Z,temp1)
    lines(Z,med,lty=2)
    lines(Z,temp2)  
    out <- as.data.frame(cbind(Z, sort(cs), temp1, med, temp2))
    colnames(out) <- c("Z", "Cox.Snell.res", "lower", "median", "upper")
    if(result == TRUE) return(out)
    # ---------------------------------------------------------------------------
  }
}

# Log-likelihood functions ----------------------------------------------------

# Negative of log-likelihood function for Beta 
ll_beta <- function (par, X, Z, y, link.mu, link.phi) {
  n_beta <- ncol(X)
  n_phi <- ncol(Z)
  mu <- link_mu(par = par, X = X, y = y, link.mu = link.mu)
  phi <- link_phi(par = par, X = X, Z = Z, y = y, link.phi = link.phi)
  ll <- sum(dbeta(y, mu*phi, (1-mu)*phi, log = TRUE))
  return(-ll)
}

## Simplex distribution
dsimplex_br <- function(y, mu, phi, log=TRUE){
  dis <- (y-mu)^2 / (y*(1-y)*mu^2 *(1-mu)^2)
  dsim <- -0.5*log(2*pi) -0.5*log(phi) -(3/2)*log(y*(1-y)) - (1/(2*phi))*dis
  if(log == FALSE){dsim <- exp(dsim)}
  return(dsim)
}

# Negative of log-likelihood function for Simplex 
ll_simplex <- function(par, X, Z, y, link.mu, link.phi){
  n_beta <- ncol(X)
  n_phi <- ncol(Z)
  mu <- link_mu(par = par, X = X, y = y, link.mu = link.mu)
  phi <- link_phi(par = par, X = X, Z = Z, y = y, link.phi = link.phi)
  ll <- sum(dsimplex_br(y, mu = mu, phi = phi))
  return(-ll)
}

# Negative of log-likelihood function for Unit gamma 1
ll_UG1 <- function(par, X, Z, y, link.mu, link.phi){
  n_beta <- ncol(X)
  n_phi <- ncol(Z)
  mu <- link_mu(par = par, X = X, y = y, link.mu = link.mu)
  phi <- link_phi(par = par, X = X, Z = Z, y = y, link.phi = link.phi)
  dt <- (mu^(1/phi))/(1 - mu^(1/phi))
  ll <- sum(phi * log(dt) - log(gamma(phi)) + 
              (dt - 1) * log(y) + (phi - 1) * log(- log(y)) )
  return(-ll)
}

# Negative of log-likelihood function for Unit gamma 2
ll_UG2 <- function(par, X, Z, y, link.mu, link.phi){
  n_beta <- ncol(X)
  n_phi <- ncol(Z)
  mu <- link_mu(par = par, X = X, y = y, link.mu = link.mu)
  phi <- link_phi(par = par, X = X, Z = Z, y = y, link.phi = link.phi)
  dt <- log(mu)/log(phi/(phi+1))
  ll <- sum(dt * log(phi) - log(gamma(dt)) + 
              (phi-1) * log(y) + (dt-1) * log(-log(y))  )
  return(-ll)
}

## Kumaraswamy distribution
dKW_br <- function(y, mu, phi, log=TRUE){
  p = 0.5
  term <- log(1 - p)/log(1-mu^phi)
  dkw <- log(phi) + log(term) + (phi-1) * log(y) + (term - 1)*log(1 - y^phi)
  if(log == FALSE){dkw <- exp(dkw)}
  return(dkw)
}

# Negative of log-likelihood function for Kumaraswamy
ll_KW <- function(par, X, Z, y, link.mu, link.phi){
  n_beta <- ncol(X)
  n_phi <- ncol(Z)
  mu <- link_mu(par = par, X = X, y = y, link.mu = link.mu)
  phi <- link_phi(par = par, X = X, Z = Z, y = y, link.phi = link.phi)
  p = 0.5
  bi <- log(1 - p)/log(1-mu^phi)
  ll <- sum(log(phi) + log(bi) + (phi-1) * log(y) + (bi - 1)*log(1 - y^phi))
  return(-ll)
}

# Negative of log-likelihood function for Kumaraswamy 2
ll_KW2 <- function(par, X, Z, y, link.mu, link.phi){
  n_beta <- ncol(X)
  n_phi <- ncol(Z)
  mu <- link_mu(par = par, X = X, y = y, link.mu = link.mu)
  phi <- link_phi(par = par, X = X, Z = Z, y = y, link.phi = link.phi)
  q = 0.5
  a <- -phi/log(mu)
  b <- log(1 - q)/log(1-exp(-phi))
  ll <- sum( log(a) + log(b) + (a-1)*log(y) + (b-1)*log(1 - y^a))
  return(-ll)
}

# Negative of log-likelihood function for Unit Weibull
ll_UW <- function(par, X, Z, y, link.mu, link.phi){
  n_beta <- ncol(X)
  n_phi <- ncol(Z)
  mu <- link_mu(par = par, X = X, y = y, link.mu = link.mu)
  phi <- link_phi(par = par, X = X, Z = Z, y = y, link.phi = link.phi)
  tau = 0.5
  ll <- sum( log(phi/y) + log(log(tau)/log(mu)) + (phi - 1) * log(log(y)/log(mu)) +
               log(tau) * (log(y)/log(mu))^phi )
  return(-ll)
}

## lWE distribution
dlWE_br <- function(y, mu, phi, log=TRUE){
  # dis <- -phi/(mu + phi * (mu - 1))
  # den <- y^2 *(mu + 2 * mu * phi + phi^2 * (mu - 1))
  dlwe <- phi*(phi+1)*mu*(y^(phi + 1) - y^(-phi/(mu + phi * (mu - 1))))/(y^2 *(mu + 2 * mu * phi + phi^2 * (mu - 1)))
  if(log == FALSE){dlwe <- exp(dlwe)}
  return(dlwe)
}

# Negative of log-likelihood function for log-weighted exponential (Não funciona!!!)
ll_logWE <- function(par, X, Z, y, link.mu, link.phi){
  n_beta <- ncol(X)
  n_phi <- ncol(Z)
  mu <- link_mu(par = par, X = X, y = y, link.mu = link.mu)
  phi <- link_phi(par = par, X = X, Z = Z, y = y, link.phi = link.phi)
  # dt <- (mu^(1/phi))/(1 - mu^(1/phi))
  ll <-  sum( (log(phi) + log(phi + 1)) + log(mu) + log(y^(phi + 1) -y^(-phi/(mu + phi*(mu - 1)))) - 2 * log(y) - 
               log(mu + 2 * mu * phi + phi^2 * (mu - 1)))
  #ll <- sum(dlWE_br(y, mu = mu, phi = phi))
  return(-ll)
}

# Bessel distribution
dbess_br <- function(y, mu, phi, log=TRUE){
  temp <- sqrt(1 + ((y - mu)^2)/(y*(1-y)))
  dbess <- (mu*(1-mu)*phi*exp(phi)/(pi * (y*(1-y))^(1.5))) * besselK(phi*temp,nu = 1)/temp
  if(log == FALSE){dbess <- exp(dbess)}
  return(dbess)
}

# Negative of log-likelihood function for bessel (Funcionouuuu!!!)
ll_BS <- function(par, X, Z, y, link.mu, link.phi){
  n_beta <- ncol(X)
  n_phi <- ncol(Z)
  mu <- link_mu(par = par, X = X, y = y, link.mu = link.mu)
  phi <- link_phi(par = par, X = X, Z = Z, y = y, link.phi = link.phi)
  zeta = sqrt((y - 2 * y * mu + mu^2 )/(y - y^2))
  zeta_bessel =  phi * zeta
  ll <-  sum( log(mu) + log(1 - mu) + log(phi) + phi -
      log(sqrt((y-2*y*mu + mu^2 )/(y - y^2))) + log(besselK(zeta_bessel, 1)) - log(pi) - (3/2) * log(y*(1-y)) )
  return(-ll)
}

# Johnson Sb distribution (Não funciona!!!)
# djsb_br <- function(y, mu, phi, log=TRUE){
#  temp1 <- log(y/(1-y))
#  temp2 <- log(mu/(1-mu))
#  djsb <- phi/( sqrt(2*pi)*y*(1-y))*exp(-0.5*(phi*(temp1 - temp2))^2 )
#  if(log == FALSE){djsb <- exp(djsb)}
#  return(djsb)
#}

# Negative of log-likelihood function for Johnson Sb
ll_JSB <- function(par, X, Z, y, link.mu, link.phi){
  n_beta <- ncol(X)
  n_phi <- ncol(Z)
  mu <- link_mu(par = par, X = X, y = y, link.mu = link.mu)
  phi <- link_phi(par = par, X = X, Z = Z, y = y, link.phi = link.phi)
  #p = 0.5
  #bi <- log(1 - p)/log(1-mu^phi)
  ll <- sum( -0.5*log(2*pi) + log(phi) - log(y) - log(1-y) - 0.5*( - phi*log(mu/(1 - mu)) + phi * log(y/(1 - y))  )^2 )
  # ll <- sum(djsb_br(y, mu = mu, phi = phi, log = TRUE))
  return(-ll)
}

# Negative of log-likelihood function for Complementary Unit Weibull
ll_CUW <- function(par, X, Z, y, link.mu, link.phi){
  n_beta <- ncol(X)
  n_phi <- ncol(Z)
  mu <- link_mu(par = par, X = X, y = y, link.mu = link.mu)
  phi <- link_phi(par = par, X = X, Z = Z, y = y, link.phi = link.phi)
  # tau = 0.5
  ll <- sum(log((phi*log(2))/(-log(1-mu))^phi)-log(1-y)+(phi-1)*log(-log(1-y))-(log(2)/(-log(1-mu))^phi)*(-log(1-y))^phi)
  return(-ll)
}

# Negative of log-likelihood function for Unit Gompertz
ll_UGO <- function(par, X, Z, y, link.mu, link.phi){
  n_beta <- ncol(X)
  n_phi <- ncol(Z)
  mu <- link_mu(par = par, X = X, y = y, link.mu = link.mu)
  phi <- link_phi(par = par, X = X, Z = Z, y = y, link.phi = link.phi)
  ll <- sum( log( phi*log(2)/(mu^(-phi)-1)) - (phi+1) * log(y) + ((y^(-phi)-1)/(1-mu^(-phi)))*log(2))
  return(-ll)
}

# Negative of log-likelihood function for Complementary unit Gompertz
ll_CUGO <- function(par, X, Z, y, link.mu, link.phi){
  n_beta <- ncol(X)
  n_phi <- ncol(Z)
  mu <- link_mu(par = par, X = X, y = y, link.mu = link.mu)
  phi <- link_phi(par = par, X = X, Z = Z, y = y, link.phi = link.phi)
  ll <- sum( log( phi*log(2)/((1-mu)^(-phi)-1)) - (phi+1) * log(1-y) + (((1-y)^(-phi)-1)/(1-(1-mu)^(-phi)))*log(2))
  return(-ll)
}

# Negative of log-likelihood function for Complementary unit gamma 1
ll_CUG1 <- function(par, X, Z, y, link.mu, link.phi){
  n_beta <- ncol(X)
  n_phi <- ncol(Z)
  mu <- link_mu(par = par, X = X, y = y, link.mu = link.mu)
  phi <- link_phi(par = par, X = X, Z = Z, y = y, link.phi = link.phi)
  dt <- ((1-mu)^(1/phi))/(1 - (1-mu)^(1/phi))
  ll <- sum(phi * log(dt) - log(gamma(phi)) + 
              (dt - 1) * log(1-y) + (phi - 1) * log(- log(1-y)) )
  return(-ll)
}

# Negative of log-likelihood function for unit gamma (mode)
ll_UGMode <- function(par, X, Z, y, link.mu, link.phi){
  n_beta <- ncol(X)
  n_phi <- ncol(Z)
  mu <- link_mu(par = par, X = X, y = y, link.mu = link.mu)
  phi <- link_phi(par = par, X = X, Z = Z, y = y, link.phi = link.phi)
  b <- (1+log(mu)-phi)/log(mu)
  ll <- sum( phi * log(b) - lgamma(phi) + (b - 1) * log(y) + (phi - 1)*log(-log(y)) )
  return(-ll)
}

# Negative of log-likelihood function for Vasicek (25/05/2021)
ll_VAS <- function(par, X, Z, y, link.mu, link.phi){
  n_beta <- ncol(X)
  n_phi <- ncol(Z)
  mu <- link_mu(par = par, X = X, y = y, link.mu = link.mu)
  phi <- link_phi(par = par, X = X, Z = Z, y = y, link.phi = link.phi)
  ll <- sum( 0.5 * log((1 - phi)/phi) + 0.5 * ( qnorm(y) * qnorm(y) - ( (qnorm(y) * sqrt(1 - phi) - qnorm(mu))/sqrt(phi))^2 ) )
  return(-ll)
}

# 03/03/2021 ------------------------------------ Não funciona!!!
duburrXII <- function(y, mu, phi, log=TRUE){
  tt <- -1/phi
  dt <- log(0.5^(tt) - 1)/log(log(1/mu)) 
  den <- ((log(0.5^(tt) - 1)^(phi))/(log(log(1/mu))^y)) * (log(1/y)^(dt - 1)) * (1 + log(1/y)^(dt) )^(-phi-1)
  if(log == FALSE){den <- exp(den)}
  return(den)
}


# Negative of log-likelihood function for Unit Burr-XII
ll_UBXII <- function(par, X, Z, y, link.mu, link.phi){
  n_beta <- ncol(X)
  n_phi <- ncol(Z)
  mu <- link_mu(par = par, X = X, y = y, link.mu = link.mu)
  phi <- link_phi(par = par, X = X, Z = Z, y = y, link.phi = link.phi)
  tau <- 0.5
  #b <- log(tau^(-1/phi) - 1)/(log(-log(mu)))
  ll <- sum( log(phi) + log(log(tau^(-1/phi) - 1)/(log(-log(mu)))) - log(y) + 
               (log(tau^(-1/phi) - 1)/(log(-log(mu))) - 1) * log(-log(y)) - 
               (phi - 1) * log(1 + (-log(y))^(log(tau^(-1/phi) - 1)/(log(-log(mu)))) ))
  #ll <- sum(duburrXII(y, mu = mu, phi = phi))
  return(-ll)
}

# 14/08/2021 ------------------------------------------------------------------
# Negative of log-likelihood function for LEEG
ll_LEEG <- function(par, X, Z, y, link.mu, link.phi){
  n_beta <- ncol(X)
  n_phi <- ncol(Z)
  mu <- link_mu(par = par, X = X, y = y, link.mu = link.mu)
  phi <- link_phi(par = par, X = X, Z = Z, y = y, link.phi = link.phi)
  tau <- 0.5
  b <- (tau - mu^phi)/(mu^phi * (1 - tau)) # Quantile
  #b <- (phi - 1)/((phi + 1) * mu^(phi)) # Mode
  ll <- sum( log(phi) + log(1 + b) + (phi - 1) * log(y) - 2 * log(1 + b * y^phi) )
  return(-ll)
}

# -----------------------------------------------------------------------------

boundedReg_fit <- function(loglik_fun, X, Z, y, link.mu, link.phi,
                           start   = NULL,
                           method  = c("BFGS",
                                       "Nelder-Mead",
                                       "CG",
                                       "L-BFGS-B",
                                       "SANN"),
                           lower   = -Inf,
                           upper   = Inf,
                           hessian = TRUE,
                           control = list()) {
  #-------------------------------------------
  # Initial values
  if (is.null(start)) {
    # Dimensions
    n_beta <- ncol(X)
    n_phi <- ncol(Z)
    model <- glm.fit(x = X, y = y, family = gaussian())
    start <- c("beta" = model$coefficients, rep(0, n_phi))
    names(start)[1:n_phi + n_beta] <- paste0("phi.", colnames(Z))
  } else {
    if (is.null(names(start)))
      names(start)  <- c(paste0("beta.", colnames(X)),
                         paste0("phi.", colnames(Z)))
  }
  #------------------------------------------
  # Maximization
  method <- match.arg(method)
  out <- suppressWarnings(
               optim(par = start,
               fn = loglik_fun,
               method = method,
               lower = lower,
               upper = upper,
               hessian = hessian,
               control = control,
               X = X,
               link.mu = link.mu,
               link.phi = link.phi,
               Z = Z,
               y = y)
         )
  return(out)
}

# Framework for boundedReg
boundedReg <- function(formula, phi.formula = ~1, model, link.mu, link.phi, data, ...) {
  if (missing(link.phi)) {
    link.phi <- "log"
  }
  temp <- list(...)
  #--------------------------------------------
  if (missing(data))
    data <- environment(formula)
  #-------------------------------------------
  # Matrix frame
  frame <- model.frame(formula, data)
  terms <- attr(frame, "terms")
  X <- model.matrix(terms, frame)
  Z <- model.matrix(phi.formula, data)
  y <- model.response(frame)
  n_beta <- ncol(X)
  n_phi <- ncol(Z)
  if (phi.formula == ~ 1) colnames(Z) <- "log(phi)"
  #----------------------------------------------
  # Log-likelihood functions
  loglik_fun <- switch(model, 
                       "beta" = ll_beta,
                       "simplex" = ll_simplex,
                       "unitgamma1" = ll_UG1,
                       "unitgamma2" = ll_UG2,
                       "kumaraswamy" = ll_KW,
                       "unitweibull" = ll_UW,
                       "bessel" = ll_BS,
                       "johnsonSB" = ll_JSB,
                       "kumaraswamy2" = ll_KW2,
                       "cunitweibull" = ll_CUW,
                       "unitgompertz" = ll_UGO,
                       "cunitgompertz" = ll_CUGO,
                       "cunitgamma1" = ll_CUG1,
                       "unitburrXII" = ll_UBXII,
                       "unitgammaMode" = ll_UGMode,
                       "vasicek" = ll_VAS,
                       "leeg" = ll_LEEG,
                       # Abbreviation -----------
                       "BE" = ll_beta,
                       "SIM" = ll_simplex,
                       "UG1" = ll_UG1,
                       "UG2" = ll_UG2,
                       "KW" =  ll_KW,
                       "UW" =  ll_UW,
                       "BS" = ll_BS,
                       "JSB" = ll_JSB,
                       "KW2" =  ll_KW2,
                       "CUW" = ll_CUW,
                       "UGO" = ll_UGO,
                       "CUGO" = ll_CUGO,
                       "CUG1" = ll_CUG1,
                       "UBXII" = ll_UBXII,
                       "UGMode" = ll_UGMode,
                       "VAS" = ll_VAS,
                       "LEEG" = ll_LEEG
                        )
  # Fitting -------------------------------------------------------------------
  start <- temp$start
  initial_values <- glm.fit(x = X, y = y, family = gaussian())
  if (is.null(start)) {
    start <- c("beta" = initial_values$coefficients, rep(0, n_phi))
    names(start)[1:n_phi + n_beta] <- paste0("phi.", colnames(Z))
    aux_args <- list(loglik_fun = loglik_fun, X = X, Z = Z, y = y, 
                      start = start, link.mu = link.mu, link.phi = link.phi)
    aux_args <- c(aux_args, temp)
  } else {
    if (is.null(names(start)))
      names(start)  <- c(paste0("beta.", colnames(X)),
                         paste0("phi.", colnames(Z)))
    aux_args <- list(loglik_fun = loglik_fun, X = X, Z = Z, y = y, 
                      start = start, link.mu = link.mu, link.phi = link.phi)
    aux_args <- c(aux_args, temp[-which(names(temp) == "start")])
  }
  details <- do.call(boundedReg_fit, aux_args)
  mu_coefs <- details$par[1:n_beta]
  phi_coefs <- details$par[1:n_phi + n_beta]
  names(mu_coefs) <- colnames(X)
  names(phi_coefs) <- colnames(Z)
  vcov <- NULL
  if ("hessian" %in% names(details))
    vcov <- solve(details$hessian)
  #--------------------------------------------
  # Fitted values
  fitted_mean <- exp(X %*% mu_coefs) # Arrumar!
  fitted_phi <- exp(Z %*% phi_coefs) # Arrumar!
  #--------------------------------------------
  # Output
  out <- list(call = match.call(),
              formula = formula,
              phi.formula = phi.formula,
              nobs = length(y),
              df.residual = length(y) - length(details$par),
              details = details,
              loglik = -details$value,
              AIC = 2*details$value + 2*length(details$par),
              BIC = 2*details$value + log(length(y))*length(details$par),
              vcov = vcov,
              model = model,
              link.mu = link.mu,
              link.phi = link.phi,
              coefficients = details$par,
              mu_coefs = mu_coefs,
              phi_coefs = phi_coefs,
              fitted_mean = c(fitted_mean), # Arrumar!
              fitted_phi = c(fitted_phi), # Arrumar!
              data2 = data,
              data = list(X = X, Z = Z, y = y))
  class(out) <- "boundedReg"
  return(out)
}

# END -------------------------------------------------------------------------
