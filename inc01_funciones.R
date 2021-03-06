## Función maxlike.df, versión de 2013-08-25
## por J.R. Ferrer Paris
## modificación de la función maxlike, del paquete maxlike
require(maxlike)
maxlike.df <- function (formula, x, z, starts, hessian = TRUE, fixed, removeDuplicates = FALSE, na.action = "na.omit", ...)  {
  if (identical(formula, ~1)) 
    stop("At least one continuous covariate must be specified in the formula")
  varnames <- all.vars(formula)
  call <- match.call()
  X.mf <- model.frame(formula, x, na.action = na.action)
  X.mf.a <- attributes(X.mf)
  X <- model.matrix(formula, X.mf)
  Z.mf <- model.frame(formula, z, na.action = na.action)
  Z.mf.a <- attributes(Z.mf)
  Z <- model.matrix(formula, Z.mf)
  npars <- ncol(X)
  parnames <- colnames(X)
  if (!"(Intercept)" %in% parnames) 
    stop("The intercept must be estimated or fixed")
  if (missing(starts)) {
    starts <- rep(0, npars)
    names(starts) <- parnames
  }
  else names(starts) <- parnames
  nll <- function(pars) {
    psix <- plogis(X %*% pars)
    psiz <- plogis(Z %*% pars)
    -1 * sum(log(psix/sum(psiz) + .Machine$double.eps))
  }
  is.fixed <- rep(FALSE, npars)
  if (!missing(fixed)) {
    if (length(fixed) != length(starts)) 
      stop("fixed should be a vector with the same length as the number of parameters to be estimated")
    if (sum(is.double(fixed)) < 1) 
      stop("fixed must contain at least one real value")
    is.fixed <- !is.na(fixed)
    if (sum(!is.fixed) < 1) 
      stop("you cannot fix all parameters in the model")
    npars <- sum(!is.fixed)
    nll.fix <- function(p) {
      p[is.fixed] <- fixed[is.fixed]
      do.call("nll", list(pars = p))
    }
    fm <- optim(starts, nll.fix, hessian = hessian, ...)
    fm$par[is.fixed] <- fixed[is.fixed]
  }
  else {
    fm <- optim(starts, nll, hessian = hessian, ...)
  }
  not.fixed <- !is.fixed
  par <- fm$par
  if (hessian) {
    vcTry <- try(solve(fm$hessian[not.fixed, not.fixed]))
    if (identical(class(vcTry), "matrix")) {
      vc <- matrix(0, length(par), length(par))
      vc[not.fixed, not.fixed] <- vcTry
      se <- sqrt(diag(vc))
    }
    else {
      vc <- matrix(NA, npars, npars)
      se <- rep(NA, npars)
    }
  }
  else {
    vc <- matrix(NA, npars, npars)
    se <- rep(NA, npars)
  }
  dimnames(vc) <- list(parnames, parnames)
  aic <- 2 * fm$value + 2 * npars
  fitted <- plogis(Z %*% par)
  out <- list(Est = cbind(Est = par, SE = se), vcov = vc, AIC = aic, 
              call = call, 
              optim = fm, not.fixed = not.fixed)
  class(out) <- c("maxlikeFit", "list")
  return(out)
}



## Función luq, versión de 2012-07-01
## por J.R. Ferrer Paris
## *l*ength of *u*ni*q*ue, non-NA values, 
luq <- function(x,contar.NA=FALSE) {
	if (contar.NA==F) {
	x <- x[!is.na(x)]
	}
 length(unique(x))
 }

