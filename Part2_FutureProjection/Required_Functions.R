###############################################################################
#
#  All functions in this Rscript can be found in 'downscaleR' R package by SantanderMetGroup,  
#  and available at https://rdrr.io/github/SantanderMetGroup/downscaleR/src/R/biasCorrection.R
#
###############################################################################

#' @title adjustPrecipFreq
#' @description Adjusts precipitation frequency in 'p' (prediction) to the observed frequency in 'o'. 
#' It constitutes a preprocess to bias correct precipitation data following Themeßl et al. (2012). 
#' @param obs A vector (e.g. station data) containing the observed climate data for the training period
#' @param pred A vector containing the simulated climate by the model for the training period. 
#' @param threshold The minimum value that is considered as a non-zero precipitation. 
#' @importFrom MASS fitdistr
#' @keywords internal
#' @importFrom stats rgamma
#' @author S. Herrera and M. Iturbide

adjustPrecipFreq <- function (obs, pred, threshold) 
{
  o <- obs[!is.na(obs)]
  p <- pred[!is.na(pred)]
  nPo <- sum(as.double(o < threshold))
  nPp <- ceiling(length(p) * nPo/length(o))
  ix <- sort(p, decreasing = FALSE, index.return = TRUE)$ix
  Ps <- sort(p, decreasing = FALSE)
  Pth <- max(Ps[nPp:(nPp + 1)], na.rm = TRUE)
  inddrzl <- which(Ps[(nPp + 1):length(Ps)] < threshold)
  if (length(inddrzl) > 0) {
    Os <- sort(o, decreasing = FALSE, na.last = NA)
    indO <- ceiling(length(Os) * (nPp + max(inddrzl))/length(Ps))
    auxOs <- Os[(nPo + 1):indO]
    if (length(unique(auxOs)) > 6) {
      auxGamma <- fitdistr(auxOs, "gamma")
      Ps[(nPp + 1):(nPp + max(inddrzl))] <- rgamma(length(inddrzl), 
                                                   auxGamma$estimate[1], rate = auxGamma$estimate[2])
    }
    else {
      Ps[(nPp + 1):(nPp + max(inddrzl))] <- mean(auxOs)
    }
    Ps <- sort(Ps, decreasing = FALSE, na.last = NA)
  }
  if (nPo > 0) {
    ind <- min(nPp, length(p))
    Ps[1:ind] <- 0
  }
  p[ix] <- Ps
  pred[!is.na(pred)] <- p
  return(list(nP = c(nPo, nPp), Pth = Pth, p = pred))
}



#' @title Empirical Quantile Mapping method for bias correction
#' @description Implementation of Empirical Quantile Mapping method for bias correction 
#' @param o A vector (e.g. station data) containing the observed climate data for the training period
#' @param p A vector containing the simulated climate by the model for the training period. 
#' @param s A vector containing the simulated climate for the variable used in \code{p}, but considering the test period.
#' @param precip Logical for precipitation data. If TRUE Adjusts precipitation 
#' frequency in 'x' (prediction) to the observed frequency in 'y'. This is a preprocess to bias correct 
#' precipitation data following Themeßl et al. (2012). To adjust the frequency, 
#' parameter \code{pr.threshold} is used (see below).
#' @param pr.threshold The minimum value that is considered as a non-zero precipitation. Ignored when 
#' \code{precip = FALSE}. See details in function \code{biasCorrection}.
#' @param n.quantiles Integer indicating the number of quantiles to be considered when method = "eqm". Default is NULL, 
#' that considers all quantiles, i.e. \code{n.quantiles = length(p)}.
#' @param extrapolation Character indicating the extrapolation method to be applied to correct values in  
#' \code{"s"} that are out of the range of \code{"p"}. Extrapolation is applied only to the \code{"eqm"} method, 
#' thus, this argument is ignored if other bias correction method is selected. 
#' @keywords internal
#' @importFrom stats approxfun ecdf quantile
#' @author S. Herrera and M. Iturbide

eqm <- function (o, p, s, precip, pr.threshold, n.quantiles, extrapolation) 
{
  if (precip == TRUE) {
    threshold <- pr.threshold
    if (any(!is.na(o))) {
      params <- adjustPrecipFreq(o, p, threshold)
      p <- params$p
      nP <- params$nP
      Pth <- params$Pth
    }
    else {
      nP = NULL
    }
    smap <- rep(NA, length(s))
    if (any(!is.na(p)) & any(!is.na(o))) {
      if (length(which(p > Pth)) > 0) {
        noRain <- which(s <= Pth & !is.na(s))
        rain <- which(s > Pth & !is.na(s))
        drizzle <- which(s > Pth & s <= min(p[which(p > 
                                                      Pth)], na.rm = TRUE) & !is.na(s))
        if (length(rain) > 0) {
          eFrc <- tryCatch({
            ecdf(s[rain])
          }, error = function(err) {
            stop("There are not precipitation days in newdata for the step length selected in one or more locations. Try to enlarge the window step")
          })
          if (is.null(n.quantiles)) 
            n.quantiles <- length(p)
          bins <- n.quantiles
          qo <- quantile(o[which(o > threshold & !is.na(o))], 
                         prob = seq(1/bins, 1 - 1/bins, 1/bins), na.rm = T)
          qp <- quantile(p[which(p > Pth)], prob = seq(1/bins, 
                                                       1 - 1/bins, 1/bins), na.rm = T)
          p2o <- tryCatch({
            approxfun(qp, qo, method = "linear")
          }, error = function(err) {
            NA
          })
          smap <- s
          smap[rain] <- if (suppressWarnings(!is.na(p2o))) {
            p2o(s[rain])
          }
          else {
            s[rain] <- NA
          }
          if (extrapolation == "constant") {
            smap[rain][which(s[rain] > max(qp, na.rm = TRUE))] <- s[rain][which(s[rain] > 
                                                                                  max(qp, na.rm = TRUE))] + (qo[length(qo)] - 
                                                                                                               qp[length(qo)])
            smap[rain][which(s[rain] < min(qp, na.rm = TRUE))] <- s[rain][which(s[rain] < 
                                                                                  min(qp, na.rm = TRUE))] + (qo[1] - qp[1])
          }
          else {
            smap[rain][which(s[rain] > max(qp, na.rm = TRUE))] <- qo[length(qo)]
            smap[rain][which(s[rain] < min(qp, na.rm = TRUE))] <- qo[1]
          }
        }
        else {
          smap <- rep(0, length(s))
          warning("There are not precipitation days in newdata for the step length selected in one or more locations. Consider the possibility of enlarging the window step")
        }
        if (length(drizzle) > 0) {
          smap[drizzle] <- quantile(s[which(s > min(p[which(p > 
                                                              Pth)], na.rm = TRUE) & !is.na(s))], probs = eFrc(s[drizzle]), 
                                    na.rm = TRUE, type = 4)
        }
        smap[noRain] <- 0
      }
      else {
        smap <- s
        warning("No rainy days in the prediction. Bias correction is not applied")
      }
    }
  }
  else {
    if (all(is.na(o))) {
      smap <- rep(NA, length(s))
    }
    else if (all(is.na(p))) {
      smap <- rep(NA, length(s))
    }
    else if (any(!is.na(p)) & any(!is.na(o))) {
      if (is.null(n.quantiles)) 
        n.quantiles <- length(p)
      bins <- n.quantiles
      qo <- quantile(o, prob = seq(1/bins, 1 - 1/bins, 
                                   1/bins), na.rm = TRUE)
      qp <- quantile(p, prob = seq(1/bins, 1 - 1/bins, 
                                   1/bins), na.rm = TRUE)
      p2o <- approxfun(qp, qo, method = "linear")
      smap <- p2o(s)
      if (extrapolation == "constant") {
        smap[which(s > max(qp, na.rm = TRUE))] <- s[which(s > 
                                                            max(qp, na.rm = TRUE))] + (qo[length(qo)] - 
                                                                                         qp[length(qo)])
        smap[which(s < min(qp, na.rm = TRUE))] <- s[which(s < 
                                                            min(qp, na.rm = TRUE))] + (qo[1] - qp[1])
      }
      else {
        smap[which(s > max(qp, na.rm = TRUE))] <- qo[length(qo)]
        smap[which(s < min(qp, na.rm = TRUE))] <- qo[1]
      }
    }
  }
  return(smap)
}
