# P-test on coefficients of QR-fits
#
#' Function computing significance of a tau-quantile fit (in 9999 MC permutation test)
#'
#' @import data.table
#'
#' @export
p_test <- function(data,tau,N=9999, freq=FALSE,Psum=FALSE, lm=FALSE,Year=TRUE, method="fn", STN=TRUE){
  if((freq==FALSE)&(Psum==FALSE)){ # Intensity data
    if(STN==TRUE){ # All stations
      if(lm==FALSE){# QR fits, only 2day resolution
        reps <- data[, list(replicate(N, round(as.numeric(coef(rq(sample(max2d) ~ d2, tau=tau,method=method))[2]),digits=7))),by=STN]
        obs <- data[, list(round(as.numeric(coef(rq(max2d ~ d2, tau=tau, method=method))[2]),digits=7)),by=STN]
        setkey(reps,STN)
        setkey(obs,STN)
        tmp <- merge(reps,obs)
        setnames(tmp, c("STN","reps","obs"))
        p <- tmp[, list(unique((1+sum(obs > reps))/(N+1))),by=STN]
      }
      else{        # LM fits
        reps <- data[, list(replicate(N, round(as.numeric(coef(lm(sample(max2d) ~ d2))[2]),digits=7))),by=STN]
        obs <- data[, list(round(as.numeric(coef(lm(max2d ~ d2))[2]),digits=7)),by=STN]
        setkey(reps,STN)
        setkey(obs,STN)
        tmp <- merge(reps,obs)
        setnames(tmp, c("STN","reps","obs"))
        p <- tmp[, list(unique((1+sum(obs > reps))/(N+1))),by=STN]
      }
    }
    else{          # 1 station
      if(lm==FALSE){# QR fits
        reps <- data[, list(replicate(N, round(as.numeric(coef(rq(sample(max2d) ~ d2, tau=tau,method=method))[2]),digits=7)))]
        obs <- data[, list(round(as.numeric(coef(rq(max2d ~ d2, tau=tau, method=method))[2]),digits=7))]
        setkey(reps,STN)
        setkey(obs,STN)
        tmp <- merge(reps,obs)
        setnames(tmp, c("STN","reps","obs"))
        p <- tmp[, list(unique((1+sum(obs > reps))/(N+1))),by=STN]
      }
      else{        # LM fits
        reps <- data[, list(replicate(N, round(as.numeric(coef(lm(sample(max2d) ~ d2))[2]),digits=7)))]
        obs <- data[, list(round(as.numeric(coef(lm(max2d ~ d2))[2]),digits=7))]
        setkey(reps,STN)
        setkey(obs,STN)
        tmp <- merge(reps,obs)
        setnames(tmp, c("STN","reps","obs"))
        p <- tmp[, list(unique((1+sum(obs > reps))/(N+1))),by=STN]
      }
    }
  }
  else if(Psum==TRUE){ # Psum data
    if(Year==FALSE){# only for all stations, 2day resolution
      if(lm==FALSE){# QR fits, only 2day resolution
      reps <- data[, list(replicate(N, round(as.numeric(coef(rq(sample(Psum) ~ d2, tau=tau,method=method))[2]),digits=7))),by=STN]
      obs <- data[, list(round(as.numeric(coef(rq(Psum ~ d2, tau=tau, method=method))[2]),digits=7)),by=STN]
      setkey(reps,STN)
      setkey(obs,STN)
      tmp <- merge(reps,obs)
      setnames(tmp, c("STN","reps","obs"))
      p <- tmp[, list(unique((1+sum(obs > reps))/(N+1))),by=STN]
      }
      else{        # LM fits
        reps <- data[, list(replicate(N, round(as.numeric(coef(lm(sample(yr_mean) ~ Year))[2]),digits=7))),by=STN]
        obs <- data[, list(round(as.numeric(coef(lm(yr_mean ~ Year))[2]),digits=7)),by=STN]
        setkey(reps,STN)
        setkey(obs,STN)
        tmp <- merge(reps,obs)
        setnames(tmp, c("STN","reps","obs"))
        p <- tmp[, list(unique((1+sum(obs < reps))/(N+1))),by=STN]
      }
    }
    else{
      if(lm==FALSE){# QR fits, only yearly resolution
      # only for all stations, yearly resolution
      reps <- data[, list(replicate(N, round(as.numeric(coef(rq(sample(Psumyr) ~ Year, tau=tau,method=method))[2]),digits=7))),by=STN]
      obs <- data[, list(round(as.numeric(coef(rq(Psumyr ~ Year, tau=tau, method=method))[2]),digits=7)),by=STN]
      setkey(reps,STN)
      setkey(obs,STN)
      tmp <- merge(reps,obs)
      setnames(tmp, c("STN","reps","obs"))
      p <- tmp[, list(unique((1+sum(obs > reps))/(N+1))),by=STN]
      }
      else{
      reps <- data[, list(replicate(N, round(as.numeric(coef(lm(sample(Psumyr) ~ Year))[2]),digits=7))),by=STN]
      obs <- data[, list(round(as.numeric(coef(lm(Psumyr ~ Year))[2]),digits=7)),by=STN]
      setkey(reps,STN)
      setkey(obs,STN)
      tmp <- merge(reps,obs)
      setnames(tmp, c("STN","reps","obs"))
      p <- tmp[, list(unique((1+sum(obs > reps))/(N+1))),by=STN]
      }
    }
  }
  else{              # Frequency data (test whether slope is significantly negative)
    if(STN==TRUE){   # All stations
      if(Year==TRUE){ # Yearly resolution
        if(lm==FALSE){# QR fits
          reps <- data[, list(replicate(N, round(as.numeric(coef(rq(sample(f) ~ Year, tau=tau,method="br"))[2]),digits=7))),by=STN]
          obs <- data[, list(round(as.numeric(coef(rq(f ~ Year, tau=tau,method="br"))[2]),digits=7)),by=STN]
          setkey(reps,STN)
          setkey(obs,STN)
          tmp <- merge(reps,obs)
          setnames(tmp, c("STN","reps","obs"))
          p <- tmp[, list(unique((1+sum(obs < reps))/(N+1))),by=STN]
        }
        else{        # LM fits
          reps <- data[, list(replicate(N, round(as.numeric(coef(lm(sample(f) ~ Year))[2]),digits=7))),by=STN]
          obs <- data[, list(round(as.numeric(coef(lm(f ~ Year))[2]),digits=7)),by=STN]
          setkey(reps,STN)
          setkey(obs,STN)
          tmp <- merge(reps,obs)
          setnames(tmp, c("STN","reps","obs"))
          p <- tmp[, list(unique((1+sum(obs < reps))/(N+1))),by=STN]         # LM fits
        }
      }
      else{          # 2day resolution
        # no LM options
        reps <- data[, list(replicate(N, round(as.numeric(coef(rq(sample(f) ~ d2, tau=tau,method=method))[2]),digits=7))),by=STN]
        obs <- data[, list(round(as.numeric(coef(rq(f ~ d2, tau=tau,method=method))[2]),digits=7)),by=STN]
        setkey(reps,STN)
        setkey(obs,STN)
        tmp <- merge(reps,obs)
        setnames(tmp, c("STN","reps","obs"))
        p <- tmp[, list(unique((1+sum(obs < reps))/(N+1))),by=STN]
      }
    }
    else{            # 1 STN
      if(Year==TRUE){ # only yearly resolution
        if(lm==FALSE){# QR fits
          tmp <- data[, list(replicate(N, round(as.numeric(coef(rq(sample(f) ~ Year, tau=tau,method="br"))[2]),digits=7)))]
          tmp$obs <- data[, list(round(as.numeric(coef(rq(f ~ Year, tau=tau,method="br"))[2]),digits=7))]
          setnames(tmp, c("reps","obs"))
          p <- tmp[, list((1+sum(obs > reps))/(N+1))]
        }
        else{        # LM fits
          tmp <- data[, list(replicate(N, round(as.numeric(coef(lm(sample(f) ~ Year))[2]),digits=7)))]
          tmp$obs<- data[, list(round(as.numeric(coef(lm(f ~ Year))[2]),digits=7))]
          setnames(tmp, c("reps","obs"))
          p <- tmp[, list(unique((1+sum(obs < reps))/(N+1)))]
        }
      }
    }
  }
  return(p)
}



