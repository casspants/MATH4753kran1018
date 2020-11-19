#' Normal Curve
#'
#' This function wil produce a normal curve when given a mean value,
#' a standard deviation, and a significance level point estimate
#' where alpha cuts off.
#' This function will provide a shaded region to the left of the
#' provided cut off value (a).
#' This function also provides the probability of a value at or
#' below the given 'a' value.
#'
#' @param mu mean value
#' @param sigma standard deviation value
#' @param a the value that alpha cuts off at; for P(x<=a)
#'
#' @return
#' @export
#'
#' @examples myncurve(10,5,6)
#' With mu = 10 mean, sigma = 5 standard deviation, and a = 6
#' where the alpha level cut-off happens.
#' So this will be a normal distirbution centered at 10 with a
#' tail shaded in to the left of the value 6.
#' There will also be a provided probability of a value at or below
#' 'a', so the probability of a value at or below 6.
myncurve = function(mu, sigma,a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  x<- seq((mu-(3*sigma)),a,length=1000)
  y<-dnorm(x,mean=mu,sd=sigma)
  polygon(c((mu-(3*sigma)),x,a),c(0,y,0),col="tomato")
  prob=(pnorm(a,mean=mu,sd=sigma))
  prob=round(prob,4)
  print(prob)
}
