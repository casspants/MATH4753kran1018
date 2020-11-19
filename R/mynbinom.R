#' Negative Binomial Probability
#'
#'This function will provide the probability of a certain number
#'of successes of binomial trials when given y, the number of trials,
#'r, the number of successes, and p, the probability of a success.
#'The probability is provided as a decimal, which can be understood
#'as a percent once multiplied by 100.
#'
#' @param y number of trials
#' @param r number of successes
#' @param p hypothesized probability of successes
#'
#' @return
#' @export
#'
#' @examples mynbin(10,3,0.4)
#' Where y = 10 trials, r = 3 successes, and 0.4 is the probability of a success
mynbin=function(y,r,p){
  choose(y-1,r-1)*p^r*(1-p)^(y-r)
}


