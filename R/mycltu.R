#' Central Limit Theorem Uniform Distribution
#'
#' Using this function will create a uniform distribution of
#' the data as provided. Giving a sample size, number of iterations,
#' and axis minimum and maximum values, the function will
#' create a histogram with a normal curve and a uniform curve.
#'
#' @param n sample size
#' @param iter number of iterations
#' @param a x-axis minimum value
#' @param b x-axis maximum value
#'
#' @return A histogram of a uniform distribution following the central limit theorem
#' @export
#'
#' @examples mycltu(n=30,iter=10000,a=0,b=10)
#'Where n = 30 sample size, iter = 10000 iterations of the sample,
#'a = 0 minimum value, and b = 10 maximum value.
mycltu=function(n,iter,a,b){
  y=runif(n*iter,a,b)
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  w=apply(data,2,mean)
  param=hist(w,plot=FALSE)
  ymax=max(param$density)
  ymax=1.1*ymax

  hist(w,freq=FALSE,  ylim=c(0,ymax), main=paste("Histogram of sample mean",
                                                 "\n", "sample size= ",n,sep=""),xlab="Sample mean")
  lines(density(w),col="Blue",lwd=2)
  curve(dnorm(x,mean=(a+b)/2,sd=(b-a)/(sqrt(12*n))),add=TRUE,col="Red",lty=2,lwd=3)
  curve(dunif(x,a,b),add=TRUE,lwd=4)

}
