AGEpcf = function(n,r,h,U,A){
  out = vector(length=length(r))
  for(i in 1:length(r)){
    ecf = A - ((r[i]/pi)*U) + ((r[i]^2)/pi)
    out[i] = (1/n)*(1/sqrt(r[i]))*(1/sqrt(h))*sqrt((A/ecf))*sqrt((A/(2*pi)))
  }
  return(out)
}

AGEpcfPp <- function(pattern,alpha,bw=0.01,r=NULL){
  n = pattern$n
  pattern_pcf = spatstat::pcf(pattern,bw=bw)
  r = pattern_pcf$r
  w = Window(pattern)
  area = spatstat::area(w)
  perimeter = spatstat::perimeter(w)
  AGEsig = AGEpcf(n=n,r=r,h=bw,U=perimeter,A=area)
  beta = 1 - (1 - alpha)^(1/length(r))
  crit = qnorm(1 - alpha/2)
  upper = 1 + crit*AGEsig
  lower = 1 - crit*AGEsig
  pz = max((abs(pattern_pcf$iso-1)/AGEsig)[-1])
  ploc = 2*(1-pnorm(pz))
  p = 1 - ((1-ploc)^(length(r)-1))
  this <- list(points = pattern,
               n = n,
               pattern_pcf = pattern_pcf,
               r = r,
               w = w,
               area = area,
               perimeter = perimeter,
               AGEsig = AGEsig,
               beta = beta,
               crit = crit ,
               upper = upper,
               lower = lower,
               p = p
  )
  class(this) <- append(class(this),"AGEpcf")
  return(this)
} 

plot.AGEpcf = function(AGEpcf){
  plot(x=AGEpcf$r,y=AGEpcf$pattern_pcf$iso,ylim=c(0,2),type="l")
  lines(x=AGEpcf$r,y=rep(1,length(AGEpcf$r)),col="blue",lty=2,type="l")
  lines(x=AGEpcf$r,y=AGEpcf$lower,col="green")
  lines(x=AGEpcf$r,y=AGEpcf$upper,col="green")
}