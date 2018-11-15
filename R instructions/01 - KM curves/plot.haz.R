plot_haz <- function(KM_obj,plot="TRUE", results="FALSE") {
	ti <- summary(KM_obj)$time
	di <- summary(KM_obj)$n.event
	ni <- summary(KM_obj)$n.risk

	#Est Hazard Function
	est.haz <- 1:(length(ti))
	for (i in 1:(length(ti)-1))
    		est.haz[i] <- di[i]/(ni[i]*(ti[i+1]-ti[i]))
	est.haz[length(ti)] <- est.haz[length(ti)-1]
	
	if (plot=="TRUE") {
		plot(ti,est.haz,type="s",xlab="Time",ylab="Hazard Rate",main=expression(paste(hat(h),(t)[KM])))
	}
	
	if (results=="TRUE") {
	  return(list(est.haz=est.haz,time=ti))
	}
}



plot_chaz <- function(KM_obj,plot="TRUE",results="FALSE") {
  ti <- summary(KM_obj)$time
  di <- summary(KM_obj)$n.event
  ni <- summary(KM_obj)$n.risk
  
  #Est Cumulative Hazard Function
  est.cum.haz <- 1:(length(ti))
  for (i in 1:(length(ti)))
    est.cum.haz[i] <- sum(di[1:i]/ni[1:i])
  
  plot.chaz <- 1:length(KM_obj$time)
  for (i in 1:length(plot.chaz))
    plot.chaz[i] <- sum((KM_obj)$n.event[1:i]/(KM_obj)$n.risk[1:i])
  
  if (plot=="TRUE") {
    plot((KM_obj)$time,plot.chaz,type="s",xlab="Time",ylab="Cumulative Hazard",main=expression(paste(tilde(H),(t))))
  }
  
  if (results=="TRUE") {
    return(list(est.chaz=plot.chaz,time=(KM_obj)$time))
  }
}
