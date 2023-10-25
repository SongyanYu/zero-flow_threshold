

peak2zero <- function(flow.df){

  colnames(flow.df) <- c("Date", "Q")
  
  nf_start <- rle(flow.df$Q)
  end = cumsum(nf_start$lengths)
  start = c(1, lag(end)[-1] + 1)
  
  nf_start = start[which(nf_start$values==0)]
  
  if(length(nf_start)==0){
    return(data.frame(Date = NA, Q = NA, peak2z_length = NA))
  }
  
  flow.df$slp.b = rep(NA, length.out = nrow(flow.df))
  flow.df$slp.f = rep(NA, length.out = nrow(flow.df))
  
  flow.df$num.date <- as.numeric(flow.df$Date)
  
  for(i in 2:(nrow(flow.df)-1)){
    ##-calculate the slope back one day
    flow.df$slp.b[i] = (flow.df$Q[i]-flow.df$Q[(i-1)])/(flow.df$num.date[i]-flow.df$num.date[(i-1)])
    ##-calculate the slope forward one day
    flow.df$slp.f[i] = (flow.df$Q[(i+1)]-flow.df$Q[i])/(flow.df$num.date[(i+1)]-flow.df$num.date[i])
  }
  
  ##-make a column for the peak of each event flagged by a change in derivative
  flow.df$peak.flag = rep(NA, length.out = nrow(flow.df))
  
  peak.threshold<- quantile(flow.df$Q, 0.5,na.rm = TRUE)
  
  ##-now flag those derivative changes
  for(i in 2:(nrow(flow.df)-1)){
    ##-if the slope back is greater than some threshold and the slope forward is negative, flag it as a peak
    if(flow.df$slp.b[i]>0.0001 & flow.df$slp.f[i]<0 & flow.df$Q[i] >= peak.threshold){
      flow.df$peak.flag[i] = 1 }
    else{
      ##-otherwise don't
      flow.df$peak.flag[i] = -9999}
  }
  
  # Mark locations of start of no-flow events
  flow.df$peak.flag[nf_start] = -1
  
  peak2zero = rle(flow.df$peak.flag)
  end = cumsum(peak2zero$lengths)
  start = c(1, lag(end)[-1] + 1)
  
  peakstart = start[which(peak2zero$values==1)]
  tt= sort(append(nf_start,peakstart))
  
  peak2z = rep(NA, length.out = length(nf_start))
  
  # in case there is only one no-flow event that is the first in tt (no peak before it)
  if(which(tt==nf_start[1]) & length(nf_start)==1){
    return(data.frame(Date = NA, Q = NA, peak2z_length = NA))
  }
  
  for (i in 1:length(nf_start)){
    
    if(which(tt==nf_start[i]) == 1){
      next
    }
    
    if ((length(nf_start[i] - tt[which(tt==nf_start[i])-1]) > 0) & (tt[which(tt==nf_start[i])-1] %in% peakstart)){
      peak2z[i] = nf_start[i] - tt[which(tt==nf_start[i])-1]
    }
  }
  
  nf_start = nf_start[!is.na(peak2z)]
  peak2z = na.omit(peak2z)
  
  total = cbind(flow.df[nf_start,c('Date','Q')], peak2z)
  colnames(total)[3] = 'peak2z_length'
  
  return(total)
}
