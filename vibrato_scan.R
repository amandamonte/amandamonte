##--------function to calculate vibrato rate--------
##Seewiesen, 13 Mai 2019, Amanda Monte

vibrato_scan <- function(d_freq_table) { # this table is an output from "dfreq" function from "Seewave" package
  if (is.data.frame(d_freq_table)==FALSE){
    d_freq_table <- as.data.frame(d_freq_table)}
  b <- na.omit(d_freq_table)
  c <- data.frame(peak=0, valey=0, diff=0, time_peak=0, time_valey=0, 
                  time_diff=0)
  n_osc <- c
  for (m in 2:nrow(b)){
    if (m+1<=nrow(b)){
      if (b$y[m]>b$y[m+1] && b$y[m]>b$y[m-1]||
          (b$y[m]==b$y[m+1] && b$y[m]>b$y[m-1])){
        c$peak <- b$y[m]
        c$time_peak <- b$x[m]
        if (m+2<=nrow(b)){
          if (b$y[m+2]>b$y[m+1]){
            c$valey <- b$y[m+1]
            c$time_valey <- b$x[m+1]}else{
              for(x in m+1:nrow(b)){
                if (x+1<=nrow(b)){
                  if(b$y[x+1]>b$y[x]){
                    c$valey <- b$y[x]
                    c$time_valey <- b$x[x];
                    break}}}}}
        c$diff <- c$peak-c$valey
        c$time_diff <- c$time_valey-c$time_peak
        n_osc<-rbind(n_osc,c)}}}
  d <- subset(n_osc, diff!=0)
  e <- subset(d, time_diff>0)
  return(e)
}
