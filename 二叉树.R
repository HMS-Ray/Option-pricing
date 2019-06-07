myproject<-function(steps){
      iq<-244:nrow(fp)
      results<-c()
      difference<-c()
    for(i in iq){
       s<-fp$`收盘价(元)`[i]
       x<-44000
       t<-(43640-fp$日期[i])/365
       for(y in 1:nrow(nrm)){
           if(nrm$时间[y]==fp$日期[i]){
               r<-nrm$收益率[y]*0.01
               break
           }
       }
       period<-t/steps
       noobs<-c()
         m<-i-243
         id<-(i-242):(i-1)
         for(o in id){
                 a<-fp$`收盘价(元)`[o]/fp$`收盘价(元)`[m]
                 noobs<-append(noobs,a)
                 m<-m+1
         }
    vol<-sd(log(noobs))*sqrt(252)
    u<-exp(vol*sqrt(period))
    d<-1/u
    star<-exp(r*period)
    p<-(star-d)/(u-d)
    pverse<-1-p
    baby <- vector(length=steps+1)
    for(l in 1:steps+1){
        baby[l] <- max((s*u^(l-1)*d^(steps+1-l)-x),0)}
    for(j in steps:1){
        for(k in 1:j)
        {baby[k] <- (pverse*baby[k]+p*baby[k+1])/star}
    }
   results<-append(results,baby[1])
}
for(v in 1:nrow(op)){
    z<-abs(op$`收盘价(元)`[v]-results[v])
    difference<-append(difference,z)
}
    print(difference)
    results
    write.csv(results,file="/Users/Ray/Downloads/project1.csv")
}
myproject(300)


