myproject2<-function(){
    results<-c()
    difference<-c()
    iq<-244:nrow(fp)
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
        noobs<-c()
        m<-i-243
        id<-(i-242):(i-1)
        for(o in id){
            a<-fp$`收盘价(元)`[o]/fp$`收盘价(元)`[m]
            noobs<-append(noobs,a)
            m<-m+1}
        vol<-sd(log(noobs))*sqrt(252)
        da<-(log(s/x)+(r+0.5*(vol^2))*t)/(vol*sqrt(t))
        db<-da-vol*sqrt(t)
        star<-exp(-r*t)
        baby<-s*pnorm(da)-x*star*pnorm(db)
        results<-append(results,baby)
        }
    for(v in 1:nrow(op)){
        z<-abs(op$`收盘价(元)`[v]-results[v])
        difference<-append(difference,z)
    }
    print(difference)
    results
    write.csv(results,file="/Users/Ray/Downloads/project2.csv")
}
myproject2()
