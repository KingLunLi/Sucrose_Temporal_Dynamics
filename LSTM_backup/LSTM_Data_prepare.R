

sampledir<-"D:/Xiguang/sample_data/DLC_data/"
outputdir<-"D:/Xiguang/sample_data/DLC_data/"
######### define positive time range
timerange1 =  c(-6,-5)
timerange2 =  c(-4,-3)
### This step may take some time 
for( bound in list(timerange1,timerange2)){
  
  Allresult<-data.frame()
  Allresulttest<-data.frame()
  for (days in c("541_SUC","542_SUC","543_SUC","544_SUC","545_SUC")
  ) {
    interval<-32
    print(paste0("Now proceed to time range ",bound[1],"s to ",bound[2],"s for data ",days))
    for (group in c("Ctrl")) {
      if(group=="Ctrl"){
        mlist<-c("Opto2")
      }
      for (name in mlist) {
        result<-data.frame()
        resulttest<-data.frame()
        for (RD in c(T,F)) {
          
          mice<-name
          
          data<-read.csv(file=paste0(sampledir,"coordinate/",mice,"_",days,"_codinate.csv"),check.names = FALSE)

            press<-read.csv(file = paste0(sampledir,"timimg/",name,"_",days,"_Lever_press_timing.csv"),check.names = FALSE)
            
          
          press<-data.frame(`Time(s)`=press[,1],check.names = FALSE)
          possib<-data[,c(3*(1:10)+1)]
          data<-data[,-c(3*(1:10)+1)]
          data<-data[data$time_modified<=3600,]
          x=1
          FR<-0
          a<-unlist(press$`Time(s)`)
          a<-a*10
          a<-round(a)
          a<-a[a>121]
          a<-a[a<floor(max(unlist(data$time_modified)))*10]
          
          if(RD==T){
            la<-paste0("Random")
            
            list<-1:(max(a)+10)
            list2<-list()
            for (b in a) {
              list2<-c(unlist(list2),(b-15):(b+15))
            }
            list<-list[list>121]
            list2<-list2[list2>121]
            list<-setdiff(list,list2)
            
            if(length(a)<=150){
              a<-sample(list,length(a)*29)
            }else{
              a<-sample(list,150*29)
            }
            
            
            
            a<-a[a>121]
          }else{
            la<-group
          }
          co<-1
          dlnum=0
          for (b in 1:length(a)) {
            b=b-dlnum
            rpnum=0
            repeat{
              
              if(is.na(which(round(data$time_modified,1)*10==a[b])[1])==TRUE){
                if(rpnum==0){
                  #print("Here NA Exist")
                  #print(a[b])
                }
                a[b]=a[b]-1
                rpnum=rpnum+1
                if(rpnum>5&RD==F){
                  a=a[-b]
                  dlnum=dlnum+1
                  #print("ALP deleted!")
                  break()
                }
              }else{
                repeat{
                  if(b>1&(which(round(data$time_modified,1)*10==a[b])[1] %in% a[1:(b-1)])){
                    a[b]=a[b]-1
                    rpnum=rpnum+1
                    #print("Sample repeat")
                    #print(a[b])
                  }else{
                    break()
                  }
                }
                if(is.na(which(round(data$time_modified,1)*10==a[b])[1])==FALSE){
                  a[b]<-which(round(data$time_modified,1)*10==a[b])[1]
                  break() 
                }
              }
            }
            
          }
          if(sum(is.na(a))>0){stop("NA still Exist!")}
          resulttemp<-data.frame()
          resulttemp2<-data.frame()
          if(RD==T){
            rdn<-round((length(a)/29)*0.2,0)*29
            
            rd<-sample(length(a),rdn)
            c<-a[rd]
            a<-a[-rd]
            for (b in a) {

              
              temp<-data[(b+bound[2]*interval-30):(b+bound[2]*interval),2:ncol(data)]
              temp$Label<-paste0(mice,"_",days,"_","press_",co)
              temp$Label2<-la
              resulttemp<-rbind(resulttemp,temp)
              co<-co+1
              if(co/30==floor(co/30)){
                resulttemp2<-rbind(resulttemp2,resulttemp)
                resulttemp<-data.frame()
                #print(co)
              }
            }
            
            resulttemp<-rbind(resulttemp2,resulttemp)
            result<-rbind(result,resulttemp)
            resulttemp<-data.frame()
            resulttemp2<-data.frame()
            for (b in c) {

              temp<-data[(b+bound[2]*interval-30):(b+bound[2]*interval),2:ncol(data)]
              temp$Label<-paste0(mice,"_",days,"_","press_",co)
              temp$Label2<-la
              resulttemp<-rbind(resulttemp,temp)
              co<-co+1
              if(co/30==floor(co/30)){
                resulttemp2<-rbind(resulttemp2,resulttemp)
                resulttemp<-data.frame()
               #print(co)
              }
            }
            
            resulttemp<-rbind(resulttemp2,resulttemp)
            
            resulttest<-rbind(resulttest,resulttemp)
          }else{
            rdn<-round(length(a)*0.2,0)
            
            rd<-sample(length(a),rdn)
            c<-a[rd]
            a<-a[-rd]
            for (shift in -14:14) {
              for (b in a) {

                temp<-data[(b+bound[2]*interval+shift-30):(b+bound[2]*interval+shift),2:ncol(data)]
                temp$Label<-paste0(mice,"_",days,"_","press_",co)
                temp$Label2<-la
                resulttemp<-rbind(resulttemp,temp)
                co<-co+1
                if(co/30==floor(co/30)){
                  resulttemp2<-rbind(resulttemp2,resulttemp)
                  resulttemp<-data.frame()
                  #print(co)
                }
              }
              
            }
            resulttemp<-rbind(resulttemp2,resulttemp)
            
            result<-rbind(result,resulttemp)
            resulttemp<-data.frame()
            resulttemp2<-data.frame()
            
            for (shift in -14:14) {
              for (b in c) {
                
                
                temp<-data[(b+bound[2]*interval+shift-30):(b+bound[2]*interval+shift),2:ncol(data)]
                temp$Label<-paste0(mice,"_",days,"_","press_",co)
                temp$Label2<-la
                resulttemp<-rbind(resulttemp,temp)
                co<-co+1
                if(co/30==floor(co/30)){
                  resulttemp2<-rbind(resulttemp2,resulttemp)
                  resulttemp<-data.frame()
                  #print(co)
                }
                
                
                
              }
              
              
            }
            
            resulttemp<-rbind(resulttemp2,resulttemp)
            resulttest<-rbind(resulttest,resulttemp)
          }
          
        }
        Allresult<-rbind(Allresult,result)
        Allresulttest<-rbind(Allresulttest,resulttest)
      }
    }
  }
  
  write.csv(Allresult,file = paste0(outputdir,"train_set_combine",bound[1],bound[2],"_31fr.csv"),row.names = FALSE)
  write.csv(Allresulttest,file = paste0(outputdir,"test_set_combine",bound[1],bound[2],"_31fr.csv"),row.names = FALSE)
  
  
}
