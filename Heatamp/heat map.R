#only need install once
install.packages("stringr")
install.packages("tidyr")
install.packages("lattice")

### import library
library(stringr)
library(tidyr)
library(lattice)

### mice name list
wildlist<-c("152","153","174","175","177","180","181")


set.seed(123)

### set directory
sampledir<-"D:/Xiguang/sample_data/"

#if k-mean applied set cluster Num
clusterNum<-3

### method included "ABvalue","Simplemax","fire_-5s","fire_-2.5s","fire_0s" and "Kmean_middle_SUC" for
### sorting by increasing, peak value, activity at -5s, -2.5s, 0s and Kmean clustering
for (method in c("Kmean_middle_SUC")) {
  CD<-10
  
  for (listname in c("WildType")) {
    if(listname=="WildType"){
      runlist<-wildlist
    }
    
    for (days in c("Day11")) {
### "A" for ALP and "R1" "R2" for Random baselines
      for (RD in c("A","R1")) {
        t<-1
        temptotal<-data.frame()
        for (name in runlist) {
          
          A<-NULL
          
          
          
          mice<-str_extract_all(name,"[0-9A-Z]+")[[1]][1]
          
          data<-read.csv(file=paste0(sampledir,"z_score/",name,"_",days,"_standardized.csv"),check.names = FALSE)
          
          
          data<-data[data$`Time(s)`!=0,]
          
          if(RD=="A"){
            la<-"ALP"
            press<-read.csv(file = paste0(sampledir,"press_timing/",name,"_",days,"_Lever_Press_Timing.csv"),check.names = FALSE)
            
            colnames(press)[1]<-"Time(s)"
            # remove lever press within 10s
            x=1
            while (x<=nrow(press)-1) {
              if(press[x,1]>=press[x+1,1]-CD){
                y=x+1
                while (y<=nrow(press)-1) {
                  if(press[y,1]>=press[y+1,1]-0){
                    y<-y+1
                  }else{
                    break()
                  }
                  
                }
                
                press<-data.frame(`Time(s)`=press[-c((x+1):y),1],check.names = FALSE) 
              }else{
                x=x+1
              }
              
            }
            a<-unlist(press$`Time(s)`)
            a<-a*10
            a<-a[a>100&a<nrow(data)-100]
            A<-length(a)
            if(A==0){
              print(c(name,days))
              next}
          }
          
          
          if(RD=="I"){
            
            la<-"ILP"
            press<-read.csv(file = paste0(sampledir,"press_timing/",name,"_",days,"_Inactive_Lever_Press_Timing.csv"),check.names = FALSE)
            
            colnames(press)[1]<-"Time(s)"
            x=1
            while (x<=nrow(press)-1) {
              if(press[x,1]>=press[x+1,1]-CD){
                
                
                
                y=x+1
                while (y<=nrow(press)-1) {
                  if(press[y,1]>=press[y+1,1]-0){
                    y<-y+1
                  }else{
                    break()
                  }
                  
                }
                
                press<-data.frame(`Time(s)`=press[-c((x+1):y),1],check.names = FALSE) 
                print(name)
                print("F")
              }else{
                x=x+1
              }
              
            }
            a<-unlist(press$`Time(s)`)
            a<-a*10
            a<-a[a>100&a<nrow(data)-100]
            A<-length(a)
          }
          if(RD %in% c("R1","R2")) {
            la<-RD
            press<-read.csv(file = paste0(sampledir,"press_timing/",name,"_",days,"_Lever_Press_Timing.csv"),check.names = FALSE)
            colnames(press)[1]<-"Time(s)"
            a<-unlist(press$`Time(s)`)
            a<-a*10
            A<-length(a)
            press<-read.csv(file = paste0(sampledir,"press_timing/",name,"_",days,"_Inactive_Lever_Press_Timing.csv"),check.names = FALSE)
            colnames(press)[1]<-"Time(s)"
            c<-unlist(press$`Time(s)`)
            c<-c*10
            a<-c(unlist(a),unlist(c))
            
            list<-unlist(data$`Time(s)`)*10
            list<-list[list>100&list<nrow(data)-100]
            list2<-list()
            for (b in a) {
              list2<-c(unlist(list2),(b-100):(b+100))
            }
            list<-setdiff(list,list2)
            a<-sample(list,A)
            
            a<-a[a>100&a<nrow(data)-100]
            #print(list)
            
          }
          i<-1
          if(A>0){
            for (b in a) {
              if(i==1){
                temp<-data[(b-99):(b+100),(2):ncol(data)]
                i=i+1
              }else{
                temp<-temp+data[(b-99):(b+100),(2):ncol(data)]
                
              }
            }
            
            
            temp<-temp/length(a)
            
            for (sss in 1:ncol(temp)) {
              temp[,sss]<-temp[,sss]-mean(temp[,sss])
            }
            if(t==1){
              temptotal<-temp
              t<-2
            }else{
              temptotal<-cbind(temptotal,temp)
            }
          }
          
          
          
        }
        
        
        rank<-data.frame(rank=1:ncol(temptotal),N=0)
        
        if(method=="Simplemax"){
          for (y in 1:ncol(temptotal)) {
            rank[y,2]<-which.max(temptotal[,y])
            
          }
        }
        
        if(method=="fire_-5s"){
          for (y in 1:ncol(temptotal)) {
            rank[y,2]<-(-(mean(temptotal[41:60,y])))
            
          }
        }
        if(method=="fire_-2.5s"){
          for (y in 1:ncol(temptotal)) {
            rank[y,2]<-(-(mean(temptotal[66:85,y])))
            
          }
        }
        if(method=="fire_0s"){
          for (y in 1:ncol(temptotal)) {
            rank[y,2]<-(-(mean(temptotal[91:110,y])))
            
          }
        }
        
        if(method=="ABvalue"){
          for (y in 1:ncol(temptotal)) {
            if(max(temptotal[,y])>=0.5){
              rank[y,2]<-which(temptotal[,y]>=0.5)[1]
            }else{
              if(max(temptotal[,y])>=0.25){
                rank[y,2]<-which(temptotal[,y]>=0.25)[1]
              }else{
                rank[y,2]<-which(temptotal[,y]>=0)[1]
              }
              
            }
            
          }
        }
        
        
        if(method=="Kmean_middle_SUC"){
          
          mporder<-NULL
          rank<-data.frame()
          l1<-list()
          l2<-list()
          temptotalcopy<-temptotal
          temp<-kmeans(t(temptotalcopy[1:150,]),centers=clusterNum)
          for(CN in 1:clusterNum){
            print(ncol(temptotal[,as.numeric(which(temp$cluster==CN))]))
            l1<-append(l1,list(temptotal[,as.numeric(which(temp$cluster==CN))]))
          }
          addNum<-0
          clustercount<-0
          for(tempdata in l1){
            clustercount<-clustercount+1
            tempdatacopy<-tempdata
            tempdata[tempdata<0]<-0
            rankt<-data.frame(rank=integer(ncol(tempdata)),N=integer(ncol(tempdata)))
            mplist<-NULL
            frontlist<-NULL
            lastlist<-NULL
            
            for (y in 1:ncol(tempdata)) {
              tempdatatemp<-tempdata[,y]
              tempdatatemp2<-tempdatacopy[,y]
              tempdatatemp<-tempdatatemp-min(tempdatatemp)
              mp<-1
              
              mp<-mean(which(tempdatatemp2[1:150]>quantile(tempdatatemp2[1:150],0.95)))
              weightmp<-round(quantile(tempdatatemp2[1:150],0.95)*4)
              
              if(weightmp<=1){
              }else{
                wmp <- 1
                while (wmp<=weightmp){
                  mplist<-c(mplist,mp)
                  wmp <- wmp + 1
                }
              }
              
              
            }
            if(length(mplist)<1){
              mp<-100
            }else{
              if(length(mplist)==1){
                mp<-round(mean(mplist))
              }else{
                if(sd(mplist)!=0){
                  mp<-round(mean(mplist[mplist>mean(mplist)-2*sd(mplist)&mplist<mean(mplist)+2*sd(mplist)]))
                }else{
                  mp<-round(mean(mplist))
                }
                
              }
            }
            
            if(mp>191){
              mp<-191
            }
            if(mp<11){
              mp<-11
            }
            for (y in 1:ncol(tempdatacopy)) {
              rankt[y,2]<-(-(mean(tempdatacopy[(mp-5):(mp+4),y])))
              
              
            }
            rank<-rankt
            rank[,1]<-rank(rank[,2],ties.method = c( "first"),)
            rank[,1]<-(-rank[,1])+max(rank[,1])+1
            tempdatacopy<-tempdatacopy[,order(rank[,2],decreasing = T)]
            mporder<-c(mporder,mp)
            l2<-append(l2,list(tempdatacopy))
            print(ncol(tempdatacopy))
          }
          templist<-l2
          mporder<-order(mporder,decreasing = T)
          
          for(CN in 2:clusterNum){
            if(CN ==2){
              temptotal<-cbind(as.data.frame(templist[mporder[1]],check.names = FALSE),
                               as.data.frame(templist[mporder[2]],check.names = FALSE))
            }else{
              temptotal<-cbind(temptotal,
                               as.data.frame(templist[mporder[CN]],check.names = FALSE))
            }
            
          }
          
        }
        if(method!="Kmean_middle_SUC"){
          rank[,1]<-rank(rank[,2],ties.method = c( "first"),)
          rank[,1]<-(-rank[,1])+max(rank[,1])+1
          temptotal<-temptotal[,order(rank[,2],decreasing = T)]
        }
        
        rgb <- colorRampPalette(c("blue","white","red"), space = "Lab")
        result<-as.matrix(temptotal)
        colnames(result)<-1:ncol(result)
        rownames(result)<-1:nrow(result)
        new_result<-pivot_longer(as.data.frame(result),cols = 1:ncol(result))
        new_result$y<-c(rep((-99:100)/10,each=nrow(new_result)/200))
        new_result$name<-as.numeric(new_result$name)
        
        plot<-plot(levelplot(value ~ y * name,data=new_result,col.regions=rgb(120),at=c(-15,seq(-0.5,0.5,length=30),15),scales=list(
          x=list(at=c(1:floor(nrow(result/25)))*2.5-100),
          y=list(at=c(1:floor(ncol(result/50)))*50)),
          main=paste0(method,"_",listname,"_",RD,"_",days),aspect="fill",xlab="Time (sec)",ylab="Cell Number"))
        print(plot)
        
        
        
        
      }
    }
  }
}



