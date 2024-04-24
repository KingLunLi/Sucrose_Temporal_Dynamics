
#only need install once
install.packages("R.matlab")
install.packages("spatialEco")
install.packages("shape")


### import library
library(R.matlab)
library(spatialEco)
library(shape)







direct<-"specify your directory/"
DF <- readMat(paste0(direct,"PittData_20240213"))
micelist<-c("example1")
days="Day11"

### "ALP" for lever press and "R1" "R2" for Random baselines
for (RD in c("ALP","R1")) {
  Num=1
  dnu=0
  temptotal<-data.frame()
  PID<-1
  set.seed(123)
  for(x in DF[(1)*2-1]){
    type<-"D1"

    mice<-micelist[Num]
    Time<-data.frame(Time=unlist(x[1]))
    data<-data.frame(x[3])
    data<-cbind(Time,data)
    colnames(data)<-c("Time","C1","C2","C3","C4","C5","C6","C7")
    data<-data[data$Time!=0,]
    data<-unique(data)
    data<-data[is.na(data[,2])==FALSE,]
    if(nrow(data)!=round(max(data$Time*10))){
      y<-1
      while(y<=nrow(data)-1){
        if(round(data[y,1],1)!=round(data[y+1,1]-0.1,1)){
          #print(y)
          for (dim in 2:8) {
            if(dim==2){
              temp<-approx(x=c(data[y,1],data[y+1,1]),y=c(data[y,dim],data[y+1,dim]),n=round((data[y+1,1]-data[y,1])*10+1))
              tempdata<-data.frame(Time=temp$x,C1=temp$y)
              tempdata<-tempdata[-c(1,nrow(tempdata)),]
            }else{
              temp<-approx(x=c(data[y,1],data[y+1,1]),y=c(data[y,dim],data[y+1,dim]),n=round((data[y+1,1]-data[y,1])*10+1))
              temp<-as.data.frame(temp$y)
              temp<-as.data.frame(temp[-c(1,nrow(temp)),])
              colnames(temp)<-paste0("C",dim-1)
              tempdata<-cbind(tempdata,temp)
            }
          }
          data<-rbind(data[1:y,],tempdata,data[(y+1):nrow(data),])
        }
        y=y+1
      }
    }
    if(nrow(data)!=round(max(data$Time*10))){
      print("Interpolation Fail")
      break
    }
    
    la<-"ALP"
    press<-read.csv(file = paste0(direct,"press_timing/",mice,"_",days,"_Lever_Press_Timing.csv"),check.names = FALSE)
    
    colnames(press)[1]<-"Time(s)"
    
    x=1
    while (x<=nrow(press)-1) {
      if(press[x,1]>=press[x+1,1]-10){
        
        
        
        y=x+1
        while (y<=nrow(press)-1) {
          if(press[y,1]>=press[y+1,1]-0){
            y<-y+1
          }else{
            break()
          }
          
        }
        
        press<-data.frame(`Time(s)`=press[-c((x+1):y),1],check.names = FALSE) 
        #print(name)
        #print("F")
      }else{
        x=x+1
      }
      
    }
    a<-unlist(press$`Time(s)`)
    a<-a*10
    a<-a[a>100&a<nrow(data)-100]
    A<-length(a)
    
    
    if(RD %in% c("R1","R2")) {
      la<-RD
      press<-read.csv(file = paste0(direct,"press_timing/",mice,"_",days,"_Lever_Press_Timing.csv"),check.names = FALSE)
      colnames(press)[1]<-"Time(s)"
      
      x=1
      while (x<=nrow(press)-1) {
        if(press[x,1]>=press[x+1,1]-10){
          
          y=x+1
          while (y<=nrow(press)-1) {
            if(press[y,1]>=press[y+1,1]-0){
              y<-y+1
            }else{
              break()
            }
            
          }
          
          press<-data.frame(`Time(s)`=press[-c((x+1):y),1],check.names = FALSE)
          #print(name)
          #print("F")
        }else{
          x=x+1
        }
        
      }
      a<-unlist(press$`Time(s)`)
      a<-a*10
      a<-a[a>100&a<nrow(data)-100]
      A<-length(a)
      
      
      
      list<-unlist(data$`Time`)*10
      list<-list[list>100&list<nrow(data)-100]
      list2<-list()
      for(xp in 1:A){
        list2temp<-sample(list,1)
        list<-setdiff(list,(list2temp-100):(list2temp+100))
        if(length(list)<1){
          print("error")
        }
        list2<-c(unlist(list2),list2temp)
      }
      a<-unlist(list2)
      a<-a[a>100&a<nrow(data)-100]
      A<-length(a)
      #print(list)
    }
    
    
    
    if(A==0){
      print(c(name,days))
      next}
    
    if(A>0){
      for (b in a) {
        temp<-data[(b-100):(b+100),]
        temp$Time<-(1:201)
        temp$list<-type
        temp$PID<-PID
        temp$group<-RD
        temp$mice<-mice
        temptotal<-rbind(temptotal,temp)
        PID<-PID+1
      }
      
      
    }
    
    Num<-Num+1
  }
  
  if(RD=="ALP"){
    resulttotal_ALP<-temptotal
  }else{
    resulttotal_RD<-temptotal
  }
  
  
  
}
########### calculation
anglecalculation<-function(x){
  return((atan2(x[2],x[1])*180/pi+360) %% 360)
}
(atan2(0,-1)*180/pi+360) %% 360
recordlenD1<-list()
recordangD1<-list()
for(arrowlen in c(15)){
  
  dim1<-2
  dim2<-3
  
  
  for (RD in c("ALP","R1")) {
    if(RD=="ALP"){
      resulttotal<-resulttotal_ALP
    }else{
      resulttotal<-resulttotal_RD
    }
    PID<-max(resulttotal$PID)
    totnum<-(201-arrowlen)*(PID)
    result<-data.frame(Angle= rep(NA,totnum),Time= rep(NA,totnum),x1= rep(NA,totnum),x2= rep(NA,totnum),
                       y1 = rep(NA,totnum), y2= rep(NA,totnum), group= rep(NA,totnum),
                       list= rep(NA,totnum), mice= rep(NA,totnum), PID= rep(NA,totnum))
    for (time in (arrowlen+1):201) {
      temp2<-resulttotal[resulttotal$Time==time,]
      temp1<-resulttotal[resulttotal$Time==time-arrowlen,]
      temp3<-temp2[,c(dim1,dim2)]-temp1[,c(dim1,dim2)]
      temp4<-data.frame(Angle=unlist(apply(temp3,1,anglecalculation)))
      temp4$Time<-temp2$Time
      temp4$x1<-temp1[,dim1]
      temp4$x2<-temp2[,dim1]
      temp4$y1<-temp1[,dim2]
      temp4$y2<-temp2[,dim2]
      temp4$group<-temp1$group
      temp4$list<-temp1$list
      temp4$mice<-temp1$mice
      temp4$PID<-temp1$PID
      result[((PID)*(time-arrowlen-1)+1):((PID)*(time-arrowlen)),]<-temp4
      #print(time)
    }
    
    
    if(RD=="ALP"){
      result_ALP<-result
    }else{
      result_RD<-result
    }
    length_filter<-0.5
    rownames(result)<-NULL
    
  }
  for(limiteangle in c(75)){
    for (RD in c("ALP","R1")) {
      if(RD=="ALP"){
        result<-result_ALP
      }else{
        result<-result_RD
      }
      anglepercent<-data.frame()
      for (mice in unique(result$mice)) {
        temp<-result[result$mice==mice,]
        temp4<-data.frame()
        for (time in unique(result$Time)) {
          temp2<-temp[temp$Time==time,]
          dis<-sqrt((temp2$x2-temp2$x1)^2+(temp2$y2-temp2$y1)^2)
          temp2<-temp2[dis>=mean(dis),]
          ma<-mean_angle(temp2$Angle,angle = "degree")
          temp2$samedirect<-FALSE
          upperbound<-ma+limiteangle
          lowerbound<-ma-limiteangle
          if(ma<limiteangle|ma>360-limiteangle){
            if(upperbound>360){
              upperbound<-upperbound-360
            }
            
            if(lowerbound<0){
              lowerbound<-lowerbound+360
            }
            temp2$samedirect[temp2$Angle<=upperbound]<-TRUE
            temp2$samedirect[temp2$Angle>=lowerbound]<-TRUE
          }else{
            temp2$samedirect[temp2$Angle<=upperbound&temp2$Angle>=lowerbound]<-TRUE
          }
          temp3<-data.frame(Time=time,Per=sum(temp2$samedirect)/nrow(temp2),mice=mice,samenum=sum(temp2$samedirect),diffnum=nrow(temp2)-sum(temp2$samedirect))
          temp4<-rbind(temp4,temp3)
          
        }

        anglepercent<-rbind(anglepercent,temp4)
      }
      
      time<-c(50:60)
      listpercent<-list()
      for (mice in unique(result$mice)) {
        target<-anglepercent$Per[anglepercent$mice==mice&anglepercent$Time %in% time]
        listpercent<-c(unlist(listpercent),max(target))
      }
      if(RD=="ALP"){
        listpercent_ALPD1<-listpercent
      }else{
        listpercent_RDD1<-listpercent
      }
    }
    # The line below will output a p-value from paired t-test comparing random vs ALP arrow directions. Uncomment this line with more than one sample.
    #print(t.test(listpercent_ALPD1[1:5],listpercent_RDD1[1:5],paired = TRUE)$p.value)
  }
  print(arrowlen)
}


### example plot

RD="ALP"
if(RD=="ALP"){
  result<-result_ALP
}else{
  result<-result_RD
}

mice<-"example1"

###
time<-53 ##ALP
#time<-56 ##Random
temp<-result[result$mice==mice,]
temp4<-data.frame()
temp2<-temp[temp$Time==time,]
dis<-sqrt((temp2$x2-temp2$x1)^2+(temp2$y2-temp2$y1)^2)
temp2<-temp2[dis>=mean(dis),]
ma<-mean_angle(temp2$Angle,angle = "degree")
temp2$samedirect<-FALSE
upperbound<-ma+limiteangle
lowerbound<-ma-limiteangle
if(ma<limiteangle|ma>360-limiteangle){
  if(upperbound>360){
    upperbound<-upperbound-360
  }
  
  if(lowerbound<0){
    lowerbound<-lowerbound+360
  }
  temp2$samedirect[temp2$Angle<=upperbound]<-TRUE
  temp2$samedirect[temp2$Angle>=lowerbound]<-TRUE
}else{
  temp2$samedirect[temp2$Angle<=upperbound&temp2$Angle>=lowerbound]<-TRUE
}
sum(temp2$samedirect)/nrow(temp2)

temp$group<-0
temp$group[temp$Time==time-15]<-1
temp$group[temp$Time==time]<-2


colors <- c(rgb(173, 216, 230,maxColorValue = 255, 10), 
            "blue", # Light green
            "red")

#  1st plot - plots with all points and arrows

plot(temp$x2[temp$group==0],temp$y2[temp$group==0],pch = 19,col = rgb(173, 216, 230,maxColorValue = 255, 30) ,main="Example Arrow Plots",xlim=c(-0.08,0.1),ylim=c(-0.08,0.1))
points(temp$x2[temp$group==1],temp$y2[temp$group==1],pch = 19,col = "blue")
points(temp$x2[temp$group==2],temp$y2[temp$group==2],pch = 19,col = "red")
Arrows(temp2$x1[temp2$samedirect==TRUE], temp2$y1[temp2$samedirect==TRUE], 
       x1 = temp2$x2[temp2$samedirect==TRUE], y1 = temp2$y2[temp2$samedirect==TRUE],arr.adj = 1,arr.type="curved",lty=1,col="blue")
Arrows(temp2$x1[temp2$samedirect==FALSE], temp2$y1[temp2$samedirect==FALSE], 
       x1 = temp2$x2[temp2$samedirect==FALSE], y1 = temp2$y2[temp2$samedirect==FALSE],arr.adj = 1,arr.type="curved",lty=1,col="red")
#  2nd plot - plots with arrows 
plot(temp$x2,temp$y2,pch = 19,col = rgb(173, 216, 230,maxColorValue = 255, 30) ,main="Example Arrow Plots",xlim=c(-0.08,0.1),ylim=c(-0.08,0.1))
Arrows(temp2$x1[temp2$samedirect==TRUE], temp2$y1[temp2$samedirect==TRUE], 
       x1 = temp2$x2[temp2$samedirect==TRUE], y1 = temp2$y2[temp2$samedirect==TRUE],arr.adj = 1,arr.type="curved",lty=1,col="blue")
Arrows(temp2$x1[temp2$samedirect==FALSE], temp2$y1[temp2$samedirect==FALSE], 
       x1 = temp2$x2[temp2$samedirect==FALSE], y1 = temp2$y2[temp2$samedirect==FALSE],arr.adj = 1,arr.type="curved",lty=1,col="red")

#  3rd plot - plots directions
plot(0,0,pch = 19,col = "white" ,main="Example Directions",xlim=c(-0.04,0.04),ylim=c(-0.04,0.04))

Arrows(x0 = 0,y0 = 0,x1 = 0+cos(upperbound*pi/180)/80, y1 = 0+sin(upperbound*pi/180)/80, arr.type="curved",lwd=2)
Arrows(x0 = 0,y0 = 0,x1 = 0+cos(lowerbound*pi/180)/80, y1 = 0+sin(lowerbound*pi/180)/80, arr.type="curved",lwd=2)
Arrows(x0 = 0,y0 = 0,x1 = 0+cos(ma*pi/180)/80, y1 = 0+sin(ma*pi/180)/80, arr.type="curved",lwd=2,col="blue")
Arrows(x0 = 0,y0 = 0,x1 = 0+cos((ma-180)*pi/180)/80, y1 = 0+sin((ma-180)*pi/180)/80, arr.type="curved",lwd=2,col="red")

