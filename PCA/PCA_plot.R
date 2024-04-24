
#only need install once
install.packages("ggplot2")
install.packages("plotrix")
install.packages("dplyr")
### import library
library(ggplot2)
library(plotrix)
library(dplyr)




options(warn=0)
resulttotal<-data.frame()
dimneed<-1:2
direct<-"D:/specify your dir/"
wildlist<-c("example1", "example2") # list of all mice IDs in this group but only one listed for demo (e.g. example1_Day11Trials_ALP_PC1_it0.csv, example2_Day11Trials_ALP_PC1_it0.csv, )

# the following contains nested loops that iterate over the specified iterations (itr), days (days), and group ("WT" in this code)
# within these loops, data is read from CSV files and processed to compute the squared differences between active lever press (ALP) and baseline (BL2) samples along the specified principal components. 
# the results are accumulated in the resulttotal data frame.
for (itr in 0:4){
  print(itr)
  
  for (days in c("Day11")) {
    print(days)
    for (listname in c("WT")) {

      if(listname=="WT"){
        runlist<-wildlist
      }

      result<-data.frame()
      for(mice in runlist){
        print(mice)
        MAXVA<-0
        
        for(RD in c("ALP","BL2")){
          if(file.exists(paste0(direct,mice,"_",days,"trials_",RD,"_PC1_it",itr,".csv"))==TRUE){
            PC1A<-t(read.csv(paste0(direct,mice,"_",days,"trials_",RD,"_PC1_it",itr,".csv"),header = FALSE))
            
            if(ncol(PC1A)>=3){
              
              dis<-0
              for (dim in dimneed) {
                tempsample<-t(read.csv(paste0(direct,mice,"_",days,"trials_",RD,"_PC",dim,"_it",itr,".csv"),header = FALSE))
                tempsample2<-t(read.csv(paste0(direct,mice,"_",days,"trials_BL1_PC",dim,"_it",itr,".csv"),header = FALSE))
                tempsample<-tempsample
                tempsample2<-tempsample2
                tempsample<-data.frame("PCX"=unlist(apply(tempsample,1,mean)))
                tempsample2<-data.frame("PCX"=unlist(apply(tempsample2,1,mean)))
                
                
                dis<-dis+(tempsample-tempsample2)^2
                if(dim==1){
                  sample1<-tempsample
                  sample2<-tempsample2
                }else{
                  sample1<-cbind(sample1,tempsample)
                  sample2<-cbind(sample2,tempsample2)
                }
              }
              
              
              dissq<-(sample1-sample2)^2
              resulttemp<-dissq
              colnames(resulttemp)<-paste0("PCDISSQ_",dimneed)
              resulttemp$dis<-unlist(dis)
              
              if(RD=="ALP"){
                MAXVA<-max(resulttemp$dis)
              }
              resulttemp$time<-1:301
              resulttemp$group<-RD
              resulttemp$Day<-days
              resulttemp$list<-listname
              resulttemp$mice<-mice
              resulttemp$itr<-itr
              resulttemp$dis<-resulttemp$dis/MAXVA
              resulttemp$dis<-sqrt(resulttemp$dis)
              
              result<-rbind(result,resulttemp)
              
              
            }
            
          }else{
            print(paste0(mice," ",days,"not exist"))
          }
        }
        
        
        
        
        
      }
      resulttotal<-rbind(resulttotal,result)
    }
    
    
  }
  
}

# the following lines calculate the mean euclidean distance and the standard error of the mean (SEM) based on the mean euclidean distances
# then prints the plot, displaying the relative distance and SEM over time for the specified day and list name
for (days in c("Day11")) {
  
  for (listname in c("WT")) {
      result<-aggregate(x=resulttotal$dis,by=list(resulttotal$time,resulttotal$group,resulttotal$Day,resulttotal$list),mean)
      plotresult<-result[result$Group.3==days&result$Group.4==listname,]
      semresult<-aggregate(x=resulttotal$dis,by=list(resulttotal$time,resulttotal$group,resulttotal$Day,resulttotal$list,resulttotal$mice),mean)
      semresult<-aggregate(x=semresult$x,by=list(semresult$Group.1,semresult$Group.2,semresult$Group.3,semresult$Group.4),std.error)
      semplotresult<-semresult[semresult$Group.3==days&semresult$Group.4==listname,]
      plotresult<-left_join(plotresult,semplotresult,by = c("Group.1", "Group.2", "Group.3", "Group.4"))
      plotresult$Group.1<-plotresult$Group.1-150
      plotresult<-plotresult[plotresult$Group.1>=-100&plotresult$Group.1<=100,]
      plot<-ggplot(plotresult, aes(x = Group.1, y = x.x,group=Group.2)) + 
        geom_line(aes(group=Group.2,color=factor(Group.2)),show.legend = FALSE,size=1)+
        xlab("Time (100ms)")+ylab("Relative Distance")+ 
        scale_x_continuous(breaks=c(-100,-75,-50,-25,0,25,50,75,100,125,150,175,200))+
        geom_ribbon(aes(y = x.x, ymin = x.x - x.y, ymax = x.x + x.y, fill = factor(Group.2)), alpha = .2)+
        coord_cartesian(ylim = c(0, 1))+geom_vline(xintercept = c(-50,-25,0), linetype="dashed",  color = c("orange","green","black"), size=1.5)+
        ggtitle(paste0(listname," ",days," Relative Distance"))+       
        theme(panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.background = element_rect(fill='transparent'),
              legend.box.background = element_rect(fill='transparent'),
              panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
              plot.title = element_text(hjust = 0.5),text = element_text(size=20)
        )+guides(fill=guide_legend(title="Group(with SEM)"))+
        scale_fill_discrete(labels=c("Active Lever Press", "Baseline"))
      print(plot)
      
    
  }
}
