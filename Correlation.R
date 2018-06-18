corr<-function(directory,threshold){
  filelist<-list.files(path=directory,pattern = ".csv",full.names = TRUE)
  id=1:332
  result<-0
  correlation <-numeric()
  for(i in id){
    data<-read.csv(filelist[i])
    comp<-sum(complete.cases(data))
    if(comp>threshold){
      data1<-na.omit(data)
      x<-data1$sulfate
      y<-data1$nitrate
      result<-cor(x,y)}
    else
      result<-0
      
    
      correlation<-c(correlation, result)
  }
  correlation
  }
  

  