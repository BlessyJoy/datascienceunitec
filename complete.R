complete<-function(directory,id=1:332)
{
  filelist<-list.files(path = directory,pattern = ".csv",full.names = TRUE)
  var<-numeric()
  for(i in id){
    data<-read.csv(filelist[i])
    var<-c(var, sum(complete.cases(data)))
  }  
  data.frame(id,var)
}

complete("C:/Users/Blessy Edward/Desktop/R-data science/Assignment Part A-20180305 (1)/rprog_assignment_dataset/specdata",1:20)
