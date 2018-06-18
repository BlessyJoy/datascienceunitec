pollutantmean<-function(directory,pollutant,id=1:332){
  filelist<-list.files(path = directory,pattern = ".csv",full.names = TRUE)
  values<-numeric()
  
  for(i in id){
    data<-read.csv(filelist[i])
    values<-c(values,data[[pollutant]])
    
  }
  mean(values,na.rm = TRUE)
}

pollutantmean("C:/Users/Blessy Edward/Desktop/R-data science/Assignment Part A-20180305 (1)/rprog_assignment_dataset/specdata","sulfate")
