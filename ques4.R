pollutantvector<-function(directory,pollutant,id,p)
{
  filelist<-list.files(path=directory,pattern = ".csv",full.names = TRUE)
  result<-numeric()
  for(i in id){
    data<-read.csv(filelist[i])
    data1<-na.omit(data)
    for(j in data1){
      if(data1[[pollutant]]>p){
        result<-c(result,data1[[pollutant]])
      }
    }
    result
  }
}