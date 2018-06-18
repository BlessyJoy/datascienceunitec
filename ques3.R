
filelist<-list.files(pattern=".csv")
y<-numeric()
for(j in 1:332)
{
  data<-read.csv(filelist[j])
  x<-na.omit(data$sulfate)
  for(i in x){
    if(x[i]>1){
      y<-c(y,x[i])}
  }
  
  }
y
