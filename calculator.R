#defining Function
calc<-function(){
  print("Simple R Calculator - Select operation:")
  print("1.Add")
  print("3.Multiply")
  print("4.Divide")
  print("5.Factors")
  print("6.Prime")
  #reading choice from the user
  choice <- as.integer(readline("Enter Choice [1/2/3/4/5/6]: "))
  #Bypassing invalid choice
  if(choice> 6){print("Error")
    break}
#the operation is unary, read 1 number
    if(choice==5||choice==6){
    n1<-as.integer(readline("Enter the number: "))
  #to find the factors of a number
      if(choice==5){
      print(paste("The factors of",n1,"are:"))
      for(i in 1:n1){
        if((n1%%i)==0){
          print(i)
        }
      }
    }
  #to check whether the number is prime or not  
    else if(choice==6){
      flag=0
      if(n1>1){
        flag=1
        for(i in 2:(n1-1)){
          if((n1%%i)==0){
            flag=0
            break
          }
        }
      }
      if(n1==2) flag=1
      if(flag==1)
        print(paste(n1,"is a Prime number"))
            else 
              print(paste(n1,"is not a Prime number"))
          }
      }
  else{
        a<-as.numeric(readline(prompt = "Enter first num"))
    b<-as.integer(readline(prompt = "Enter sencond number"))
   
    #Addition
    add<-function(x,y){return (x+y)}
    #subtraction Function
    sub<-function(x,y){return (x-y)}
    #multiplication Function
    mul<-function(x,y){return (x*y)}
    #Division Function
    div<-function(x,y){return (x/y)}
    switch(choice,add(a,b), sub(a,b), mul(a,b), div(a,b))
  }
  
}
  

if(interactive())calc()