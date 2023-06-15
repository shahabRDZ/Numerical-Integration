rm(list=ls())
n=1000
data=data.frame(matrix(rep(10000,n*9),ncol = 9))
colnames(data)=c("airplane","entranceTime","lane0Status","lane1Status","queueLength","laneChosen","landingTime","landingDuration","departureTime")
lamda=10 #per hr
mu=6 #per hr


#ENTRANCE
for(i in 1:(n-1)){
  data$airplane[i]=i
  data$entranceTime[1]=0
  data$entranceTime[i+1]=data$entranceTime[i]+60*(-1/lamda*log(1-runif(1,0,1)))
}
data$airplane[n]=n
data$entranceTime[n]=data$entranceTime[n-1]+60*(-1/lamda*log(1-runif(1,0,1)))

#QUEUE
queue=data.frame(matrix(rep(0,0),ncol = 2))
colnames(queue)=c("airplane","entranceTime")

#SERVER
server=data.frame(matrix(rep(10000,2*2),ncol = 2))
colnames(server)=c("airplane","departureTime")

#1ST AIRPLANE
data$lane0Status[1]=1
data$lane1Status[1]=0
data$laneChosen[1]=0
data$queueLength[1]=0
data$landingTime[1]=0
data$landingDuration[1]=60*(-1/mu*log(1-runif(1,0,1)))
data$departureTime[1]=data$landingTime[1]+data$landingDuration[1]
server$airplane[1]=1
server$departureTime[1]=data$departureTime[1]


#OTHER AIRPLANES
for(i in 2:n){
while (data$entranceTime[i]!=min(server$departureTime[1],server$departureTime[2],data$entranceTime[i])) {
  if(server$departureTime[1]==min(server$departureTime[1],server$departureTime[2],data$entranceTime[i])){
    if(dim(queue)[1]>0){
      data$laneChosen[queue$airplane[1]]=0
      data$lane0Status[queue$airplane[1]]=1
      data$landingTime[queue$airplane[1]]=server$departureTime[1]
      data$landingDuration[queue$airplane[1]]=60*(-1/mu*log(1-runif(1,0,1)))
      data$departureTime[queue$airplane[1]]=data$landingTime[queue$airplane[1]]+data$landingDuration[queue$airplane[1]]
      server$airplane[1]=queue$airplane[1]
      server$departureTime[1]=data$departureTime[queue$airplane[1]]
      queue=queue[-1,]
    }else{
      server$airplane[1]=10000
      server$departureTime[1]=10000
    }
  }
  if(server$departureTime[2]==min(server$departureTime[1],server$departureTime[2],data$entranceTime[i])){
    if(dim(queue)[1]>0){
      data$laneChosen[queue$airplane[1]]=1
      data$lane1Status[queue$airplane[1]]=1
      data$landingTime[queue$airplane[1]]=server$departureTime[2]
      data$landingDuration[queue$airplane[1]]=60*(-1/mu*log(1-runif(1,0,1)))
      data$departureTime[queue$airplane[1]]=data$landingTime[queue$airplane[1]]+data$landingDuration[queue$airplane[1]]
      server$airplane[2]=queue$airplane[1]
      server$departureTime[2]=data$departureTime[queue$airplane[1]]
      queue=queue[-1,]
    }else{
      server$airplane[2]=10000
      server$departureTime[2]=10000
    }
  }
}
  data$lane0Status[i]=ifelse(server$airplane[1]==10000,0,1)
  data$lane1Status[i]=ifelse(server$airplane[2]==10000,0,1)
  data$queueLength[i]=dim(queue)[1]
  if(data$lane0Status[i]==0){
    data$laneChosen[i]=0
    data$lane0Status[i]=1
    data$landingTime[i]=data$entranceTime[i]
    data$landingDuration[i]=60*(-1/mu*log(1-runif(1,0,1)))
    data$departureTime[i]=data$landingTime[i]+data$landingDuration[i]
    server$airplane[1]=i
    server$departureTime[1]=data$departureTime[i]
  } else if(data$lane1Status[i]==0){
    data$laneChosen[i]=1
    data$lane1Status[i]=1
    data$landingTime[i]=data$entranceTime[i]
    data$landingDuration[i]=60*(-1/mu*log(1-runif(1,0,1)))
    data$departureTime[i]=data$landingTime[i]+data$landingDuration[i]
    server$airplane[2]=i
    server$departureTime[2]=data$departureTime[i]
  }else{
    data$queueLength[i]=dim(queue)[1]+1
    queue=rbind(queue,c(data$airplane[i],data$entranceTime[i]))
    colnames(queue)=c("airplane","entranceTime")
  }
  
}

output1=c("Mean Queue Length", "Mean Waiting In Queue", "Mean Waiting In System")
Output11=c(mean(data$queueLength),mean(data$landingTime-data$entranceTime),mean(data$departureTime-data$entranceTime))
output=data.frame(output1,Output11)


