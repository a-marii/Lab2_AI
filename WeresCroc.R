ObservationMatrix <- function(probs, position,readings){
  Solidity=0
  Phosphate=0
  Nitrogen=0
  ProbProbs=list()
  for (i in 1:length(probs$salinity[,1])){
    Solidity=dnorm(readings[1], probs$salinity[i,1], probs$salinity[i,2])
    Phosphate=dnorm(readings[2], probs$phosphate[i,1], probs$phosphate[i,2])
    Nitrogen=dnorm(readings[3], probs$nitrogen[i,1], probs$nitrogen[i,2])
    ProbProbs[i]=(Solidity+Phosphate+Nitrogen)/3
  }
  if(!is.na(position[1]) && position[1]<0){
    pos=-position[1]
    ProbProbs[pos]=1
  }
  else if(!is.na(position[2]) && position[2]<0){
    pos=-position[2]
    ProbProbs[pos]=1
  }
  return ( ProbProbs)
  
}
BFS<- function(edges, start,goal){
  queue<-c()
  queue[length(queue)+1]=start
  visited=matrix(0,nrow = 1, ncol = 40)
  visited[start]=1
  prev=matrix(0,nrow = 1, ncol = 40)
  while (length(queue) != 0){
    node=queue[[1]]
    neighbours=which(edges[,1] == node)
    for (i in  neighbours) {
      if(visited[edges[i,2]]==0){
        queue[length(queue)+1]<-edges[i,2]
        visited[edges[i,2]]=1
        prev[edges[i,2]]=node
      }
    }
    neighbours=which(edges[,2] == node)
    for (i in  neighbours) {
      if(visited[edges[i,1]]==0){
        queue[length(queue)+1]<-edges[i,1]
        visited[edges[i,1]]=1
        prev[edges[i,1]]=node
      }
    }
    queue=queue[-1]
    
  }
  
  path=c()
  at=goal
  while(TRUE){
    path[length(path)+1] = at
    at=prev[at]
    if(at==0){
      break
    }
  }
  path=rev(path)
  return(path)
  
}

myFunction = function(moveInfo,readings,positions,edges,probs) {
  
  obs_vector=ObservationMatrix(probs,positions,  readings)
  Croc_pos=which.min(obs_vector)
  my_pos=positions[3]
  path = BFS(edges, positions[3],Croc_pos)
  if(length(path)>2){
    moveInfo$moves <- c(path[2],path[3])
  }
  else if(length(path)==2){
    moveInfo$moves <- c(path[2],0)
  }
  else if(length(path)==1){
    moveInfo$moves <- c(0,0)
  }
  print(Croc_pos)
  print(path)
  return(moveInfo)
}

