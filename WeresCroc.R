generateInitialState = function(pos)
{
  if (pos[1] == pos[2])
  {
    initialState = c(rep(1/38, 40))
    initialState[[pos[1]]] = 0
    initialState[[pos[3]]] = 0
    return (initialState)
  }
  else
  {
    initialState = c(rep(1/37, 40))
    initialState[[pos[1]]] = 0
    initialState[[pos[2]]] = 0
    initialState[[pos[3]]] = 0
    return (initialState)
  }
}

generateTransitions = function(edges) {
  tMatrix = matrix(0, nrow=40, ncol=40);
  edgeOptions = vector("list", 40)
  myList = rep(list(1), 40)
  for (i in 1:nrow(edges))
  {
    s1 = edges[i, 1]
    s2 = edges[i, 2]
    if (is.null(edgeOptions[[s1]]))
    {
      edgeOptions[[s1]] = c(s1, s2)
    }
    else {
      edgeOptions[[s1]] = append(edgeOptions[[s1]], s2)
    }
    if (is.null(edgeOptions[[s2]]))
    {
      edgeOptions[[s2]] = c(s1, s2)
    }
    else {
      edgeOptions[[s2]] = append(edgeOptions[[s2]], s1)
    }
    myList[[s1]] = myList[[s1]] + 1
    myList[[s2]] = myList[[s2]] + 1
  }
  for (i in 1:nrow(edges))
  {
    s1 = edges[i, 1]
    s2 = edges[i, 2]
    s1_trans = 1 / myList[[s1]]
    s2_trans = 1 / myList[[s2]]
    tMatrix[s1, s1] = s1_trans
    tMatrix[s1, s2] = s1_trans
    tMatrix[s2, s1] = s2_trans
  }
  res = list()
  res$tMatrix = tMatrix
  res$edgeOptions = edgeOptions
  return (res)
}

ObservationMatrix <- function(probs, position,readings){
  ProbProbs=c()
  for (i in 1:length(probs$salinity[,1])){
    Solidity=dnorm(readings[1], probs$salinity[i,1], probs$salinity[i,2])
    Phosphate=dnorm(readings[2], probs$phosphate[i,1], probs$phosphate[i,2])
    Nitrogen=dnorm(readings[3], probs$nitrogen[i,1], probs$nitrogen[i,2])
    ProbProbs[i]=Solidity*Phosphate*Nitrogen
  }
  if(!is.na(position[1]) && position[1]<0){
    pos=-position[1]
    ProbProbs[pos]=1
  }
  else if(!is.na(position[2]) && position[2]<0){
    pos=-position[2]
    ProbProbs[pos]=1
  }
  return ( ProbProbs )
  
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

updateState = function(state, tMatrix, obs_vector)
{
  new_state = state %*% t(tMatrix)
  for (i in 1:40)
  {
    new_state[[i]] = new_state[[i]] * obs_vector[[i]]
  }
  new_state = new_state / sum(new_state)
  return (new_state)
}

myFunction = function(moveInfo,readings,positions,edges,probs) {
  if (moveInfo$mem$status == 0)
  {
    # New Game
    moveInfo$mem$status = 1
    transResult = generateTransitions(edges)
    moveInfo$mem$matrix = transResult$tMatrix
    moveInfo$mem$state = generateInitialState(positions)
    moveInfo$mem$edgeOptions = transResult$edgeOptions
  }
  trav1 = positions[1]
  trav2 = positions[2]
  dead = F
  deadPos = 0
  state = moveInfo$mem$state
  tMatrix = moveInfo$mem$matrix
  obs_vector = ObservationMatrix(probs, positions, readings)
  if (!is.na(trav1) && trav1 < 0)
  {
    dead = T
    deadPos = trav1
  }
  if (!is.na(trav2) && trav2 < 0)
  {
    dead = T
    deadPos = trav2
  }
  if (dead)
  {
    state = c(rep(0, 40))
    state[deadPos] = 1
  }
  else
  {
    state = updateState(state, tMatrix, obs_vector)
    state = updateState(state, tMatrix, obs_vector)
    state = updateState(state, tMatrix, obs_vector)
  }
  Croc_pos = which.max(state)
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
  print("Croc")
  print(Croc_pos)
  #print("Ranger")
  #print(positions[3])
  #print(obs_vector)
  moveInfo$mem$state = state
  return(moveInfo)
}

