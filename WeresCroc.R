generateInitialState = function(pos)
{
  initialState = c(rep(1/40, 40))
  return(initialState)
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

updateState = function(state, tMatrix, obs_vector, positions)
{
  new_state = state %*% t(tMatrix)
  new_state_mod = new_state
  if (!is.na(positions[1]))
  {
    if (positions[1] > 0)
    {
      #new_state[positions[1]] = 0
      new_state_mod[positions[1]] = 0
    }
  }
  if (!is.na(positions[2]))
  {
    if (positions[2] > 0)
    {
      #new_state[positions[2]] = 0
      new_state_mod[positions[2]] = 0
    }
  }
  #new_state[positions[3]] = 0
  new_state_mod[positions[3]] = 0
  sum = 0
  sum2 = 0
  for (i in 1:40)
  {
    new_state[[i]] = new_state[[i]] * obs_vector[[i]]
    new_state_mod[[i]] = new_state_mod[[i]] * obs_vector[[i]]
  }
  new_state = new_state / sum(new_state)
  new_state_mod = new_state_mod / sum(new_state_mod)
  return (list(new_state, new_state_mod))
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
  dead = F
  deadPos = 0
  if (!is.na(positions[1]))
  {
    if (positions[1] < 0)
    {
      dead = T
      deadPos = -positions[1]
    }
  }
  if (!is.na(positions[2]))
  {
    if (positions[2] < 0)
    {
      dead = T
      deadPos = -positions[2]
    }
  }
  Croc_pos = deadPos
  move1 = 0
  move2 = 0
  if (!dead)
  {
    initialState = moveInfo$mem$state
    tMatrix = moveInfo$mem$matrix
    obs_vector=ObservationMatrix(probs,positions,  readings)
    new_states = updateState(initialState, tMatrix, obs_vector, positions)
    new_state = new_states[[1]]
    new_state_mod = new_states[[2]]
    new_pos = which.max(new_state_mod)


    move1 = BFS(edges, positions[3], new_pos)
    move1 = move1[2]
    
    new_states = updateState(new_state, tMatrix, obs_vector, positions)
    t1 = which.max(new_states[[1]])
    new_states = updateState(new_states[[1]], tMatrix, obs_vector, positions)
    t2 = which.max(new_states[[1]])
    new_states = updateState(new_states[[1]], tMatrix, obs_vector, positions)
    t3 = which.max(new_states[[1]])
    
    print("---------------")
    print(new_pos)
    print(t1)
    print(t2)
    print(t3)
    print("Best Future Hole")
    #print(Mode(c(new_pos, t1, t2, t3)))
    print("---------------")
    #print(new_state_mod)
    Croc_pos=which.max(new_states[[1]])
    #Croc_pos = best_edge
    
    moveInfo$mem$state = new_states[[1]]
  }
  if (dead)
  {
    new_state = c(rep(0, 40))
    new_state[[Croc_pos]] = 1
    moveInfo$mem$state = new_state
  }
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
  return(moveInfo)
}

