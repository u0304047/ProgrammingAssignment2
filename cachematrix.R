makeCacheMatrix<-function(x=matrix()){
  m<-solve(x)
  set<-function(y){
    x<<-y
    m<<-solve(x)
  }
  get<-function() x
  setinv<-function(inv) m <<-inv
  getinv<-function() m
  
  list(get=get,set=set,getinv=getinv,setinv=setinv)
  
}


cacheSolve<-function(x,...){
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(m)
  x$setinv(m)
  m
}