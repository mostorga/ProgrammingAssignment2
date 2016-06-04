##This functions creates the matrix that will be used


makeCacheMatrix <-function(x=matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL}
  get<-function()x
  setmatrix<-function(solve) m<<-solve
  getmatrix<-function()m
  list(set=set,get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)}


##This function will cache the inverse of the Matrix

cachesolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat.data <- x$get()
  m <- solve(mat.data, ...)
  x$setmatrix(m)
  return(m)
}