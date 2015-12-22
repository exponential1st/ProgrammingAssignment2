## makeCacheMatrix and cacheSolve are defined to create behavior of caching reverse matrix of a square matrix.
## The value of reverse matrix in only calculated first time and cached for further usage. 

##makeCacheMatrix function will return a list of functions which is used to manipulate an encapsulated
##state with <<- operator.



##makeCacheMatrix takes a square matrix, and returns a List of functions which are used to manipulate the encasulated state 
##(matrix and its cached reverse matrix). 
##functions in the retuned list are:
##getMatrix  get the original matrix passed in as argument
##setMatrix  set the matrix and clear cached reverse matrix if any
##setReverseMatrix set reverse matrix of the original matrix and store its value 
##getReverseMatrix get stored value of the reverse matrix, NULL if not stored yet


makeCacheMatrix <- function(x = matrix()) {
  reverseMatrix<-NULL
  setMatrix<-function(y){
    x<<-y
    reverseMatrix<<-NULL
  }
  getMatrix <-function() x
  setReverseMatrix<-function(reverse) reverseMatrix<<-reverse
  getReverseMatrix<-function() reverseMatrix
  
  list(setMatrix=setMatrix, getMatrix=getMatrix, setReverseMatrix=setReverseMatrix,getReverseMatrix=getReverseMatrix)
  
}


##cacheSolve fucntion will take in the return type defined by makeCacheMatrix(list of functions that have access to the cached value)
##and return its reverse matrix if already calculated, if not, calculate it and cache it. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getReverseMatrix()
  
  if(!is.null(m)){
    message("cached, return cached value")
    return(m)
  }
  message("not cached yet, calculate and store in environment")
  m<-solve(x$getMatrix())
  x$setReverseMatrix(m)
  return(m)
  
}