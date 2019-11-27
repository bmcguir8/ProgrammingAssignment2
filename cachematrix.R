
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL         ##initializes m as an object
  set <- function(y) {         ##allows function to swap out values after initial run
    x <<- y         ##make easier to read, no duplicate x; assigns y in this environment to x 
          ##in the parent environment (NOT global, "makeCacheMatrix" env.)
          ## the value of y in this env is now the value of x in parent
    s <<- NULL        ##clears any value that may already be assigned to s from previous runs
          ##of this code
  }
  get <- function() x        ##x not set in function, just returns matrix x from parent env.
  setsolve <- function(solve) s <<- solve        ##sets solve in this env to be s in parent env
         ##basically function(s); the value of s in this env is now the value of s in parent env.
  getsolve <- function() s         ##no function, returns s from parent env (solve from setsolve env)
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)        ##names each function,
         ##allows the functions to be called by name later, needs to be list to do this?
}

## the functions within cacheSolve work because the output of makeCacheMatrix has access to
## the functions within makeCacheMatrix due to lexical scoping!! explains why the arguments 
## of cacheSolve needs to be an output of makeCacheMatrix


cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
