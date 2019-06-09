### A pair of interlinked functions, that allow storing matrices and their inversed matrices

# This function creates a special matrix and provides a cache
makeCacheMatrix <- function(x = matrix())
{
      inversed <- NULL
      set <- function(y)
      {
            x <<- y
            inversed <<- NULL
      }
      get <- function() x
      set_inversed <- function(solve_matrix) inversed <<- solve_matrix
      get_inversed <- function() inversed
      list(set=set, get=get, set_inversed=set_inversed, get_inversed=get_inversed)
}


# This function generates the inverse of the matrix provided by the function above
cacheSolve <- function(x, ...)
{
      inversed <- x$get_inversed()
      if(!is.null(inversed)){
            message("getting cached data")
            return(inversed)
      }
      data <- x$get()
      inversed <- solve(data)
      x$set_inversed(inversed)
      inversed     
}
