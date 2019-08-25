## This module defines methods for optimized caclulating of matrix inverse

#' Wraps given matrix into special object capable of owning it's matrix inverse
#' 
#' @param mtx The initial matrix object. If not provided creates a new empty matrix.
#' @return Wrapper object of \code{mtx} matrix
#' @examples
#' makeCacheMatrix(matrix(1:9, nrow=3, ncol=3))

makeCacheMatrix <- function (mtx = matrix()) {
  cachedInverce <- NULL
  
  set <- function (newMatrix) { 
    mtx <<- newMatrix
    cachedInverce <<- NULL
  }
  get <- function () mtx
  
  getInverse <- function () cachedInverce
  setInverse <- function (inversedMatrix) cachedInverce <<- inversedMatrix
  
  list(set = set, 
       get = get,
       getInverse = getInverse,
       setInverse = setInverse)
}

#' Soolves invers of matrix, wrapped in the object created by makeCacheMatrix.
#' If invese is already cached, returns cached value. Otherwise calculates the inverse and saves the cache.
#' 
#' @param wmtx The wrapper object of a matrix.
#' @return inverse of \code{wmtx$get()} matrix.
#' @examples
#' cacheSolve(makeCacheMatrix(matrix(c(1,0,3,2,2,4,3,2,1), ncol=3)))

cacheSolve <- function(wmtx, ...) {
  inversedMatrix <- wmtx$getInverse()
  if (!is.null(inversedMatrix)) {
    message("Already cached -- return the cached value")
    return(inversedMatrix)
  }
  inversedMatrix <- solve(wmtx$get(), ...)
  wmtx$setInverse(inversedMatrix)
  inversedMatrix
}
