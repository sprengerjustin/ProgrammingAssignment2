## These functions cache the inverse of a matrix and retrieve the cached inverse
## if it has already been solved to increase performance during multiple calculations

## This function takes a matrix and caches its inverse if need be

makeCacheMatrix <- function(x = matrix()) {
  cm <- NULL
  
  get <- function() {x}
  
  setSolve <- function() {cm <<- solve(x)}
  
  getSolve <- function() {cm}
  
  list(get = get, setSolve = setSolve, getSolve = getSolve)
}

## This function solves the matrix or retrieves the cached inverse if it has already
## been calculated

cacheSolve <- function(fn, ...) {
  cm <- fn$getSolve()
  
  if(!is.null(cm)) {
    message("retrieving cached matrix")
    return(cm)
  }
  
  mat <- fn$get()
  
  cm <- solve(mat, ...)
  
  fn$setSolve()
  
  cm
}

