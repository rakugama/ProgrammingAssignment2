## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## a function to check if the inverse has been calculated
## 1. check if inv has been previously calculated
## 2. if so, return cached data
## 3. if not, get matrix and solve,
## 4. then save the inverse for future use
## 5. and return the inverted matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
