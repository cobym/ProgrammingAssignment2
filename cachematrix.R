
## makeCacheMatrix is a function for handling matricies as well as caching their inverse.
### Returns the handle for matrix m

### Creating a new matrix:              m <- makeCacheMatrix(newMatrix)
###     Where newmatrix is an argument of class 'matrix'
### Get matrix value:                   m$get()
### Set new value to existing matrix:   m$set(newMatrixValue)
### Get cached matrix inverse:          m$getinv()
###     If inverse not chached, will return NULL

makeCacheMatrix <- function(m = matrix()) {
        inv <- NULL             #initiate to null
        set <- function(y) {
                m <<- y         #set new value to matrix
                inv <<- NULL    #null prior calculated inverse 
        }
        get <- function() m             #get matrix
        setinv <- function(inverse) inv <<- inverse     #set inverse - should be done only via 'cacheSolve' function
        getinv <- function() inv        #get cached inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve is a function for calculating and managing the caheing of matrix inverse
## Returns the inverse of the matrix m

### The argument for this function is a matrix m, that has previously created with the makeCacheMatrix function
### Calculate the inverse:      m_inv <- cacheSolve(m)
###     If the inverse has already been calculated and cached, a message will be printed: "getting cached data"

cacheSolve <- function(m) {
        inv <- m$getinv()
        if(!is.null(inv)) {     
                message("getting cached data")
                return(inv)     #if inverse already calculater, get cached result
        }
        data <- m$get()         # get the matrix
        inv <- solve(data)      #calculate the inverse of m
        m$setinv(inv)           #cache the inverse
        inv
}
