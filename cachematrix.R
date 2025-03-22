## creating a system that stores the inverse of a matrix, so it does not have to recalculate everything (it can be computational)

## it creates a place to store the matrix and inverse, returns a set of function for the matrix to set and get the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # initialising inverse to NULL

        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        get<- function() x # function to get the matrix x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <-function() inv        # here to het the inverse

        list(set=set,get=get, setinverse = setinverse, 
             getinverse=getinverse)
}


## this function checks if the inverse is saved, if not it returns it, otherwise it makes the inverse and saves it

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()

        if (!is.null(inv)) {        # checking if inverse is NULL
                message("cached data!")
                return(inv)
        }
        data_matrix <-x$get()        # calculating inverse value
        inv <- solve(data_matrix, ...)
        x$setinverse(inv)

        return(inv)        # returning the matrix
}
