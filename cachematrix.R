## Function makeCacheMatrix elaborated by Marco Quintero (markut)

makeCacheMatrix <- function(x = matrix()) {
        k <- NULL
        set <- function(y) {
                x <<- y
                k <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) k <<- solve
        getinverse <- function() k
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function cacheSolve elaborated by Marco Quintero (markut)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        k <- x$getinverse()
        if(!is.null(k)) {
                message("getting cached data")
                return(k)
        }
        data <- x$get()
        k <- solve(data, ...) 
        x$setinverse(k)
        k        
}

###example of run:
#t_matrix<-matrix(c(2,0,-1, -1,2,0, 0,1,2),nrow=3,ncol=3)
#t_matrix

#cacheSolve(makeCacheMatrix(t_matrix))
#           [,1]      [,2]       [,3]
#[1,]  0.4444444 0.2222222 -0.1111111
#[2,] -0.1111111 0.4444444 -0.2222222
#[3,]  0.2222222 0.1111111  0.4444444

#solve(t_matrix)
#           [,1]      [,2]       [,3]
#[1,]  0.4444444 0.2222222 -0.1111111
#[2,] -0.1111111 0.4444444 -0.2222222
#[3,]  0.2222222 0.1111111  0.4444444



