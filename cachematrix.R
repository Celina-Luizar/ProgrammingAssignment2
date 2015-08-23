# It is a conjunction of two functions.
# The first function creates a special "matrix" object
# that can cache its inverse.
# The second computes the inverse of the "matrix"
# and it returns the inverse of the matrix


# The followinf function creates a invertible "matrix" object

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        
        ## set the value of the invertible "matrix"
        
        set = function (y) {
                ## The operator <<- assigns the value in a different enviroment
                
                x <<- y
                inv <<- NULL
        }
        ## get the value of the "matrix"
        
        get = function() x
        
        ## set the values of the inverse of the "matrix"
        
        setinv = function(inverse) inv <<- inverse
        getinv = function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


# The following function calculates the inverse of the invertible "matrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv = x$getinv()
        
        # verify if the calculated inverse  exist
        if (!is.null(inv)) { 
                message ("getting cached data")
                return(inv)
        }
        # or, calculates the inverse
        data <- x$get()
        inv = solve(data, ...)
        
        # set the values of the inverse of the "matrix"
        
        x$setinv(inv)
        
        ##get the inverse of the "matrix"
        
        return(inv)
}

# Test(m1) matrix1

test = function(matrix){
        ## @mat: an invertible matrix
        
        temp = makeCacheMatrix(matrix)
        
        start.time = Sys.time()
        cacheSolve(temp)
        dur = Sys.time() - start.time
        print(dur)
        
        start.time = Sys.time()
        cacheSolve(temp)
        dur = Sys.time() - start.time
        print(dur)
}



## Example1 with seeds=1111000, nrow=1000, ncol=1000

set.seed(1111100)
r = rnorm(1000000)
m1 = matrix(r, nrow=1000, ncol=1000)
test(m1)
