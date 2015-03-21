## 
##     Assignment: Caching the Inverse of a Matrix
##
#
# The pair of functions below make use of the R lexical scoping rules to cache the inverse of a matrix
#
## Lexical Scoping/use of <<- operator:
# R uses lexical scoping rules. These rules determine how a value is associated with a free variable
# in a function. The values of free variables are searched for in the environment in which the function was
# defined. Typically a function is defined in the global environment (user's workspace). 
# In R, functions can also be defined within other functions. By creating functions within another 
# function we have created an isolated environment in which those functions are defined. The <<- operator
# when used in such a function assigns the value to the variable in that isolated environment in which the
# functions were defined.  

## makeCacheMatrix function:
#
# Takes a matrix as it argument (assumed to be a square matrix) and creates a 'special' matrix that holds the
# cached value of its inverse once calculated (initially set to a NULL) and a list of functions that:
# 1. get the value of the matrix
# 2. get the value of the cached matrix inverse
# 3. set the value of the cached matrix inverse

makeCacheMatrix <- function(x = matrix()){
    m <- x                                  # assign the matrix argument to local variable m
    cacheInvMatrix <- NULL                  # initialise the local variable (inverse matrix) to NULL
    getMatrix <- function() m               # get the value of matrix m
    getInverse <- function() cacheInvMatrix # get value of cached inverse matrix
    setInverse <- function(y){              # set value of the cached inverse matrix
        cacheInvMatrix <<- y                # << - operator used
    }
    list (get = getMatrix, getinv = getInverse, setinv = setInverse) # list of the created function names        
}

## cacheSolve function:
#
# Takes a 'special' matrix created by the makeCacheMatrix function as its argument. It then
# 1. Uses the getinv() function of its argument to assign the current value of the cached matrix inverse
#    to a local variable (cache)
# 2. Outputs the "checking cache..." message to the console
# 3. Checks if the matrix inverse already exists i.e. not a NULL
# 4. If the inverse does exist it outputs the "inverse already cached" message
# 5. If the inverse does not already exist it outputs the "calculating new inverse and caching it" message and;
# 6. Uses the R function solve() to calculate its inverse then;
# 7. Uses the setinv() function of its argument to set the cacheInvMatrix to the inverse value
# 8. It then outputs the "Finished" message and;
# 9. Returns the value of the cached inverse value by using the getinv() function if its argument

cacheSolve <- function(x = matrix()){
    cache <- x$getinv()                     # assign current cached inverse of x to local cache variable
    message("checking cache...")            # output 'checking..' message to the console
    if(!is.null(cache)){                    # check if already cached i.e. not NULL 
        message("inverse already cached")   # if already cached, output message to the console       
    }
    else{                                                 # if not already cached then
        message("calculating new inverse and caching it") # output '..new inverse..' to console
        cache <- solve(x$get())                           # get matrix x, take inverse and assign to local variable
        x$setinv(cache)                                   # set makeCachematrix cacheInvMatrix variable to inverse value
    }    
    message("Finished")                     # output 'Finished' message to console 
    return(x$getinv())                      # get and retun the inverse matrix value    
}




## Additional Information
# To test the operation of these functions they were loaded into the workspace and the following
# inputted/outputted at the console
#
#
#> m <- matrix(c(-1, -2, 1, 1), 2,2)
#> x <- makeCacheMatrix(m)
#> x$get()
#[,1] [,2]
#[1,]   -1    1
#[2,]   -2    1
#> inv <- cacheSolve(x)
#checking cache...
#calculating new inverse and caching it
#Finished
#> inv
#[,1] [,2]
#[1,]    1   -1
#[2,]    2   -1
#> inv <- cacheSolve(x)
#checking cache...
#inverse already cached
#Finished
#> inv
#[,1] [,2]
#[1,]    1   -1
#[2,]    2   -1
#> x$get()%*%inv
#[,1] [,2]
#[1,]    1    0
#[2,]    0    1