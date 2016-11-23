## The following code calculates the inverse of a matrix (assuming it exists) or gets the inverse if the matrix was not changed and the inverse was already
## calculated.

## This syntax defines two functions: makeCacheMatrix and cacheSolve.



# makeCacheMatrix creates a list containing a function to
# 1. set the value of a matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix using solve()
# 4. get the value of the inverse of the matrix using solve()

makeCacheMatrix <- function(x = matrix()) {   # x is initialized as a function argument
  m <- NULL                                   # m is initialized as an object within the makeCacheMatrix environment to be used later in the function
  set <- function(y) {                        # the input argument is assigned to the x object in the parent environment
    x <<- y
    m <<- NULL                                # the value of NULL is assigned to the m object in the parent environment
  }                                           # so if there is a valid inverse cached in m, whenever x is reset, the value of m is cleared forcing
                                              # subsequent calls of cacheSolve to recalculate the inverse of the object x
  get <- function() x                         # retrieve x from the parent environment
  setsolve <- function(solve) m <<- solve     # assign solve to m in the parent environment
  getsolve <- function() m                    # get the value of m
  list(set = set, get = get,                  # Assign each of the four functions as an element within a list and return it to the parent environment
       setsolve = setsolve,                   # each element in the list is named so we can use the $ operator to retrieve the correct function from the list
       getsolve = getsolve)
}


## cacheSolve calculates the inverse of the matrix that is set as the argument of the makeCacheMatrix function. If the inverse of the matrix already was
## calculated, it uses the stored result of the inverse and displays "getting cached data". cacheSolve requires an input argument of type makeCacheMatrix()
##

cacheSolve <- function(x, ...) {
  m <- x$getsolve()                           # assign the value of getsolve (which gets the value of m) to m
  if(!is.null(m)){                            # check whether m is unequal to NULL. If so, give the message "getting cached data" and return the result m
    message("getting cached data")            # the first time that the function cacheSolve is called m will be NULL, so this if-statement is passed
    return(m)
  }
  data <- x$get()                             # assign the value of get (which gets the inputmatrix) to the object data
  m <- solve(data, ...)                       # assign the inverse of the matrix to m
  x$setsolve(m)                               # assign the result to m in the parent environment
  m                                           # print m
}


# Example of testing the functions:
#makeMatrix <- matrix(1:4, c(2,2))             # make a 2x2-matrix, with values 1 to 4
#makeMatrix1 <- makeCacheMatrix(makeMatrix)    # assign the result of the makeCacheMatrix function over the argument makeMatrix to makeMatrix1
#cacheSolve(makeMatrix1)                       # Calculate or get the stored value of the inverse of the makeMatrix

# Result at first call:
#> cacheSolve(makeMatrix1)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

# Result at second call:
#> cacheSolve(makeMatrix1)
#getting cached data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

# Make new matrix in makeMatrix gives the following results
#> makeMatrix <- matrix(4:1, c(2,2))
#> makeMatrix1 <- makeCacheMatrix(makeMatrix)
#> cacheSolve(makeMatrix1)
#[,1] [,2]
#[1,] -0.5    1
#[2,]  1.5   -2
#> cacheSolve(makeMatrix1)
#getting cached data
#[,1] [,2]
#[1,] -0.5    1
#[2,]  1.5   -2
