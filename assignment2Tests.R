makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function()
                x
        setmean <- function(mean)
                m <<- mean
        getmean <- function()
                m
        list(
                set = set, get = get,
                setmean = setmean,
                getmean = getmean
        )
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}


x <- makeVector(10)

x$setmean(15)

cachemean(x)


c <- rbind(c(1, -1/4), c(-1/4, 1))  

c

c <- solve(c)
c

x <- list(a = matrix(1:4, 2, 2), b = matrix(1:6, 3, 2))
x

source("cachematrix.R")

c
cacheMatrix <- makeCacheMatrix(c)
cacheMatrix

solved <- cacheSolve(cacheMatrix)
solved

cBack <- cacheSolve(makeCacheMatrix(solved))
cBack
