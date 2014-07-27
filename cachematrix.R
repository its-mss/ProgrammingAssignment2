#This function gets and sets a matrix per parameters passed
#Usage makeCacheMatix(<Matrix passed as paremeter here>)
makeCacheMatrix <- function(x = matrix()) {
     invMatrix <-NULL
     
     #Set matrix function
     set<-function(y){
          x <<-y
          invMatrix <<-NULL
     }
     
     #get matrix function
     get<-function() x
     
     #set inverse matrix function
     setInverseMatrix<-function(inverse) invMatrix <<- inverse
     
     #get Inverse matix function
     getInverseMatrix<-function() invMatrix
     
     list(set=set
          , get=get
          , setInverseMatrix=setInverseMatrix
          , getInverseMatrix=getInverseMatrix
     )
}

#Cachesolve funtion checks if matrix passed has been inverted in past
#If yes cached results are returned
#Else matrix is inverted and returned
#This function assumes that matrix is invertible
#Usage cacheSolve(<Invertible matrix to be inverted here>)
cacheSolve <- function(x, ...) {
     invMatrix <-x$getInverseMatrix()
     #return from cache if already inverted
     if(!is.null(invMatrix)){
          message("getting cached data")
          return(invMatrix)
     }
     data <-x$get()
     #use solve method to get inverse
     invMatrix <-solve(data, ...)
     x$setInverseMatrix(invMatrix)
     invMatrix
}

#Test results
#> x = rbind(c(1, 2), c(3, 4))
#> m = makeCacheMatrix(x)
#> m$get()
#[,1] [,2]
#[1,]    1    2
#[2,]    3    4
#First run - no caching
#> cacheSolve(m)
#[,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5
#Secind run - cached result returned
#> cacheSolve(m)
#getting cached data
#[,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5
