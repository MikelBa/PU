


y <- rep(c(1,2,3),20 ) #could use sample instead if you want this to be random as in docendo's answer

#for the matrix of variables x
#you need a matrix of 50 variables i.e. 50 columns and 60 rows i.e. 60x50 dimensions (=3000 table cells)
x <- matrix( rnorm(3000), ncol=50 )

#bind the 2 - y will be the first column 
mymatrix <- cbind(y,x)