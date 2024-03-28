library( MASS )    #### Needed for multivariate normal simulation
####
####   S I M U L A T E   A   R A N D O M    F U N C T I O N
####

RandomFunction = function( n , muVec , sigma2 )
{
  sigmaMat = diag( rep( sigma2 , n ) )
  b = 1
  x = sort( runif(n) )
  #### Correlation between two points is exp(-d/b) where d is distance 
  #### between the points.  This is the trickiest part.
  for ( i in 1:(n-1) ) 
  {
    for (j in (i+1):n)
    {
      d = abs( x[i] - x[j] )
      sigmaMat[i,j] = sigma2*exp(-d/b)
      sigmaMat[j,i] = sigmaMat[i,j]
    }
  }  
  #### These loops don't change the diagonal (variance) elements of sigmaMat
  z = mvrnorm( 1 , muVec , sigmaMat )
  return( data.frame(x,z) )
}

n = 200                 ####  N >= 1000 is slow
muVec = rep( 0 , n )   ####  Outcome mean is 0
sigma2 = 9             ####  Outcome variance is 9; s.d. is 3

xz = RandomFunction( n , muVec , sigma2 )
x = xz$x
z = xz$z
# windows( 12 , 7 )
plotchar = "o"
plot( x , z , type="p" , pch=plotchar , xlim=c(0,1) , ylim=c(-10,10) )

for (k in 1:10) 
{
  xz = RandomFunction( n , muVec , sigma2 )
  x = xz$x
  z = xz$z
  lines( x , z , col=rgb(0.33,0.33,0.34,0.50) )
}
