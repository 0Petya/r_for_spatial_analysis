####  Plotting in R

x2 = seq( 0 , 2*pi , length=100 )
y2 = sin( x2 )
y4 = cos( x2 )

windows( 9 , 7 )

plot( y2 , y4 )

plot(y2, y4, asp = 1)

plot( y2 , y4 , type="n" )  
  points( c(-1,-1,1,1) , c(-1,1,-1,1) , pch=19 , col="red" , cex=2 )
  lines( c(-1,-1,1,1,-1) , c(-1,1,1,-1,-1) , col="red" , lwd=2 )
  polygon( y2 , y4 , col="forestgreen" )
  
plot( y2 , y4 , asp=1 , type="n" )
  points( c(-1,-1,1,1) , c(-1,1,-1,1) , pch=19 , col="red" , cex=2 )
  lines( c(-1,-1,1,1,-1) , c(-1,1,1,-1,-1) , col="red" , lwd=2 )
  polygon( y2 , y4 , col="gold1" )

  
####  Plotting Maps
  
# install.packages( "GISTools",depend=TRUE )
library( GISTools )
data( georgia )
appling = georgia.polys[[1]]

str(georgia@data$Name)
which( georgia@data$Name == "Bulloch" )

Bulloch = georgia.polys[[16]]
windows( 9 , 9 )
plot( Bulloch , asp=1 , type="n" , xlab="Easting" , ylab="Northing" )
polygon( Bulloch , density=14 , angle=45 , col="red" )

# Create an outline of Georgia
georgia.outline <- unionSpatialPolygons(georgia,rep(1,159))

str( georgia@polygons[[16]]@Polygons[[1]]@coords )

windows( 9 , 9 )
plot( georgia.outline )
polygon( georgia@polygons[[16]]@Polygons[[1]]@coords )     ## Add Bulloch Co.
points( -81.7832, 32.4488 , pch=19 , col="red " , cex=2 )  ## Add point at Statesboro


