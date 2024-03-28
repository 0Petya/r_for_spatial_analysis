#----------------------------------------------------------------------------
#-
#-  Code from Textbook: Intro to R for Spatial Analysis and Mapping, 2nd Ed.
#-
#-  Chapter 2: Data and Plots (Part 2)
#-
#----------------------------------------------------------------------------

x1 = rnorm( 100 )
y1 = rnorm( 100 )
plot( x1 , y1 , pch=19 , col="red" )

x2 = seq( from=0 , to=2*pi , length=100 )
y2 = sin( x2 )

# windows( 9 , 7 )
plot( x2 , y2 , type="l" , col="darkgreen" , lwd=2 , 
      xlim=c(0,6.2) , ylim=c(-1.2,1.2) , xlab="x" , ylab="sin(x)")
points( x2 , y2 + rnorm(100,0,0.1) , pch=19 , col="orange" )

par( mfrow=c(1,2) )
plot( x2 , y2 , type="l" , lwd=3 , col="darkorange" )
plot( x2 , y2 , type="p" , lwd=2 , pch=1 )

x3 = seq( from=0 , to=2*pi , length=20 )
y3 = cos( x3 )
# windows( 9 , 7 )
par( mfrow=c(1,2) )
plot( x3 , y3 , type="b" , pch=19 , col="black" )
plot( x3 , y3 , type="h" , col="gray" , lwd=4 )


x2 = seq( from=0 , to=2*pi , length=100 )
y2 = sin( x2 )
y4 = cos( x2 )
# windows( 9 , 7 )
par( mfrow=c(1,2) )
plot( y2 , y4)
  polygon( y2 , y4 , col="coral" )
plot( y2 , y4 , type="n" , asp=1 )
  polygon( y2 , y4 , col="chartreuse" )

##  install.packages( "GISTools" , depend=TRUE )    ## if necessary
library( GISTools )
data( georgia )

str( georgia.polys )
str( georgia )
slotNames(georgia)

str( georgia@data )
str(georgia@data$Name)
which( georgia@data$Name == "Bulloch" )

Bulloch = georgia.polys[[16]]
# windows( 9 , 9 )
plot( Bulloch , asp=1 , type="n" , xlab="Easting" , ylab="Northing" )
polygon( Bulloch , density=14 , angle=45 , col="red" )

# Create an outline of Georgia
georgia.outline <- unionSpatialPolygons(georgia,rep(1,159))

str( georgia@polygons[[16]] )
str( georgia@polygons[[16]]@Polygons[[1]] )
str( georgia@polygons[[16]]@Polygons[[1]]@coords )

# windows( 9 , 9 )
plot( georgia.outline )
  polygon( georgia@polygons[[16]]@Polygons[[1]]@coords , col="gray" )     ## Add Bulloch Co.
  polygon( georgia@polygons[[1]]@Polygons[[1]]@coords )      ## Add Appling Co.
  for ( i in 1:159 ) polygon( georgia@polygons[[i]]@Polygons[[1]]@coords )
  points( -81.7832, 32.4488 , pch=19 , col="red " , cex=1.5 )  ## Add point at Statesboro

##  Colors

# windows( 9 , 9 )
plot( Bulloch , asp=1 , type="n" , xlab="Easting", ylab="Northing" )
  polygon( Bulloch , col=rgb(0,0.5,0.7) )
  points( x=10000*runif(500,130,136) , y = 10000*runif(500,111,117) , 
          pch=16 , col="brown" )
  
# windows( 9 , 9 )
  plot( Bulloch , asp=1 ,type="n" , xlab="Easting", ylab="Northing" )
  polygon( Bulloch , col="#B3B333" )
  text( 1330000 , 1140000 , "Bulloch County" , cex=2 )
  text( 1330000 , 1135000 , "Georgia" , col="#0000FF" )
  
# locator()

# windows( 9 , 9 )
plot( c(-1.5,1.5) , c(-1.5,1.5) , asp=1 , type="n" )
  rect( -0.5 , -0.5 , 0.5 , 0.5 , border=NA , col=rgb(0,0.5,0.5) )
  rect( 0 , 0 , 1, 1 , col=rgb(1,0.5,0.5) )
##
##  Add a fourth argument (transparency) to rgb
  
data( meuse.grid )
str( meuse.grid )
mat = SpatialPixelsDataFrame( points=meuse.grid[c("x","y")] , data=meuse.grid )
str( mat )

# windows( 9 , 7 )
par( mfrow=c(1,2) , mar=c(0,0,0,0) )
image( mat , "dist" )
image( mat , "soil" )
