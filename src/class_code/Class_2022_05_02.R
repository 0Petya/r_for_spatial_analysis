# setwd("C:/Users/srigd/Dropbox/MyCourses/BST_5600_Spring2022")

library( plot3D )
library(deldir)
library(sp)
library(gstat)
library(tmap)
library(GISTools)
library(RColorBrewer)

PM2p5 = read.csv("./data/raw/class/PM2p5.csv")
print( PM2p5 )
x = PM2p5$x
y = PM2p5$y
z = PM2p5$z
n = length( x )

# windows(9,9)
plot( x , y , cex=z/2 , pch=19 , col="red" , asp=1 )

# windows(9,9)
plot( x , y , pch=19 , col="red" , asp=1 , 
      cex=1.5 , xlim=c(0,1) , ylim=c(0,1) )
# windows(9,7)
scatter3D( x , y , z , xlim=c(0,1) , ylim=c(0,1) , zlim=c(0,15) )
for (i in 1:n) lines3D(c(x[i],x[i]),c(y[i],y[i]),c(0,z[i]),add=TRUE)

###############################################
####  
####  Method 1: Nearest Neighbor Interpolation
####
###############################################

#########################################################
#
# Original code from Carson Farmer
# http://www.carsonfarmer.com/2009/09/voronoi-polygons-with-r/
# Subject to minor stylistic modifications
#
#########################################################
####  SR - It's not necessary to understand the details
####  of this code.  It's just a way to create Voronoi
####  diagrams.
#########################################################

# Modified Carson Farmer code
voronoipolygons = function(layer) {
  crds <- layer@coords
  z <- deldir(crds[,1], crds[,2])
  w <- tile.list(z)
  polys <- vector(mode='list', length=length(w))
  for (i in seq(along=polys)) {
    pcrds <- cbind(w[[i]]$x, w[[i]]$y)
    pcrds <- rbind(pcrds, pcrds[1,])
    polys[[i]] <- Polygons(list(Polygon(pcrds)),
                           ID=as.character(i))
  }
  SP <- SpatialPolygons(polys)
  voronoi <- SpatialPolygonsDataFrame(SP, 
                                      data=data.frame(x=crds[,1], 
                                                      y=crds[,2], 
                                                      layer@data,
                                                      row.names=sapply(slot(SP, 'polygons'), 
                                                                       function(x) slot(x, 'ID'))))
  proj4string(voronoi) <- CRS(proj4string(layer))
  return(voronoi)
}  

boundbox = matrix( c(0,0,1,1) , nrow=2 ,ncol=2 )
rownames(boundbox) = c( "x" , "y" )
colnames(boundbox) = c( "min" , "max" )
myData.spdf = SpatialPointsDataFrame( cbind(x,y) , data.frame(z) ,
                            bbox=boundbox )
proj4string(myData.spdf) = CRS( "+init=epsg:32631" )
myData.voro = voronoipolygons( myData.spdf )

myData.pt = tm_shape( myData.spdf ) +
  tm_dots( size=0.2 )
myData.vr = tm_shape( myData.voro ) +
  tm_borders( col="blue" ) + 
  tm_dots( size=0.1 , col="red" )
# windows(9,9)
tmap_arrange( myData.vr )
tmap_arrange( myData.pt,myData.vr )

# windows(9,9)
shades = shading( breaks=seq(2,10,2) , cols=brewer.pal(6,"Reds"))
tm_shape( myData.voro ) +
  tm_fill( col='z' , style="fixed" , 
           breaks=c(0,2,4,6,8,10,12) , alpha=0.8 ,
           title="PM2.5 Levels" ) +
  tm_borders( col="black" ) +
  tm_dots( size=0.1 , col="black" )

###############################################
####
####  Data described in book
####
###############################################

data(fulmar)

fulmar.spdf <- SpatialPointsDataFrame(cbind(fulmar$x,fulmar$y),
                                      fulmar)
fulmar.spdf <- fulmar.spdf[fulmar.spdf$year==1999,]
proj4string(fulmar.spdf) <- CRS("+init=epsg:32631")
fulmar.voro <- voronoipolygons(fulmar.spdf)

fpt <- tm_shape(fulmar.spdf) + tm_dots(size=0.1)
fvr <- tm_shape(fulmar.voro) + tm_borders() + tm_dots(size=0.05)
# windows( 12 , 12 )
tmap_arrange(fpt,fvr)
  
# windows(9,9)
tm_shape( fulmar.voro ) +
  tm_fill( col="fulmar" , style="fixed" , 
           breaks=c(0,10,20,30,40,50) , alpha=0.8 ,
           title="Fulmar" ) +
  tm_borders( col="gray" ) +
  tm_dots( size=0.02 , col="black" )

###############################################
####  
####  Method 2: Iterative Distance Weighting
####
###############################################
library( maptools )
library( GISTools )
library( gstat )

s.grid = spsample( fulmar.voro , type="regular" , n=1000 )
## Note n is the *approximate* number of grid points
# windows(9,9)
plot(s.grid)

my.dist = function( x0 , x1 ) 
{ 
  return( sqrt( (x0[1]-x1[1])^2 + (x0[2]-x1[2])^2 ) )
}

my.idw = function( fulmar , s.grid , alpha )
{
  n = length( fulmar$x )
  m = nrow( s.grid@coords )
  w = matrix( -1 , nrow=n , ncol=m )
  z = fulmar$fulmar
  for ( j in 1:m )
  {
    for ( i in 1:n)
    {
      x = c( s.grid@coords[j,1] , s.grid@coords[j,2] )  ## grid point
      y = c( fulmar$x[i] , fulmar$y[i] )
      w[i,j] = my.dist( x , y )^(-alpha)  
    }
  }
  zhat = rep( NA , m )
  for ( j in 1:m )
  {
    zhat[j] = sum( w[,j]*z ) 
  }
  return( zhat )
}

alpha = 1
idw.est = my.idw( fulmar , s.grid , alpha )
str( idw.est )

xy = s.grid@coords
zhat = idw.est

grid.sp = SpatialPointsDataFrame( xy , data.frame( zhat=zhat ) )

x = xy[,1]
y = xy[,2]
z = zhat
# windows( 12 , 12 )
scatter3D( x , y , z )

library(rgl)
plot3d(x, y, z, colvar = z, colkey = TRUE )


x.unique = unique( x )
y.unique = unique( y )
library( reshape )
z.reshape = matrix( z , nrow=length(x.unique) , ncol=length(y.unique) )

# windows( 12 , 12 )
persp3D( x.unique , y.unique , z.reshape )
contour(x.unique, y.unique, z.reshape)

library(lattice)
levelplot(z.reshape, col.regions = terrain.colors(100))

grid.rast = SpatialPixelsDataFrame(grid.sp, grid.sp@data)

tm_shape(grid.rast) +
  tm_raster(col = "zhat", title = "Fulmar Density", palette = "Greens") +
tm_layout(legend.bg.color = "white", legend.frame = T)

#####################################################################
####
####  Code from book, but idw.est doesn't work
####
#####################################################################


alpha = 1
idw.est = gstat::idw( fulmar ~ 1 , fulmar.spdf , newdata=s.grid , idp=alpha )
## idp is the inter-population distance parameter, which is what 
## we called alpha.

#tmap_mode("view")  ## Must be connected to the internet
tm_shape( idw.est) +
  tm_dots(col="var1.pred" , border.col=NA , alpha=0.7 )

tm_shape( idw.est) +
  tm_dots(col="var1.pred" , border.col="gray" , alpha=0.7 )

idw.grid = SpatialPixelsDataFrame( idw.est , data.frame(idw.est) )
tm_shape( idw.grid ) +
  tm_raster( col="var1.pred" , title="Fulmar Data Set")

idw.est1 = gstat::idw( fulmar ~ 1 , fulmar.spdf , newdata=s.grid , idp=1 )
idw.est2 = gstat::idw( fulmar ~ 1 , fulmar.spdf , newdata=s.grid , idp=2 )
idw.grid1 = SpatialPixelsDataFrame( idw.est1 , data.frame(idw.est) )
idw.grid2 = SpatialPixelsDataFrame( idw.est2 , data.frame(idw.est) )

ux = unique( coordinates( idw.est1)[,1] )
uy = unique( coordinates( idw.est1)[,2] )
####  ux and uy would be the same for idw.est2 b/c both are based 
####  on the same grid.

print( c( length(ux) , length(uy) ) )
length(ux) * length(uy)
####  Note the grid is 59 by 101, a total of 5959 grid points
####  This is close to, but not exactly equal to, the desired 6000

predmat1 = matrix( idw.est1$var1.pred , length(ux) , length(uy) )
predmat2 = matrix( idw.est2$var1.pred , length(ux) , length(uy) )
str(predmat1)
windows( 12 , 6 )
par( mfrow=c(1,2) , mar=c(0.1,0.1,1.8,0) )
persp( predmat1 , box=FALSE , main="IDW with alpha=1" )
persp( predmat2 , box=FALSE , main="IDW with alpha=2" )


