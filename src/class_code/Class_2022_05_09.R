library(gstat)
library( plot3D )
library(deldir)
library(sp)
library(tmap)
library(GISTools)
library(RColorBrewer)

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

data( fulmar )

fulmar.spdf = SpatialPointsDataFrame( cbind( fulmar$x,fulmar$y ) , fulmar )
fulmar.spdf = fulmar.spdf[ fulmar.spdf$year == 1999 , ]
proj4string( fulmar.spdf ) = CRS( "+init=epsg:32631" )
fulmar.voro <- voronoipolygons(fulmar.spdf)

evgm = variogram( fulmar ~ 1 , fulmar.spdf , boundaries=seq(0,250000,l=51))

s.grid = spsample( fulmar.voro , type="regular" , n=4000 )
####  Choices for vgm method: "Exp", "Sph", "Gau", "Mat"
fvgm = fit.variogram( evgm , vgm("Sph",100000,nugget=5,psill=12) )

# windows( 9 , 9 )
plot( evgm , model=fvgm )

proj4string( s.grid ) = CRS( "+init=epsg:32631" )
krig.est = krige( fulmar ~ 1 , fulmar.spdf,newdata=s.grid , model=fvgm )

krig.grid = SpatialPixelsDataFrame( krig.est , krig.est@data )
View( krig.grid@data )

levs = c(0,2,4,6,8,Inf)
krig.map.est = tm_shape( krig.grid ) + 
  tm_raster( col='var1.pred' , breaks=levs , title='Fulmar Density' ,
             palette='Reds' ) + 
  tm_layout( legend.bg.color='white' , legend.frame = TRUE )

var.levs = c(0,3,6,9,12,Inf) 
krig.map.var = tm_shape(krig.grid) + 
  tm_raster( col='var1.var' , breaks=var.levs,title='Estimate Variance' ,
             palette='Reds' ) + 
  tm_layout( legend.bg.color='white' , legend.frame = TRUE )

tmap_arrange( krig.map.est , krig.map.var )





data(meuse)
meuse.spdf = SpatialPointsDataFrame( cbind( meuse$x,meuse$y ) , meuse )
proj4string( meuse.spdf ) = CRS( "+init=epsg:32631" )

s.grid = spsample( meuse.spdf , type="regular" , n=4000 )
meuse.vgm = variogram( meuse$cadmium ~ 1 , meuse.spdf )
plot( meuse.vgm )

####  Choices for vgm method: "Exp", "Sph", "Gau", "Mat"
meuse.fit.vgm = fit.variogram( meuse.vgm , vgm("Exp",nugget=3) )

# windows( 9 , 7 )
plot( meuse.vgm , model=meuse.fit.vgm )

proj4string( s.grid ) = CRS( "+init=epsg:32631" )
krig.est = krige( cadmium ~ 1 , meuse.spdf , 
                  newdata=s.grid , model=meuse.fit.vgm )

krig.grid = SpatialPixelsDataFrame( krig.est , krig.est@data )
# View( krig.grid@data )

min.cadmium = min( meuse$cadmium )
max.cadmium = max( meuse$cadmium )
levs = c(0,5,10,15,20,Inf)
krig.map.est = tm_shape( krig.grid ) + 
  tm_raster( col="var1.pred" , breaks=levs , title="Cadmium" ,
             palette="Purples" ) + 
  tm_layout( legend.bg.color='white' , legend.frame = TRUE )

var.levs = c(0,3,6,9,12,Inf) 
krig.map.var = tm_shape(krig.grid) + 
  tm_raster( col="var1.var" , breaks=var.levs,title="Estimated Variance" ,
             palette="Purples" ) + 
  tm_layout( legend.bg.color='white' , legend.frame = TRUE )

tmap_arrange( krig.map.est , krig.map.var )


plot( meuse$x , meuse$y , cex=meuse$cadmium/20)
plot( meuse$x , meuse$y , cex=meuse$copper/20)
