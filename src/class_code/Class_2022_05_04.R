library(gstat)
library( plot3D )
library(deldir)
library(sp)
library(tmap)
library(GISTools)
library(RColorBrewer)

data( fulmar )

fulmar.spdf = SpatialPointsDataFrame( cbind( fulmar$x,fulmar$y) , fulmar )
fulmar.spdf = fulmar.spdf[ fulmar.spdf$year == 1999 , ]
proj4string( fulmar.spdf ) = CRS( "+init=epsg:32631" )

evgm = variogram( fulmar ~ 1 , fulmar.spdf , boundaries=seq(0,250000,l=51))

####  Choices for vgm method: "Exp", "Sph", "Gau", "Mat"
fvgm = fit.variogram( evgm , vgm(3,"Sph",100000,1) )

windows( 9 , 7 )
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



