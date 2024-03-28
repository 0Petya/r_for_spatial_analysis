#---------------------------------------------------------------------------
#-
#-  Code from Textbook: Intro to R for Spatial Analysis and Mapping, 2nd Ed.
#-
#-  Chapter 3: Handling Spatial Data in R
#-
#-  Be sure to run code in previous file first
#-
#--------------------------------------------------------------------------
#-------------------------------------------------------------------
#  
#-  Data on earthquakes
#- 
#-------------------------------------------------------------------

library( GISTools )
library( tmap )
library( sf )
data( quakes )    ## from GISTools package
head( quakes )

coords.tmp = cbind( quakes$long , quakes$lat )

quakes.sp = SpatialPointsDataFrame( coords.tmp ,
                                    data = data.frame( quakes ) ,
                                    proj4string = CRS("+proj=longlat " ) )
tm_shape( quakes.sp ) +
  tm_dots( size=0.5 , alpha=0.3 )

quakes_sf = st_as_sf( quakes.sp )
tm_shape( quakes_sf ) +
  tm_dots( size=0.5 , alpha=0.3 )

library( grid )

p1 = tm_shape( quakes_sf ) +
  tm_bubbles( "depth" , scale=1 , shape=19 , alpha=0.3 , 
              title.size="Quake Depths" ) +
  tm_layout(legend.position = c("left", "bottom"))

p2 = tm_shape( quakes_sf ) + 
  tm_dots( "depth" , shape=19, alpha=0.5 , size=0.5 ,
           palette="Blues" ,
           title="Quake Depths" ) +
  tm_layout(legend.position = c("left", "bottom"))

# windows( 12 , 9 )
grid.newpage()
pushViewport( viewport( layout=grid.layout(1,2) ) )
print( p1 , vp=viewport( layout.pos.col=1, height=5 ) )
print( p2 , vp=viewport( layout.pos.col=2, height=5 ) )

##
##  How do you adjust the position of the legend? 
##  See https://cran.r-project.org/web/packages/tmap/tmap.pdf  p. 92
##

p3 = tm_shape( quakes_sf ) +
       tm_bubbles( "depth" , scale=1 , shape=19 , alpha=0.3 , 
                   title.size="Quake Depths" ) +
       tm_legend( outside = TRUE, 
                  outside.position = "bottom", 
                  stack = "horizontal" )

p4 = tm_shape( quakes_sf ) + 
       tm_dots( "depth" , shape=19, alpha=0.5 , size=0.5 ,
                palette="Blues" ,
                title="Quake Depths" ) +
       tm_legend( outside = TRUE, 
                  outside.position = "bottom", 
                  stack = "horizontal" )

# windows( 12 , 9 )
grid.newpage()
pushViewport( viewport( layout=grid.layout(1,2) ) )
print( p3 , vp=viewport( layout.pos.col=1, height=5 ) )
print( p4 , vp=viewport( layout.pos.col=2, height=5 ) )


library( leaflet )

leaflet( quakes_sf ) %>%
  addTiles() %>%
  addCircleMarkers( radius = ~depth/100 )

#-------------------------------------------------------------------
#  
#-  Lines and Attributes
#- 
#-------------------------------------------------------------------

data( newhaven )
class( blocks )
class( roads )
class( breach )

bbox( roads )

xmin = bbox(roads)[1,1]
ymin = bbox(roads)[2,1]
xmax = xmin + diff( bbox(roads)[1,] ) / 2   ## Use western half
ymax = ymin + diff( bbox(roads)[2,] ) / 2   ## Use southern half

xx = as.vector( c(xmin,xmin,xmax,xmax,xmin) )
yy = as.vector( c(ymin,ymax,ymax,ymin,ymin) )

plot( xx , yy ); lines( xx , yy )

crds = cbind( xx , yy )
P1 = Polygon( crds )
class( P1 )
ID = "clip"
P1s = Polygons( list(P1) , ID=ID )
class( P1 )
class( P1s )
SP1s = SpatialPolygons( list(P1s) )
class( SP1s )
df = data.frame( value=1 , row.names=ID )
clip.bb = SpatialPolygonsDataFrame( SP1s , df )
class( clip.bb )

clip_sf = st_as_sf( clip.bb )
roads_sf = st_as_sf( roads )
roads_tmp = st_intersection( st_cast(clip_sf) , roads_sf ) 

# windows( 9 , 9 )
tm_shape( roads ) +
  tm_lines()
  
# windows( 9 , 9 )
tm_shape( roads_tmp ) +
  tm_lines() 

# windows( 9 , 9 )
tm_shape( roads ) +
  tm_lines( col="AV_LEGEND" , lwd=3 , palette="Set2" ) 

# windows( 9 , 9 )
tm_shape( roads_tmp ) +
  tm_lines( col="AV_LEGEND" , lwd=3 , palette="Set2" ) 

# tmaptools::palette_explorer()  ## Try different palettes

#-------------------------------------------------------------------
#  
#-  Histogram in Legend
#- 
#-------------------------------------------------------------------

blocks_sf = st_as_sf( blocks )
# windows( 9 , 9 )
tm_shape( blocks_sf ) +
  tm_polygons( "P_OWNEROCC" , title="Owner Occumpied" ,
               palette="-GnBu" ,  # Notice the minus sign! & below ... 100
               breaks=c(0,round(quantileCuts(blocks$P_OWNEROCC,6),1),100) ,
               legend.hist=TRUE ) +
  tm_scale_bar( width=0.22 ) +
                tm_compass( position=c(0.8,0.07) ) +
  tm_layout( frame=FALSE , title="New Haven" ,
             title.size=2 , title.position=c(0.55,"top") ,
             legend.hist.size=0.5 )

#-------------------------------------------------------------------
#  
#-  Statistical Analysis
#- 
#-------------------------------------------------------------------

data( newhaven )
library( tidyverse )

# windows( 9 , 9 )
hist( blocks$P_VACANT , breaks=40 , col="cyan" , 
      main="Distribution of Percent Vacant across Blocks" ,
      xlab="Percent Vacant" , ylab="Frequency" )

# windows( 9 , 7 )
ggplot( blocks@data , aes(P_VACANT) ) +
  geom_histogram( col="salmon" , fill="cyan" , bins=40 ) +
  xlab( "Percent Vacant" ) +
  ylab( "Frequency" ) +
  labs( title="Distribution of Percent Vacant across Blocks") +
  theme_bw()


library( reshape )   ## needed for melt function below

index = blocks$P_VACANT > 10
blocks$vac = index + 1
blocks$vac = factor( blocks$vac , labels=c("Low","High" ) )

df = melt( blocks@data[ , c("P_OWNEROCC","P_WHITE","P_BLACK","vac") ] )
# windows( 12 , 7 )
ggplot( df , aes( variable , value ) ) +
  geom_boxplot() +
  facet_wrap( ~vac )

p.vac = blocks$P_VACANT / 100
p.ownerocc = blocks$P_OWNEROCC / 100

# windows( 9 , 7 ) 
plot( p.ownerocc , p.vac )

lm( p.vac ~ p.ownerocc )
mod1 = lm( p.vac ~ p.ownerocc )
summary( mod1 )

df1 = data.frame( p.vac , p.ownerocc )
p1 = ggplot( df1 , aes( p.vac , p.ownerocc ) ) +
  #stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth( method='lm' ) +
  geom_point() +
  xlab("Proportion of Vacant Properties") +
  ylab("Proporion Owner Occupied") +
  labs(title = "Regression of Vacant Properties aginst Proportion Owner Occupied")

p2 = ggplot( df1 , aes(p.vac, p.ownerocc) ) +
  geom_smooth(method='loess') +
  geom_point() +
  xlab("Proportion of Vacant Properties") +
  ylab("Proporion Owner Occupied") +
  labs(title = "Regression of Vacant Properties aginst Proportion Owner Occupied")

grid.newpage()
pushViewport(viewport(layout=grid.layout(2,1)))
print(p1, vp=viewport(layout.pos.row = 1, height = 5))
print(p2, vp=viewport(layout.pos.row = 2, height = 5))
