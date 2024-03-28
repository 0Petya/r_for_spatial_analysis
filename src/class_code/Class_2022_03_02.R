#---------------------------------------------------------------------------
#-
#-  Code from Textbook: Intro to R for Spatial Analysis and Mapping, 2nd Ed.
#-
#-  Chapter 5: Using R as a GIS
#-
#-------------------------------------------------------------------

#---------------------------------------------------------------------------
#-
#-  Calculations
#-
#-------------------------------------------------------------------

library( GISTools )
library( tmap )
library( sf )
library( dplyr )
library( ggplot2 )
library( tmap )
library( sp )
library( reshape )   
library( grid )

data( tornados )
class( torn2 )
class( us_states2 )

slotNames( torn2 )
slotNames( us_states2 )

proj4string( us_states2 )
proj4string( torn2 )

proj4string( us_states )
proj4string( torn )

MO_torn = torn2[ torn2$STATE == "29" , ]
MO = us_states2[ us_states2@data$STATE_NAME == "Missouri" , ] 

tm_shape( MO_torn ) +
  tm_dots( col="coral" , alpha=0.5 , size=0.2 ) +
  tm_shape( MO ) +
    tm_borders( col="darkred" , lwd=2 )
 
poly.counts( torn2 , us_states2 )
us_states2@data$counts = poly.counts( torn2 , us_states2 )
head( us_states2@data )

tm_shape( us_states2 ) +
  tm_fill( col="counts" )

class( us_states2@data$STATE_NAME )

us_states2@data$STATE_NAME = as.character( us_states2@data$STATE_NAME )
cbind( us_states2@data$STATE_NAME , us_states2@data$counts )
str( us_states2@data$counts )

us_states2_sf = st_as_sf( us_states2 )
proj4string( us_states2 )
st_crs( us_states2_sf )
proj4string( us_states2 )

# windows( 9 , 7 )
tm_shape( us_states2_sf ) +
  tm_fill( col="counts" )

poly.areas( us_states2 )
st_area( us_states2_sf )

us_states2_sf$AREA_SqK = st_area( us_states2_sf )
##  us_states2_sf  already has an AREA column

plot( us_states2_sf$AREA , us_states2_sf$AREA_SqK )  
us_states2_sf$tornadoes_sqk = us_states2_sf$counts / us_states2_sf$AREA_SqK  
  
# windows( 9 , 7 )
tm_shape( us_states2_sf ) +
  tm_fill( col="tornadoes_sqk" )  
  
# windows( 9 , 7 )
tm_shape( us_states2_sf ) +
  tm_fill( col="tornadoes_sqk" ) +
  tm_borders() +
  tm_shape( torn2 ) +
    tm_dots( col="black" , alpha=0.5 , size=0.1 )
## fix size

#---------------------------------------------------------------------------
#-
#-  New Haven
#-
#-------------------------------------------------------------------

data( newhaven )

proj4string( blocks )
## Note +units=us-ft

gArea( blocks )
gArea( blocks , byid=TRUE )

##  1 foot = 0.3048 meters
##  1 km = 1000 meters

blocks@data$area_sqk = gArea( blocks , byid=TRUE ) * 0.3048^2 / 1000^2

##  Book's solution
ft2miles( ft2miles( gArea(blocks , byid=TRUE ) ) ) * 2.58999

blocks$densities = poly.counts( breach , blocks ) / 
                     ( poly.areas( blocks ) / 5280^2 )
   
ggplot( blocks@data , aes( P_OWNEROCC , densities ) ) +
  geom_point() +
  geom_smooth( method="lm" )


##  breaches ~ Poisson( AREA*exp( a + b*P_OWNEROCC ) )

n.breaches = poly.counts( breach , blocks )
area = poly.areas( blocks ) / 5280^2
sum( area )

tm_shape( blocks , unit="mi" ) +
  tm_borders() + 
  tm_scale_bar( width=0.22 ) +
  tm_compass( position=c(0.8,0.07) ) +
  tm_layout( frame=FALSE , title="New Haven" ,
             title.size=2 , title.position=c(0.55,"top") )

P_OWNEROCC = blocks@data$P_OWNEROCC
model1 = glm( n.breaches ~ P_OWNEROCC , offset=log(area) , family="poisson" )
summary( model1 )