#---------------------------------------------------------------------------
#-
#-  Code from Textbook: Intro to R for Spatial Analysis and Mapping, 2nd Ed.
#-
#-  Chapter 5: Using R as a GIS
#-
#-------------------------------------------------------------------

#-------------------------------------------------------------------
#  
#-  Statistical Analysis
#- 
#-------------------------------------------------------------------

library( GISTools )
library( tmap )
library( sf )
library( dplyr )
library( ggplot2 )
library( tmap )
library( sp )
library( reshape )   ## needed for melt function below
##  https://www.statmethods.net/management/reshape.html
library( grid )

data( newhaven )

index = blocks$P_VACANT > 10
blocks$vac = index + 1
blocks$vac = factor( blocks$vac , labels=c("Low","High" ) )

df0 = blocks@data[ , c("P_OWNEROCC","P_WHITE","P_BLACK","vac") ] 
df  = melt( df0  , id="vac" )

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
mod1$coefficients

df1 = data.frame( p.vac , p.ownerocc )
p1 = ggplot( df1 , aes( p.ownerocc , p.vac) ) +
  #stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth( method='lm' ) +
  geom_point() +
  ylab("Proportion of Vacant Properties") +
  xlab("Proporion Owner Occupied") +
  labs(title = "Regression of Vacant Properties aginst Proportion Owner Occupied")

p2 = ggplot( df1 , aes(p.vac, p.ownerocc) ) +
  geom_smooth(method='loess') +
  geom_point() +
  xlab("Proportion of Vacant Properties") +
  ylab("Proporion Owner Occupied") +
  labs(title = "Regression of Vacant Properties aginst Proportion Owner Occupied")

# windows( 9 , 9 )
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,1)))
print(p1, vp=viewport(layout.pos.row = 1, height = 5))
print(p2, vp=viewport(layout.pos.row = 2, height = 5))

#-------------------------------------------------------------------
#  
#-  Chapter 5: Using R as a GIS
#-
#-------------------------------------------------------------------

data( georgia )
class( georgia )

georgia_sf = st_as_sf( georgia )
class( georgia_sf )

georgia_v2 = as( georgia_sf , "Spatial")
class( georgia_v2 )

##  as(,) works for "sf" class also
georgia_v3 = as( georgia , "sf" )
class( georgia_v3 )

##  Tornado data

data( tornados )    ## from GISTools package

us_states_sf = st_as_sf( us_states )
torn_sf = st_as_sf( torn )

# windows( 12 , 8 )
tm_shape( us_states_sf ) +
  tm_polygons( "white" ) +
  tm_shape( torn ) +
    tm_dots( col="black" , size=0.06 , shape=1 , alpha=0.5 ) +
    tm_shape( us_states_sf ) +
    tm_borders( "blue" , lwd=2 ) 

##  torn2  and  us_states2  ---  Different projection

class( torn2 )
class( us_states2 )

slotNames( torn2 )
slotNames( us_states2 )

proj4string( us_states2 )
proj4string( torn2 )

proj4string( us_states )
proj4string( torn )

# windows( 9 , 9 )
par( mfrow=c(2,1))
plot( us_states2 , main="Use this Projection" )
plot( us_states , main="Not this One" )

# windows( 12 , 9 )
plot( us_states2 )
plot( torn2 , add=TRUE )

# windows( 12 , 9 )
tm_shape( us_states2 ) +
  tm_borders() +
  tm_shape( torn2 ) +
    tm_dots()

# windows( 12 , 9 )
tm_shape( torn2 ) +
  tm_dots( col="coral" ) +
  tm_shape( us_states2 ) +
    tm_borders( col="darkblue" )
##  Alaska and Hawaii are included!

torn2_clip = torn2[ us_states2 , ]

# windows( 12 , 9 )
tm_shape( torn2_clip ) +
  tm_dots( col="coral" ) +
  tm_shape( us_states2 ) +
    tm_borders( col="darkblue" )

## Add a few enhancements
##
us_outline = gUnaryUnion( us_states2 )
plot( us_outline )

# windows( 12 , 9 )
tm_shape( torn2_clip ) +
  tm_dots( col="coral" , alpha=0.5 , size=0.1 ) +
  tm_shape( us_states2 ) +
    tm_borders( col="darkred" ) +
    tm_shape( us_outline ) +
      tm_borders( lwd=2 , col="darkred" )

#---------------------------------------------------------------------------
#-
#-  Calculations
#-
#-------------------------------------------------------------------

MO_torn = torn2[ torn2$STATE == "29" , ]
plot( MO_torn )
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

windows( 9 , 7 )
tm_shape( us_states2_sf ) +
  tm_fill( col="counts" )

poly.areas( us_states2 )
st_area( us_states2_sf )

us_states2_sf$AREA_SqK = st_area( us_states2_sf )
##  us_states2_sf  already has an AREA column

plot( us_states2_sf$AREA , us_states2_sf$AREA_SqK )  
us_states2_sf$tornadoes_sqk = us_states2_sf$counts / us_states2_sf$AREA_SqK  
  
windows( 9 , 7 )
tm_shape( us_states2_sf ) +
  tm_fill( col="tornadoes_sqk" )  
  
windows( 9 , 7 )
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




