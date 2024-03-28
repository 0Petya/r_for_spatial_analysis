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
    tm_dots( col="black" , alpha=0.5 , size=0.02 )
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

# windows( 12 , 12 )
tm_shape( blocks , unit="mi" ) +
  tm_borders() + 
  tm_scale_bar( width=0.22 ) +
  tm_compass( position=c(0.8,0.07) ) +
  tm_layout( frame=FALSE , title="New Haven" ,
             title.size=2 , title.position=c(0.55,"top") )

P_OWNEROCC = blocks@data$P_OWNEROCC
model1 = glm( n.breaches ~ P_OWNEROCC , offset=log(area) , family="poisson" )
summary( model1 )


#---------------------------------------------------------------------------
#-
#-  Calculating Distances
#-
#-------------------------------------------------------------------

x = matrix( runif(10) , nrow=5 , ncol=2 )
colnames( x ) = paste0( "Var" , 1:2 )
dist( x )
plot( x , type="n" , xlim=c(0,1) , ylim=c(0,1) )
  for (i in 1:5) text( x[i,1] , x[i,2] , paste(i) )
as.matrix( dist(x) )

x = matrix( rnorm(100) , nrow=5 , ncol=20 )
colnames( x ) = paste0( "Var" , 1:20 )
dist( x )

proj4string( blocks )
coordinates( blocks )   ## coordinates of centroids of blocks (in feet)
as.matrix( dist( coordinates(blocks) ) )  ## distances are in feet

data( georgia )
proj4string( georgia )
proj4string( georgia2 )
as.matrix( dist( coordinates(georgia2) ) )

class( georgia )
class( georgia2 )
gDistance( georgia2[1,] , georgia2[2,] )  ## distance btwn counties 1 & 2
gDistance( georgia[1,] , georgia[2,] )    ## beware!
##  gDistance computes Euclidean distance in plane

georgia2_sf = st_as_sf( georgia2 )
georgia_sf  = st_as_sf( georgia ) 
st_distance( georgia2_sf[1,] , georgia2_sf[2,1] )
st_distance( georgia_sf[1,] , georgia_sf[2,1] )
##  Notice how sp's gDistance failed, but sf's st_distance worked

sp = st_as_sf( SpatialPoints( coordinates( georgia) ) )
## coordinates in sp are in long/lat
st_distance( sp[1,] , sp[2,] )
st_distance( sp[1,] , sp[1:3,] )
distMat = st_distance( sp )
str( distMat )

data( newhaven )
View( places@data )
proj4string( places ) = proj4string( blocks )
centroids = SpatialPoints( coordinates(blocks) , 
                           proj4string=CRS(proj4string(blocks)) )
# windows( 9 , 9 )
tm_shape( blocks ) +
  tm_borders() +
  tm_shape( centroids ) + 
    tm_dots() +
  tm_shape( places ) + 
    tm_dots( size=1.2 , col="red" )
  
distances = gDistance( places , centroids , byid=TRUE )
round(distances, 0)     ## What are the units?
proj4string( places )

distances = ft2miles( gDistance( places , centroids , byid=TRUE ) )
round( distances , 2 )
  

#---------------------------------------------------------------------
#-
#-  Accessibility Exercise
#-  Read p. 165 bottom & p. 166 top
#-
#---------------------------------------------------------------------

#-  Approximate the number of people with and without access (within
#-  one mile) to places

distances = ft2miles( gDistance( places , centroids , byid=TRUE ) )
round(distances, 2)
apply( distances , 1 , min )     ## This is key!
min.dist = as.vector( apply( distances , 1 , min ) )  
min.dist
blocks$access = ( min.dist < 1 )

names( blocks@data )
ethnicity = as.matrix( data.frame( blocks[,14:18] ) / 100 )
ethnicity1 = apply( ethnicity , 2 , function(x) (x*blocks$POP1990) ) # Book
ethnicity2 = ethnicity*blocks$POP1990                # Much simpler!
ethnicity = matrix( as.integer(ethnicity2) , ncol=5 )
ethnicity

colnames( ethnicity ) = c( "White" , "Black" , "Native American" ,
                           "Asian" , "Other" )
mat.access.tab = xtabs( ethnicity ~ blocks$access )
data.set = as.data.frame( mat.access.tab )
colnames( data.set ) = c( "Access" , "Ethnicity" , "Freq" )

dat <- data.frame( NoAccess = data.set[ c(1,3,5,7,9) , 3 ] , 
                   Access  =  data.set[ c(2,4,6,8,10) , 3 ] ,
                   row.names = colnames(ethnicity) )
chisq.test( dat )
##  Book uses Poisson regression, but the chi-square test 
##  is more appropriate

mosaicplot( t(mat.access.tab) , xlab="" , ylab="Access" ,
            main="Mosaic Plot of Access" , shade=TRUE , las=3 , cex=0.8 )





