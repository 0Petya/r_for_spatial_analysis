#---------------------------------------------------------------------------
#-
#-  Code from Textbook: Intro to R for Spatial Analysis and Mapping, 2nd Ed.
#-
#-  Chapter 7: Spatial Attribute Analysis with R
#-
#-------------------------------------------------------------------

library( GISTools )
library( tmap )
library( tmaptools )
library( sf )
library( dplyr )
library( ggplot2 )
library( tmap )
library( sp )
library( reshape )   
library( grid )
library( SpatialEpi )

data( pennLC )   ## Pennsylvania lung cancer data
View( pennLC )
class( pennLC )

penn.state.latlong = pennLC$spatial.polygon
class( penn.state.latlong )
penn.state.utm <- set_projection(penn.state.latlong, 3724)  
##  This line doesn't work.  Instead, use code below.
data(georgia)
proj4string( penn.state.latlong ) = proj4string( georgia )

penn.state.utm = penn.state.latlong 

if ( "sf" %in% class(penn.state.utm) ) 
  penn.state.utm <- as(penn.state.utm,"Spatial")

## Map the counties in penn.state.utm
tm_shape( penn.state.utm ) + tm_borders()

class( pennLC )
str( pennLC )
str( pennLC[[1]] )
str( pennLC[[2]] )
str( pennLC$smoking )

penn.state.utm$smk = pennLC$smoking$smoking * 100
##  You have to be 100% sure that the order of counties is the same in
##  penn.state.utm and pennLC.
##  If this is not the case, you'll have to use merge or left_join

# windows( 9 , 6 )
tm_shape( penn.state.utm ) +
  tm_polygons( col="smk" , title="% of Population" )

##  Are neighboring counties smoking percentages independent?
##  Or correlated?   
##  Correlation in space is called autocorrelation.

##  "Shuffle" the smoking percentages across counties

sample( 1:6 )
penn.state.utm$smk_rand1 = sample( penn.state.utm$smk )
penn.state.utm$smk_rand2 = sample( penn.state.utm$smk )
penn.state.utm$smk_rand3 = sample( penn.state.utm$smk )
penn.state.utm$smk_rand4 = sample( penn.state.utm$smk )
penn.state.utm$smk_rand5 = sample( penn.state.utm$smk )

vars = sample( c("smk" , "smk_rand1" , "smk_rand2" , "smk_rand3" ,
                 "smk_rand4" , "smk_rand5" ) )
real.data.i = which( vars == "smk" )

# windows( 9 , 9 )
tm_shape( penn.state.utm ) +
  tm_polygons( col=vars , legend.show=FALSE ) +
  tm_layout(title=1:6 , title.position=c("right","top") )

##  Neighbors

penn.state.utm$county = as.character( pennLC$geo$county )
##  as.character is needed to keep this from being a factor variable

# windows( 12 , 7 )
tm_shape( penn.state.utm) +
  tm_polygons( col="smk" ) +
  tm_text( "county" )

library( spdep )
penn.state.nb = poly2nb( penn.state.utm )
penn.state.nb
summary( penn.state.nb )
str( penn.state.nb )
penn.state.nb[[1]]
penn.state.utm@data$county[[1]]
penn.state.utm@data$county[[21]]
penn.state.utm@data$county[[28]]
penn.state.utm@data$county[[67]]

##  Discuss edge effects
##  Discuss Queen vs Rook adjacency

# Queens
penn.state.net = nb2lines(penn.state.nb,coords=coordinates(penn.state.utm))
proj4string( penn.state.net ) = proj4string( penn.state.utm )
class( penn.state.net )

# windows( 12 , 7 )
tm_shape( penn.state.utm ) + tm_borders( col="gray") + 
  tm_shape( penn.state.net ) + tm_lines( col="red" )

penn.state.nb2 = poly2nb( penn.state.utm , queen=FALSE )
penn.state.net2 = nb2lines(penn.state.nb2,coords=coordinates(penn.state.utm))

# windows( 12 , 7 )
tm_shape( penn.state.utm ) + tm_borders( col="gray") + 
  tm_shape( penn.state.net2 ) + tm_lines( col="red" )

##  Plot both networks together in different colors
# windows( 12 , 7 )
tm_shape( penn.state.utm ) +
  tm_borders( col="gray" ) +
  tm_shape( penn.state.net ) +
    tm_lines( col="blue" , lwd=2 ) +
  tm_shape( penn.state.net2 ) +
    tm_lines( col="orange" , lwd=4 )

##  Lagged means
##  See equation (7.3) on p. 254.  Note error in book.

penn.state.lw = nb2listw( penn.state.nb2 )
class( penn.state.lw )
str( penn.state.lw )
penn.state.lw$neighbours
penn.state.lw$weights

penn.state.utm$smk.lagged.means = lag.listw( penn.state.lw , 
                                             penn.state.utm$smk )
View( penn.state.utm )
# windows( 12 , 12 )
tm_shape( penn.state.utm ) +
  tm_polygons( col=c("smk","smk.lagged.means") , title="% of Population" )

with( data.frame( penn.state.utm) ,
  {
    plot( smk , smk.lagged.means , asp=1 , 
          xlim=range(smk) , ylim=range(smk) )
    abline( a=0 , b=1 ) 
    abline( v=mean(smk) , lty=2 )
    abline( h=mean(smk.lagged.means) , lty=2 )
  })

##  Weighting Matrices
##
##  Read in the MO shape file using the tigris package
##

library( tigris )
MO = counties( "Missouri" )
MO = select( MO , "NAME" , "COUNTYFP" , "geometry" )

MO$countyFIPS = as.numeric( MO$COUNTYFP )

setwd("C:/Users/srigd/Dropbox/MyCourses/BST_5600_Spring2022")
covidDeaths = read.csv( "covid_deaths_usafacts.csv" )
MO_covidDeaths = covidDeaths %>% 
  filter( State == "MO" ) %>%
  filter( countyFIPS > 1 ) %>%
  select( "countyFIPS" , "County.Name" , "X12.31.2020" , "X12.31.2021"  )
MO_covidDeaths$Deaths2020 = MO_covidDeaths$X12.31.2020
MO_covidDeaths$Deaths2021 = MO_covidDeaths$X12.31.2021 - MO_covidDeaths$X12.31.2020

library( stringr )
MO_pop = read.csv( "population.csv" )
head( MO_pop )
MO_pop$Pop = as.numeric( str_replace_all( MO_pop$Pop , "," , "" ) ) 
head( MO_pop )

MO$countyFIPS = MO$countyFIPS + 29000

####  Use the left_join function from the dplyr package.  Note that
####  the "by=" argument gives the county FIPS code
MO1 = left_join( MO , MO_covidDeaths , by = c("countyFIPS") )
MO1 = left_join( MO1 , MO_pop , by=c("countyFIPS") )

MO1$deathRate2020 = MO1$Deaths2020 / MO1$Pop
MO1$deathRate2021 = MO1$Deaths2021 / MO1$Pop

windows( 9 , 9 )
tm_shape( MO1 ) + 
  tm_fill( col="deathRate2020" ) +
  tm_borders()

StL.EMS.region = filter( MO , 
                         COUNTYFP %in% c( "071" , "099" , "113" , "163" ,
                                          "183" , "189" , "219" , "510") )
StL.EMS.region = arrange( StL.EMS.region , COUNTYFP )  ## Do this before
                                                       ## converting to sp   
StL.EMS.region = as( StL.EMS.region , "Spatial" )      


StL.EMS.region.nb = poly2nb( StL.EMS.region )
StL.EMS.region.nb[[2]]
summary( StL.EMS.region.nb )
str( StL.EMS.region.nb )

StL.EMS.region.net = nb2lines( StL.EMS.region.nb ,
                               coords=coordinates(StL.EMS.region) )
proj4string( StL.EMS.region ) = proj4string( georgia )
class( StL.EMS.region )

library( grid )
windows( 12 , 7 )
grid.newpage()

windows( 10 , 7 )
p1 = tm_shape( StL.EMS.region ) + 
  tm_borders( col="gray") + 
  tm_text( "COUNTYFP" ) +
  tm_shape( StL.EMS.region.net ) + 
  tm_lines( col="red" ) 
p2 = tm_shape( StL.EMS.region ) + 
  tm_borders( col="gray") + 
  tm_text( "NAME" ) +
  tm_shape( StL.EMS.region.net ) + 
  tm_lines( col="red" ) 

pushViewport( viewport( layout=grid.layout(1,2) ) )  
print( p1 , vp=viewport(layout.pos.col=1 , height=5 ) )
print( p2 , vp=viewport(layout.pos.col=2 , height=5 ) )

nb2mat( StL.EMS.region.nb )

str(StL.EMS.region.nb)
##  Moran's I
##  See equation (7.4) on p. 257

moran.test( penn.state.utm$smk , penn.state.lw )


