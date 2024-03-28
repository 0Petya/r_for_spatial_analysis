#---------------------------------------------------------------------------
#-
#-  Code from Textbook: Intro to R for Spatial Analysis and Mapping, 2nd Ed.
#-
#-  Chapter 7: Spatial Attribute Analysis with R
#-
#-------------------------------------------------------------------

##  Run code in previous day's lecture
##

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

class( MO1 )

MO1$deathRate2020 = MO1$Deaths2020 / MO1$Pop
MO1$deathRate2021 = MO1$Deaths2021 / MO1$Pop

windows( 9 , 9 )
tm_shape( MO1 ) + 
  tm_fill( col="deathRate2021" ) +
  tm_borders()

StL.EMS.region = filter( MO1 , 
                         COUNTYFP %in% c( "071" , "099" , "113" , "163" ,
                                          "183" , "189" , "219" , "510") )
StL.EMS.region = arrange( StL.EMS.region , COUNTYFP )  ## Do this before
                                                       ## converting to sp   
StL.EMS.region = as( StL.EMS.region , "Spatial" )      
#proj4string( StL.EMS.region ) = proj4string( georgia2 )

StL.EMS.region.nb = poly2nb( StL.EMS.region )
StL.EMS.region.nb[[2]]
summary( StL.EMS.region.nb )
str( StL.EMS.region.nb )

StL.EMS.region.net = nb2lines( StL.EMS.region.nb ,
                               coords=coordinates(StL.EMS.region) )
class( StL.EMS.region )

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


####  Inverse distance weighting

dist.mat = as.matrix( dist( coordinates( StL.EMS.region ) ) )

diag( dist.mat ) = 1   ## Arbitrary number
dist.mat
proj4string( StL.EMS.region )
W = 1/dist.mat
diag( W ) = 0
W


##  Moran's I
##  See equation (7.4) on p. 257

library( spdep )

moran.range = function( lw )
{
  wmat = listw2mat( lw )
  return( range( eigen( (wmat + t(wmat) )/2 ) $values ) )
}
moran.range( penn.state.lw )


moran.test( penn.state.utm$smk , penn.state.lw , randomisation=FALSE )
##  Note: "randomisation" not "randomization"

moran.test( penn.state.utm$smk , penn.state.lw )
##  Note: randomisation is the default

moran.mc( penn.state.utm$smk , penn.state.lw , 10000 )


