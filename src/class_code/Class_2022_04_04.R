#---------------------------------------------------------------------
#-
#-  Code from Textbook: Intro to R for Spatial Analysis & Mapping, 2e
#-
#-  Chapter 7: Spatial Attribute Analysis with R
#-
#---------------------------------------------------------------------
#-
#-  Review Lines 1 - 200
#-
#---------------------------------------------------------------------

library( GISTools )
library( tmap )
library( tmaptools )
library( sf )
library( dplyr )
library( ggplot2 )
library( tmap )
library( sp )
library(spdep)
library( reshape )   
library( grid )
library( SpatialEpi )

data( pennLC )   ## Pennsylvania lung cancer data
class( pennLC )

penn.state.latlong = pennLC$spatial.polygon
class( penn.state.latlong )
data( georgia )
proj4string( penn.state.latlong ) = proj4string( georgia )

penn.state.utm = penn.state.latlong 

if ( "sf" %in% class(penn.state.utm) ) 
  penn.state.utm <- as(penn.state.utm,"Spatial")

## Map the counties in penn.state.utm
tm_shape( penn.state.utm ) + tm_borders()

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

# Queens
penn.state.nb = poly2nb( penn.state.utm )
penn.state.net = nb2lines(penn.state.nb,coords=coordinates(penn.state.utm))
proj4string( penn.state.net ) = proj4string( penn.state.utm )
class( penn.state.net )

penn.state.nb2 = poly2nb( penn.state.utm , queen=FALSE )
penn.state.net2 = nb2lines(penn.state.nb2,coords=coordinates(penn.state.utm))

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
# View( penn.state.utm )
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

covidDeaths = read.csv( "./data/raw/class/covid_deaths_usafacts.csv" )
MO_covidDeaths = covidDeaths %>% 
  filter( State == "MO" ) %>%
  filter( countyFIPS > 1 ) %>%
  select( "countyFIPS" , "County.Name" , "X12.31.2020" , "X12.31.2021"  )
MO_covidDeaths$Deaths2020 = MO_covidDeaths$X12.31.2020
MO_covidDeaths$Deaths2021 = MO_covidDeaths$X12.31.2021 - MO_covidDeaths$X12.31.2020

library( stringr )
MO_pop = read.csv( "./data/raw/class/population.csv" )
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

# windows( 9 , 9 )
tm_shape( MO1 ) + 
  tm_fill( col="deathRate2021" ) +
  tm_borders()

####  Note change here ... MO1, not MO
StL.EMS.region = filter( MO1 ,    
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
proj4string( StL.EMS.region ) = CRS( proj4string( georgia ) )
class( StL.EMS.region )

library( grid )
# windows( 12 , 7 )
grid.newpage()

# windows( 10 , 7 )
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

tm_shape( StL.EMS.region ) +
  tm_borders() +
  tm_polygons( col="deathRate2021" )

####  Inverse distance weighting

dist.mat = as.matrix( dist( coordinates( StL.EMS.region ) ) )

diag( dist.mat ) = 1   ## Arbitrary number, but not 0
round( dist.mat , 3 )
proj4string( StL.EMS.region )
W = 1/dist.mat
diag( W ) = 0
round( W , 3 )

W1 = W/apply(W,1,sum)  ## normalized so that rows sum to 1

##  Moran's I
##  See equation (7.4) on p. 257 & PDF Notes

library( spdep )

range( eigen( (W1 + t(W1) )/2 ) $values )

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

moran.mc.output <- moran.mc( penn.state.utm$smk , penn.state.lw , 100000 )

hist(moran.mc.output$res, xlim = moran.range(penn.state.lw))
abline(v = moran.mc.output$statistic)


moran.test( StL.EMS.region$deathRate2021 , 
            nb2listw(StL.EMS.region.nb) , 
            10000 )

#------------------------------------------------------------------
#-  
#-  moran.test  and  moran.mc  require a neighborhood list
#-
#-  If you want to apply a general weight matrix  W , you need to
#-  use the "ape" package and the Moran.I() function
#-
#------------------------------------------------------------------

library( ape )

Moran.I( StL.EMS.region$deathRate2021 , W1 )

Moran.I( penn.state.utm$smk , listw2mat( penn.state.lw ) )

moran.test( penn.state.utm$smk , penn.state.lw )

dist.mat.penn = as.matrix( dist( coordinates( penn.state.utm ) ) )
round( dist.mat.penn , 3 )

diag( dist.mat.penn ) = 1   ## Arbitrary number, but not 0
round( dist.mat.penn , 3 )

W.penn = 1/dist.mat.penn
diag( W.penn ) = 0
round( W.penn , 3 )

W1.penn = W.penn/apply( W.penn , 1 , sum )  ## normalized so that rows sum to 1

Moran.I( penn.state.utm$smk , W1.penn )
Moran.I( penn.state.utm$smk , listw2mat( penn.state.lw ) )

#--------------------------------------------------------------------
#-
#-  Compute Moran's I and give choropleth map for shuffled smoking
#-  rates.
#-
#--------------------------------------------------------------------

penn.state.utm$smk_rand = sample( penn.state.utm$smk )
moran = moran.test( penn.state.utm$smk_rand , penn.state.lw )$estimate[1]
plot1 = tm_shape( penn.state.utm ) +
  tm_polygons( col="smk_rand" , legend.show=FALSE ) +
  tm_layout( paste0( "Moran's I = " , round(moran,3) ) ,
             title.position=c("right","top") )

penn.state.utm$smk.lagged.means.rand = lag.listw( penn.state.lw ,
                                                  penn.state.utm$smk_rand )
xbar = mean( penn.state.utm$smk_rand )
ybar = mean( penn.state.utm$smk.lagged.means.rand )
plot2 = ggplot( penn.state.utm@data , aes( x=smk_rand , y=smk.lagged.means.rand ) ) +
  xlim( 15 , 30 ) +
  ylim( 15 , 30 ) +
  geom_point() +
  geom_hline( yintercept=ybar ) +
  geom_vline( xintercept=xbar )

library( grid )
# windows( 12 , 6 )
grid.newpage()

pushViewport( viewport( layout=grid.layout(1,2) ) )
print( plot1 , vp=viewport(layout.pos.col=1 , height=5 ) )
print( plot2 , vp=viewport(layout.pos.col=2 , height=4 ) )
