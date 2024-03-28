#----------------------------------------------------------------------------
#-
#-  Code from Textbook: Intro to R for Spatial Analysis and Mapping, 2nd Ed.
#-
#-  Chapter 2: Data and Plots 
#-
#-  Be sure to run the previous files first to define all variables
#-
#----------------------------------------------------------------------------


library( ggplot2 )
library( tmap )
library( GISTools )
data( georgia )

str( georgia )
slotNames(georgia)
####-----------------------------------------------------------------  
####  Using  tmap  package to create choropleth maps
####-----------------------------------------------------------------

# windows( 10 , 10 )
tm_shape( georgia ) +
  tm_polygons( "PctBach" )

# windows( 10 , 10 )
tm_shape( georgia ) +
  tm_fill( "PctBach" , n=5 , style="quantile" ) +
  tm_borders( col="black" )

# windows( 10 , 10 )
tm_shape( georgia ) +
  tm_fill( "PctBach" , breaks=seq(0,40,5) , palette="Blues" ) +
  tm_borders( col="darkred" )

library(shiny)
tmaptools::palette_explorer()

####---------------------------------------------------------------
####  Create choropleth map for number of housing units by county
####---------------------------------------------------------------

##  search ...  georgia demographics housing units by county

library( dplyr )

HousingUnits = read.csv("./data/raw/class/georgia_housing_units.csv")

names( HousingUnits ) = c( "Name" , "HousingUnits" )

georgia1 = left_join( georgia@data, HousingUnits, by ='Name')
georgia@data = georgia1

# windows( 10 , 10 )
tm_shape( georgia ) +
  tm_fill( "HousingUnits" , n=6 , palette="Reds" ) +
  tm_borders( col="darkred" )

####  ? ? ? ? ? ? ? ? ? ?
####  Debug ! ! !

as.numeric( HousingUnits$HousingUnits )   ## ? ? ? ?

HousingUnits$HousingUnits =
       as.numeric( gsub( "," , "" , HousingUnits$HousingUnits ) )
####
####  Start over
####

data( georgia )           ## Reload data
HousingUnits = read.csv("GAHousingUnits.csv")
names( HousingUnits ) = c( "Name" , "HousingUnits")

HousingUnits$HousingUnits =
  as.numeric( gsub( "," , "" , HousingUnits$HousingUnits ) )

georgia1 = left_join( georgia@data, HousingUnits, by ='Name')
georgia@data = georgia1

# windows( 10 , 10 )
tm_shape( georgia ) +
  tm_fill( "HousingUnits" , n=6 , palette="Reds" ) +
  tm_borders( col="darkred" )
##  Vary n=

#----------------------------------------------------------------------------
#-
#-  Code from Textbook: Intro to R for Spatial Analysis and Mapping, 2nd Ed.
#-
#-  Chapter 4: Scripting and Functions 
#-
#----------------------------------------------------------------------------

data( georgia )

####
####  Find a distance matrix for counties in Georgia
####

distance = function( v1 , v2 )
{
  d = sqrt( (v1[1]-v2[1])^2 + (v1[2]-v2[2])^2 )
  return( d )
}

v1 = c(0,0)
v2 = c(1,1)
distance( v1 , v2 )

dist = matrix( 0 , nrow=159 , ncol=159 )

for( i in 1:159 )
{
  for( j in 1:159 )
  {
    dist[i,j] = distance( c(georgia$X[i],georgia$Y[i]) , 
                          c(georgia$X[j],georgia$Y[j]) ) 
  }
}

####
####  apply
####
m = matrix( c(1,2,3,
              4,5,6,
              7,8,9) , nrow=3 , ncol=3 , byrow=TRUE )
m
apply( m , 1 , sum )    ## get the 1st row sum, 2nd row sum, ... 
apply( m , 2 , sum )    ## get the 1st col sum, 2nd col sum, ...

apply( m , 1 , max )
apply( m , 2 , max )

####
####  which and which.max
####
set.seed( 123 )
x = rnorm( 5 )
which( x > 0 )  
which.max( x )
which.min( x )

which( m > 5 )   ## works wierd on matrices

####
####  Which two counties are furthest apart
####
max( dist )
which.max( dist )
apply( dist , 1 , max )
which.max( apply( dist , 1 , max ) )
which.max( dist[,20] )

georgia$Name[20]
georgia$Name[41]

