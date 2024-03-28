#---------------------------------------------------------------------------
#-
#-  Code from Textbook: Intro to R for Spatial Analysis and Mapping, 2nd Ed.
#-
#-  Chapter 3: Handling Spatial Data in R
#-
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#-
#-  sp (spatial, OLD)
#-  sf (simple features, NEW)
#-
#-  sp - Spatial Data Formats in R
#- 
#-  Without Attributes          With Attributes
#-  ------------------------------------------------------
#-  SpatialPoints               SpatialPointsDataFrame 
#-  SpatialLines                SpatialLinesDataFrame
#-  SpatialPolygons             SpatialPolygonsDataFrame
#-  ------------------------------------------------------
#-
#--------------------------------------------------------------------------

library( GISTools )
library( sp )
library( sf )
library( tmap )
data( georgia )
data( newhaven )

#--------------------------------------------------------------------------
#-
#-  sp (spatial, OLD)
#-  sf (simple features, NEW)
#-
#-  sf - Spatial Data Formats in R
#- 
#-  Feature             Description
#-  ----------------------------------------------------------------
#-  POINT               Single point
#-  LINESTRING          Sequence of nonintersecting line segments 
#-  POLYGON             Points defining a polygon.  
#-                      Closed "LINESTRING". If more than one 
#-                      polygon is given, subsequent polygons are
#-                      holes
#-  MULTIPOINT          Set of POINTs
#-  MULTILINESTRING     Set of LINESTRINGs
#-  MULTIPOLYGON        Set of POLYGONs
#-  ----------------------------------------------------------------
#-
#-------------------------------------------------------------------

#-------------------------------------------------------------------
#  Converting the class from  sp  to  sf .
#-------------------------------------------------------------------

class( georgia )
class( roads )

georgia_sf = st_as_sf( georgia )
class( georgia_sf )
str( georgia_sf )
slotNames( georgia_sf )
georgia_sf$geometry
str( georgia_sf$geometry )

blocks_sf = st_as_sf( blocks )
class( blocks_sf )
str( blocks_sf )
slotNames( blocks_sf )
blocks_sf$geometry
str( blocks_sf$geometry )

breach_sf = st_as_sf( breach )
class( breach_sf )
str( breach_sf )
breach_sf$geometry
str( breach_sf$geometry )

roads_sf = st_as_sf( roads )
class( roads_sf )
str( roads_sf$geometry )

roads@proj4string = blocks@proj4string
roads_sf = st_as_sf( roads )

# windows( 10 , 10 )
tm_shape( blocks_sf ) +
  tm_fill( "POP1990" , palette="Reds" , breaks=seq(0,8000,500)) +
  tm_borders( col="darkred" ) +
  tm_shape( roads_sf ) + 
  tm_lines( col="gray" ) +
  tm_shape( breach_sf ) +
  tm_dots( size=0.2 , col="black" ) 

# windows( 10 , 10 )
plot( georgia )
plot( georgia_sf )


#-------------------------------------------------------------------
#  Reading and writing  shp  files.
#-
#- Tiger files and  "tigris"  package in R
#- 
#-------------------------------------------------------------------

library( tigris )
library( stringr )
library( dplyr )

CT = counties( "Connecticut" )

# windows( 10 , 10 )
tm_shape( CT ) +
  tm_fill( "AWATER" , palette="Blues" ) +
  tm_borders( col="darkblue" )

# setwd("C:/Users/srigd/Dropbox/MyCourses/BST_5600_Spring2022")
CT_COVID = read.csv("COVID-19-CT-2020-04-12.csv")

CT_COVID$FIPS_code = as.numeric( str_sub( CT_COVID$FIPS , -3 , -1 ) )
CT$FIPS_code = as.numeric( CT$COUNTYFP )

CT1 = left_join( CT , CT_COVID , "FIPS_code" )
CT1$COVID_rate = CT1$COVIDcases/CT1$POP2019
# windows( 10 , 10 )
tm_shape( CT1 ) +
  tm_fill( "COVID_rate" , palette="Purples" ) +
  tm_borders( col="darkblue" )


#-------------------------------------------------------------------
#  
#- More on  tmap
#- 
#-------------------------------------------------------------------

# windows( 9 , 9 )
tm_shape( georgia_sf ) +
  tm_fill( "red" ) + 
  tm_borders( lty="dashed" , col="purple" ) +
  tm_style( "natural" , bg.color="pink" ) +
  tm_layout( title="Ugly Map of Georgia" , title.size=2 , 
             title.position=c(0.7,"top") )

# windows( 9 , 9 )
tm_shape( georgia_sf ) +
  tm_fill( "goldenrod2" ) + 
  tm_borders( lty="dashed" , col="black" ) +
  tm_style( "natural" , bg.color="beige" ) +
  tm_layout( title="Better Map of Georgia" , title.size=2 , 
             title.position=c(0.54,"top") ) +
  tm_text( "Name" , size=1 ) 

g = st_union( georgia_sf )
# windows( 9 , 9 )
tm_shape( georgia_sf ) +
  tm_fill( "goldenrod2" ) + 
  tm_borders( lty="dashed" , col="black" ) +
  tm_style( "natural" , bg.color="beige" ) +
  tm_layout( title="Better Map of Georgia" , title.size=2 , 
             title.position=c(0.54,"top") ) +
  tm_text( "Name" , size=0.5 ) +
  tm_shape( g ) +
  tm_borders( lwd=3 , col="brown" )

t1 = tm_shape( georgia_sf ) +
  tm_fill( "goldenrod2" ) + 
  tm_borders( lty="dashed" , col="black" ) +
  tm_style( "natural" , bg.color="beige" ) +
  tm_layout( title="Better Map of Georgia" , title.size=2 , 
             title.position=c(0.54,"top") ) +
  tm_text( "Name" , size=0.5 ) +
  tm_shape( g ) +
  tm_borders( lwd=3 , col="brown" )

t2 = tm_shape( georgia2 ) +
  tm_fill( "goldenrod2" ) + 
  tm_borders( lty="dashed" , col="black" ) +
  tm_style( "natural" , bg.color="beige" ) +
  tm_layout( title="Better Map of Georgia" , title.size=2 , 
             title.position=c(0.54,"top") ) +
  tm_text( "Name" , size=0.5 ) +
  tm_shape( g ) +
  tm_borders( lwd=3 , col="brown" )

library( grid )
# windows( 12 , 7 )
grid.newpage()
pushViewport( viewport( layout=grid.layout(1,2) ) )  
print( t1 , vp=viewport(layout.pos.col=1 , height=5 ) )
print( t2 , vp=viewport(layout.pos.col=2 , height=5 ) )

georgia@proj4string
georgia2@proj4string



index = c(81, 82, 83, 150, 62, 53, 21, 16, 124, 121, 17)
georgia_sf.sub = georgia_sf[index,]

windows( 9 , 9 )
tm_shape( georgia_sf ) +
  tm_fill( "white" ) +
  tm_borders( "gray" , lwd = 0.75) +
  tm_shape( g ) +
  tm_borders( lwd = 2 ) +
  tm_shape( georgia_sf.sub ) +
  tm_fill( "tan" ) +
  tm_borders() +
  tm_layout( frame = TRUE , title = "Georgia with a subset of counties", 
             title.size = 1 , title.position = c( 0.02 , "bottom" ) )

windows( 9 , 9 )
tm_shape( georgia_sf ) +
  tm_fill( "white" ) +
  tm_borders( "gray" , lwd = 0.75) +
  tm_shape( g ) +
  tm_borders( lwd = 2 ) +
  tm_shape( georgia_sf.sub ) +
  tm_fill( "PctBach" ) +
  tm_borders() +
  tm_layout( frame = TRUE , title = "Georgia with a subset of counties", 
             title.size = 1 , title.position = c( 0.02 , "bottom" ) )






