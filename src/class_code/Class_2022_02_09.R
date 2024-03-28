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
data( georgia )
class( georgia )

data( newhaven )    
####
####  Look at objects  (1) blocks  (2)  breach  (3)  roads

class( breach )
slotNames( breach )

class( blocks )
slotNames( blocks )

class( roads )
slotNames( roads )
View( roads@data )

plot( breach )
plot( blocks )

locator()

plot( roads ); points( breach )

# windows( 9 , 9 )
plot( blocks )
  points( breach )

library( tmap )

# windows( 9 , 9 )
tm_shape( breach ) +
  tm_dots( )

blocks@data$POP1990
blocks$POP1990     ## Note: these yield the same output

# windows( 10 , 10 )
tm_shape( blocks ) +
  tm_fill( "VACANT" , n=8 , palette="Greens" ) +
  tm_borders( col="darkblue" ) 

# windows( 10 , 10 )
tm_shape( blocks ) +
  tm_fill( "VACANT" , n=8 , palette="Greens" ) +
  tm_borders( col="blue" ) +
  tm_shape( breach ) +
  tm_dots( size=0.1 , col="maroon" )

# windows( 10 , 10 )
tm_shape( roads ) +
  tm_lines() 

# windows( 10 , 10 )
tm_shape( blocks ) +
  tm_fill( "VACANT" , n=8 , palette="Reds" ) +
  tm_borders( col="blue" ) +
  tm_shape( breach ) +
    tm_dots( size=0.1 , col="deepskyblue2" ) +
  tm_shape( roads ) + 
    tm_lines( )

roads@proj4string = blocks@proj4string
# windows( 10 , 10 )
tm_shape( blocks ) +
  tm_fill( "VACANT" , n=8 , palette="Reds" , breaks=seq(0,300,30)) +
  tm_borders( col="blue" ) +
  tm_shape( roads ) + 
    tm_lines( col="gray" ) +
  tm_shape( breach ) +
    tm_dots( size=0.2 , col="dodgerblue2" ) 

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
#--------------------------------------------------------------------------




roads@proj4string = blocks@proj4string

  