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
#- More on the  tmap  package
#- 
#-------------------------------------------------------------------

g = st_union( georgia_sf )

# windows( 9 , 9 )
tm_shape( g ) +
  tm_borders( col="skyblue" , lwd=3 )

# windows( 9 , 9 )
tm_shape( georgia_sf ) +
  tm_fill( "goldenrod2" ) + 
  tm_borders( lty="dashed" , col="black" ) +
  tm_style( "natural" , bg.color="beige" ) +
  tm_layout( title="Better Map of Georgia" , title.size=2 , 
             title.position=c(0.54,"top") ) +
  tm_text( "Name" , size=0.5 ) +
  tm_shape( g ) +
  tm_borders( lwd=4 , col="goldenrod4" )


#-------------------------------------------------------------------
#  
#- Assigning plots to variables and plotting multiple maps
#- 
#-------------------------------------------------------------------

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

#-------------------------------------------------------------------
#  
#- Plotting subsets of counties
#- 
#-------------------------------------------------------------------

index = c(81, 82, 83, 150, 62, 53, 21, 16, 124, 121, 17)
georgia_sf.sub = georgia_sf[index,]

# windows( 9 , 9 )
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

# windows( 9 , 9 )
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

#-------------------------------------------------------------------
#  
#- Leaflet
#- 
#-------------------------------------------------------------------

library( leaflet )

# Can't use windows command

m <- leaflet()
m <- addTiles(m)
m <- addMarkers(m, lng=174.768, lat=-36.852, popup="The birthplace of R")
m

#### The pipe operator  %>%

m <- leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng=-84.3927, lat=32.0341, popup="Plains, GA")
m  

#-------------------------------------------------------------------
#  
#- Back to the New Haven Data Set
#- 
#-------------------------------------------------------------------

rm( list = ls() )
data( newhaven )

class( blocks )
class( breach )
class( tracts )
class( roads )
##  Note: 129 blocks, but only 29 tracts.  Tracts are bigger.

# windows( 12 , 8 )
map1 = tm_shape( blocks ) +
         tm_borders( col="gray" ) +
         tm_layout( title = "New Haven Blocks", 
             title.size = 1.5 , title.position = c( 0.05 , "bottom" ) )

tracts@proj4string = blocks@proj4string         
map2 = tm_shape( tracts ) +
         tm_borders( col="dodgerblue" ) +
         tm_layout( title = "New Haven Tracts", 
            title.size = 1.5 , title.position = c( 0.05 , "bottom" ) )

library( grid )
# windows( 12 , 7 )
grid.newpage()
pushViewport( viewport( layout=grid.layout(1,2) ) )  
print( map1 , vp=viewport(layout.pos.col=1 , height=5 ) )
print( map2 , vp=viewport(layout.pos.col=2 , height=5 ) )


#-------------------------------------------------------------------
#  
#- Converting New Haven data to sf class
#- 
#-------------------------------------------------------------------

blocks_sf = st_as_sf( blocks )
breach_sf = st_as_sf( breach )
tracts_sf = st_as_sf( tracts )
roads_sf  = st_as_sf( roads )

class( blocks_sf )
class( breach_sf )
class( tracts_sf )
class( roads_sf )


data.frame( blocks_sf )
View( data.frame(blocks_sf) )
colnames( blocks@data )   ## mostly demographic data
colnames( tracts@data )   ## mostly housing data

## tmap the sf files
# windows( 12 , 8 )
map1 = tm_shape( blocks_sf ) +
  tm_borders( col="gray" ) +
  tm_layout( title = "New Haven Blocks", 
             title.size = 1.5 , title.position = c( 0.05 , "bottom" ) )

tracts@proj4string = blocks@proj4string         
map2 = tm_shape( tracts_sf ) +
  tm_borders( col="dodgerblue" ) +
  tm_layout( title = "New Haven Tracts", 
             title.size = 1.5 , title.position = c( 0.05 , "bottom" ) )

library( grid )
# windows( 12 , 7 )
grid.newpage()
pushViewport( viewport( layout=grid.layout(1,2) ) )  
print( map1 , vp=viewport(layout.pos.col=1 , height=5 ) )
print( map2 , vp=viewport(layout.pos.col=2 , height=5 ) )

tm_shape( tracts ) +
  tm_fill( col="P_VACANT") +
  tm_borders( col="gray" )

##  Heat map using kernel density estimate (KDE)

KDE = kde.points( breach , lims=tracts , h = 10000 )
200*200
class( KDE )
breach.dens = st_as_sf( KDE )
plot( breach.dens )
class( breach.dens )

# windows( 9 , 9 )
tm_shape( tracts ) +
  tm_borders() +
  tm_shape( breach ) +
    tm_dots() + 
  tm_shape( KDE ) +
    tm_raster( alpha=0.5 )     ## Need alpha=


