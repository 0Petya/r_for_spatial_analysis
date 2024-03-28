#---------------------------------------------------------------------------
#-
#-  Code from Textbook: Intro to R for Spatial Analysis and Mapping, 2nd Ed.
#-
#-  Chapters 3 & 5: Using R as a GIS
#-
#-  Be sure to run code in previous file first
#-
#--------------------------------------------------------------------------

library( GISTools )
library( tmap )
library( sf )
library( dplyr )
library( ggplot2 )
library( tmap )

#-------------------------------------------------------------------
#  
#-  Histogram in Legend
#- 
#-------------------------------------------------------------------

data(newhaven)
blocks_sf = st_as_sf( blocks )
# windows( 9 , 9 )
tm_shape( blocks_sf ) +
  tm_polygons( "P_OWNEROCC" , title="Owner Occupied" ,
               palette="-YlOrRd" ,  # Notice the minus sign! & below ... 100
               breaks=c(0,round(quantileCuts(blocks$P_OWNEROCC,6),1),100) ,
               legend.hist=TRUE ) +
  tm_scale_bar( width=0.22 ) +
  tm_compass( position=c(0.8,0.07) ) +
  tm_layout( frame=FALSE , title="New Haven" ,
             title.size=2 , title.position=c(0.55,"top") ,
             legend.hist.size=0.5 )

#-------------------------------------------------------------------
#  
#-  Statistical Analysis
#- 
#-------------------------------------------------------------------

# windows( 9 , 9 )
hist( blocks$P_VACANT , breaks=40 , col="cyan" , 
      main="Distribution of Percent Vacant across Blocks" ,
      xlab="Percent Vacant" , ylab="Frequency" )

# windows( 9 , 7 )
ggplot( blocks@data , aes(P_VACANT) ) +
  geom_histogram( col="black" , fill="cyan" , bins=40 ) +
  xlab( "Percent Vacant" ) +
  ylab( "Frequency" ) +
  labs( title="Distribution of Percent Vacant across Blocks")


library( reshape )   ## needed for melt function below
##  https://www.statmethods.net/management/reshape.html

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

lm( p.vac ~ p.vac )
mod1 = lm( p.vac ~ p.ownerocc )
summary( mod1 )

df1 = data.frame( p.vac , p.ownerocc )
p1 = ggplot( df1 , aes( p.ownerocc, p.vac ) ) +
  # stat_summary(fun.data=mean_cl_normal) +
  geom_smooth( method='lm' ) +
  geom_point() +
  ylab("Proportion of Vacant Properties") +
  xlab("Proporion Owner Occupied") +
  labs(title = "Regression of Vacant Properties aginst Proportion Owner Occupied")

p2 = ggplot( df1 , aes(p.ownerocc, p.vac) ) +
  geom_smooth(method='loess') +
  geom_point() +
  ylab("Proportion of Vacant Properties") +
  xlab("Proporion Owner Occupied") +
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

library( sf )
library( GISTools )   
library( tmap )
library( sf )

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

windows( 12 , 8 )
tm_shape( us_states_sf ) +
  tm_polygons( "white" ) +
  tm_shape( torn ) +
    tm_dots( col="black" , size=0.06 , shape=1 , alpha=0.5 ) +
  tm_shape( us_states_sf ) +
    tm_borders( "blue" , lwd=2 ) 
 
## Using plot from base R
windows( 12 , 8 ) 
plot( us_states , col="aliceblue" )
plot( torn , add=TRUE , pch=1 , col="coral" , cex=0.4 )
plot( us_states , add=TRUE )

##  Use us_states2 which uses a different projection

us_states2_sf = st_as_sf( us_states2 )
torn2 = torn
torn2@proj4string = us_states2@proj4string

windows( 12 , 8 )
tm_shape( us_states2_sf ) +
  tm_polygons( "white" ) +
  tm_shape( torn2 ) +
    tm_dots( col="black" , size=0.06 , shape=1 , alpha=0.5 ) +
    tm_shape( us_states_sf ) +
    tm_borders( "blue" , lwd=2 ) 


##  Looking at a subset of states

index = us_states$STATE_NAME == "Missouri" | 
        us_states$STATE_NAME == "Arkansas" |
        us_states$STATE_NAME == "Oklahoma" | 
        us_states$STATE_NAME == "Kansas" | 
        us_states$STATE_NAME == "Nebraska" |
        us_states$STATE_NAME == "Iowa" | 
        us_states$STATE_NAME == "Illinois" |
        us_states$STATE_NAME == "Kentucky" |
        us_states$STATE_NAME == "Tennessee" 
index

##  Area of Interest

AoI = us_states[ index , ]
AoI_sf = us_states_sf[ index , ]

tm_shape( AoI_sf ) +
  tm_borders( col="black" ) +
  tm_layout( frame=FALSE ) +
  tm_shape( torn_sf ) +
    tm_dots( col="#FB6A4A" , size=0.2 , shape=1 , alpha=0.5 )

##  Need to "clip" out those tornados outside of these 9 states
##  IOW, keep only those tornados inside these 9 states

torn_clip_sf = torn_sf[ AoI_sf , ]

windows( 12 , 8 )
tm_shape( torn_clip_sf ) +
  tm_dots( col="#FB6A4A" , size=0.2 , shape=1 , alpha=0.5 ) +
  tm_shape( AoI_sf ) + 
    tm_borders()

windows( 12 , 8 )
tm_shape( AoI_sf ) +
  tm_borders() +
  tm_shape( AoI_torn ) +
    tm_dots( col="#FB6A4A" , size=0.2 , shape=1 , alpha=0.5 )

##  Buffers

data( tornados )
AoI_MO = us_states2[ us_states2$STATE_NAME == "Missouri" , ]
AoI_MO_buf = gBuffer( AoI_MO , width=25000 )

plot( AoI_MO )
plot( AoI_MO_buf , add=TRUE )

plot( AoI_MO_buf )
plot( AoI_MO , add=TRUE )


windows( 9 , 9 )
tm_shape( AoI_MO_buf ) +
  tm_borders() + 
  tm_shape( AoI_MO ) +
    tm_borders( col="blue" , lwd=2 )



