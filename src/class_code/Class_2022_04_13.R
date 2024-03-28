#---------------------------------------------------------------------
#-
#-  Code from Geospatial Health Data: Modeling and Visualization with
#-  INLA and Shiny
#-
#-  Chapter 5: Areal Data
#-
#---------------------------------------------------------------------

# install.packages( "INLA",
#                   repos=c(getOption("repos") , 
#                           INLA="https://inla.r-inla-download.org/R/stable") , 
#                   dep=TRUE)

library( INLA )   ## all caps
library( spdep )
library( SpatialEpi )
library( dplyr )

class( pennLC )
pennLC[[1]]
pennLC[[2]]
pennLC[[3]]

map = pennLC$spatial.polygon
class( map )
plot( map )

pennLC.data = pennLC$data
# View( pennLC.data )

df = data.frame(county = names(map), neigh = rep(0, length(map)))
rownames( df ) = names( map )
map = SpatialPolygonsDataFrame(map, df, match.ID = TRUE)
# View( map )

d = group_by( pennLC.data , county ) %>% 
      summarize( Y = sum( cases ) , pop = sum(population) )
head( d )
names( d ) = c( "county" , "Y" , "pop" )

map = merge( map , d )
map@data$idarea = 1:nrow(map@data)
str( pennLC[[3]] )
map = merge( map , pennLC[[3]] )
# View( map@data )

# windows( 9 , 7 )
map@data$LC.rate = 10000*map@data$Y/map@data$pop
plot( map@data$smoking , map@data$LC.rate , 
      xlab="Smoking %" , ylab="Lung Cancer Rate/100,000" )
plot( map@data$smoking , map@data$LC.rate , 
      xlab="Smoking %" , ylab="Lung Cancer Rate/100,000" ,
      cex=0.005*sqrt(map@data$pop) )

lin.mod = lm( map@data$LC.rate ~ map@data$smoking )
summary( lin.mod )

library( tmap )
# windows( 9 , 7 )
tm_shape( map ) + 
  tm_borders() +
  tm_fill( col="LC.rate" )

##
##  Find the expected number of cases in each county
##
pennLC$data = pennLC$data[ order( pennLC$data$county ,
                                  pennLC$data$race ,
                                  pennLC$data$gender ,
                                  pennLC$data$age ) , ]
E = expected( population = pennLC$data$population ,
              cases = pennLC$data$cases ,
              n.strata = 16 )    ## 16 = (2 races) x (2 genders) x (4 age groups)
map@data$E = E

setwd("C:/Users/srigd/Dropbox/MyCourses/BST_5600_Spring2022")
nb = poly2nb( map )
nb2INLA( "map.adj" , nb )
g = inla.read.graph( filename="map.adj" )

formula =  Y ~ smoking + f( idarea , model="bym" , graph=g, 
                      hyper =
                       list(prec.unstruct = list(prior="loggamma",param=c(1,0.01)),
                            prec.spatial = list(prior="loggamma",param=c(1,0.01))))
mod = inla( formula , family="poisson" , data=map@data , E=E ) 
summary( mod )
class( mod )

mod$names.fixed
mod$summary.fixed
mod$marginals.fixed
str( mod$marginals.fixed )

windows( 9 , 10 )
par( mfrow=c(2,1) )
plot( mod$marginals.fixed$`(Intercept)` , type="l" , lwd=2 )
plot( mod$marginals.fixed$smoking , type="l" , lwd=2 )

windows( 9 , 10 )
par( mfrow=c(2,1) )
plot( mod$marginals.hyperpar$`Precision for idarea (iid component)` , 
      xlim=c(0,1000) , xlab="Precision for iid component" , type="l" ) 
plot( mod$marginals.hyperpar$`Precision for idarea (spatial component)` , 
      xlim=c(0,1000) , xlab="Precision for spatial component" , type="l" ) 

mod$summary.fitted.values
map@data$RR = mod$summary.fitted.values[,"mean"]
map@data$LL = mod$summary.fitted.values[,"0.025quant"]
map@data$UL = mod$summary.fitted.values[,"0.975quant"]
View( map@data )
##

windows( 9 , 12 )
tm_shape( map ) +
  tm_borders() +
  tm_polygons( col=c("LL","RR","UL") , breaks=seq(0.65,1.30,0.05) )






