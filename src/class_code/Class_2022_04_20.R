#---------------------------------------------------------------------
#-
#-  Code from Geospatial Health Data: Modeling and Visualization with
#-  INLA and Shiny
#-
#-  Chapter 5: Areal Data
#-
#---------------------------------------------------------------------

# setwd("C:/Users/srigd/Dropbox/MyCourses/BST_5600_Spring2022")

##  install.packages( "INLA",
##                  repos=c(getOption("repos") , 
##                          INLA="https://inla.r-inla-download.org/R/stable") , 
##                  dep=TRUE)

library( INLA )   ## all caps
library( spdep )
library( SpatialEpi )
library( dplyr )
library( tmap )

class( pennLC )
pennLC[[1]]
pennLC[[2]]
pennLC[[3]]

map = pennLC$spatial.polygon
class( map )
plot( map )

pennLC.data = pennLC$data
View( pennLC.data )

df = data.frame( county = names(map) )
rownames( df ) = names( map )
map = SpatialPolygonsDataFrame(map, df, match.ID = TRUE)
View( map )

d = group_by( pennLC.data , county ) %>% 
  summarize( Y = sum( cases ) , pop = sum(population) )
head( d )
names( d ) = c( "county" , "Y" , "pop" )

map = merge( map , d )
map@data$idarea = 1:nrow(map@data)
str( pennLC[[3]] )
map = merge( map , pennLC[[3]] )
View( map@data )

# windows( 9 , 7 )
map@data$LC.rate = 100000*map@data$Y/map@data$pop
plot( map@data$smoking , map@data$LC.rate , 
      xlab="Smoking %" , ylab="Lung Cancer Rate/100,000" )
plot( map@data$smoking , map@data$LC.rate , 
      xlab="Smoking %" , ylab="Lung Cancer Rate/100,000" ,
      cex=0.005*sqrt(map@data$pop) )

lin.mod = lm( map@data$LC.rate ~ map@data$smoking )
summary( lin.mod )

# windows( 9 , 7 )
tm_shape( map ) + 
  tm_borders() +
  tm_fill( col="LC.rate" )

nb = poly2nb( map )
nb2INLA( "map.adj" , nb )
g = inla.read.graph( filename="map.adj" )
file.remove("map.adj")

####
####  Y ~ POISSON( P_i exp( beta0 + beta1*smoking + u_i + v_i ) )
#### 
####  Apply separate f's for u_i and v_i
####

map@data$idarea1 = map@data$idarea
hyperPrior1 = list( prec = list(prior="loggamma",param=c(1,0.01)) )
hyperPrior2 = list( prec = list(prior="loggamma",param=c(1,0.01)) )

formula1 =  Y ~ smoking + 
  f( idarea , model="besag" , graph=g , hyper=hyperPrior1 ) +
  f( idarea1 , model="iid"  , graph=g , hyper=hyperPrior2 )

mod1 = inla( formula1 , family="poisson" , data=map@data , offset=log(pop) )
summary( mod1 )

####
####  Y ~ POISSON( P_i exp( beta0 + beta1*smoking + u_i + v_i ) )
#### 
####  Use model="besag" to combine into one step
####

hyperPriors = list( prec.unstruct = list(prior="loggamma",param=c(1,0.01)) ,
                    prec.spatial  = list(prior="loggamma",param=c(1,0.01)) )

formula =  Y ~ smoking + f( idarea , model="bym" , graph=g, 
                            hyper = hyperPriors )
mod = inla( formula , family="poisson" , data=map@data , offset=log(pop) )
summary( mod )

####
####  Extract Posterior Information
####

mod$names.fixed
mod$summary.fixed
mod$marginals.fixed
str( mod$marginals.fixed )

beta0hat = mod$summary.fixed$mean[1]
beta1hat = mod$summary.fixed$mean[2]

sigma2vhat = 1 / mod$summary.hyperpar$mean[1]
sigma2uhat = 1 / mod$summary.hyperpar$mean[2]

# windows( 9 , 10 )
par( mfrow=c(2,1) )
plot( mod$marginals.fixed$`(Intercept)` , type="l" , lwd=2 )
plot( mod$marginals.fixed$smoking , type="l" , lwd=2 )

# windows( 9 , 10 )
par( mfrow=c(2,1) )
plot( mod$marginals.hyperpar$`Precision for idarea (iid component)` , 
      xlim=c(0,1000) , xlab="Precision for iid component" , type="l" ) 
plot( mod$marginals.hyperpar$`Precision for idarea (spatial component)` , 
      xlim=c(0,1000) , xlab="Precision for spatial component" , type="l" ) 

mod$summary.fitted.values

map@data$RR = mod$summary.fitted.values[,"mean"] / map@data$pop
map@data$LL = mod$summary.fitted.values[,"0.025quant"] / map@data$pop
map@data$UL = mod$summary.fitted.values[,"0.975quant"] / map@data$pop
map@data$rawLC.rate = map@data$Y / map@data$pop
View( map@data )
##

# windows( 9 , 12 )
tm_shape( map ) +
  tm_borders() +
  tm_polygons( col=c("rawLC.rate","LL","RR","UL") , 
               breaks=seq(0.0000,0.0014,0.0001) ) +
  tm_layout( legend.show = FALSE , 
             title=c("Raw Rate","Bayesian LL","Posterior Mean","Bayesian UL") ,
             title.position=c(0.65,0.95) )
