#------------------------------------------------------------------------
#-
#-   Kernel Density Estimation in One Dimension
#-
#------------------------------------------------------------------------

kde = function( xvec , x , bw )
{
  m = length( xvec )
  n = length( x )
  y = matrix( 0 , nrow=n , ncol=m )
  for ( i in 1:n ) y[i,] = dnorm( xvec , x[i] , bw )
  z = apply( y , 2 , sum ) 
  return( z )
}

kde.uniform = function( xvec , x , bw )
{
  m = length( xvec )
  n = length( x )
  y = matrix( 0 , nrow=n , ncol=m )
  for ( i in 1:n ) y[i,] = dunif( xvec , min=x[i]-bw , max=x[i]+bw )
  z = apply( y , 2 , sum ) 
  return( z )
}


x = c(53,94,77,83,84,72,89,68,73,82)
n = length(x)
bw = 2
# windows( 9 , 5 )
plot( x , 1:n , type="n" , ylim=c(0,1) , xlim=c(40,120) )
points( x , rep(0,n) , pch=19 )
x0 = seq(40,110,0.25)
for( i in 1:n ) lines( x0 , dnorm(x0,x[i],bw) )
y = kde( x0 , x , bw ) 
lines( x0 , y , lwd=4 , col="red")

bw = 2
# windows( 9 , 5 )
plot( x , 1:n , type="n" , ylim=c(0,1) , xlim=c(40,120) )
points( x , rep(0,n) , pch=19 )
x0 = seq(40,110,0.25)
for( i in 1:n ) lines( x0 , dunif(x0,x[i]-bw,x[i]+bw) )
y = kde.uniform( x0 , x , bw ) 
lines( x0 , y , lwd=4 , col="red")

####
####  Try a larger data set
####

x = rnorm( 100 , 200 , 5 )
n = length(x)
bw = 1
# windows( 9 , 5 )
plot( x , 1:n , type="n" , ylim=c(0,1) , xlim=c(180,220))
points( x , rep(0,n) , pch=19 )
x0 = seq( 180 , 220 , length.out=200 ) 
for( i in 1:n ) lines( x0 , dnorm(x0,x[i],bw) , col="gray" )
y = kde( x0 , x , bw ) 
lines( x0 , y/20 , lwd=4 , col="red")

bw = 5
# windows( 9 , 5 )
plot( x , 1:n , type="n" , ylim=c(0,1) , xlim=c(180,220))
points( x , rep(0,n) , pch=19 )
x0 = seq( 180 , 220 , length.out=200 ) 
for( i in 1:n ) lines( x0 , dunif(x0,x[i]-bw,x[i]+bw) )
y = kde.uniform( x0 , x , bw ) 
lines( x0 , y/10 , lwd=4 , col="red")



library( GISTools )
library( tmap )
library( tmaptools )
library( sp )
library( spatstat )
library( raster )

# install.packages("remotes")
# remotes::install_github("mtennekes/oldtmaptools")
library( oldtmaptools )


data( newhaven )

tm_shape( blocks ) + 
  tm_borders() +
  tm_shape( breach ) +
  tm_dots( col="navyblue" )

blocks_sf = st_as_sf( blocks )
areas = st_area( blocks_sf )    ## Note area units are in ft^2
## so distance units are in ft

choose_bw = function( spdf )    ##  Equation (6.2) in book
{
  X = coordinates( spdf )
  sigma = c( sd(X[,1]) , sd(X[,2]) ) * ( 2 / 3*nrow(X))^(1/6)
  convfactor = 1.60934/5280   ## convert from ft to km
  convfactor = 1/1000
  return( sigma*convfactor )
}

## Experiment with different bandwidths
## my_bw = choose_bw(breach)
heuristic_bw <- choose_bw(breach)
my_bw = c(1.5,1.5)
breach_dens = smooth_map( breach , cover=blocks , 
                          bandwidth = my_bw )
tm_shape( breach_dens$raster ) +
  tm_raster()


tm_shape( blocks ) +
  tm_borders( alpha=0.5 ) +
  tm_shape( breach_dens$iso ) +
  tm_lines( col="tomato4" , lwd=2 ) +
  tm_shape( blocks ) +
  tm_borders( col="gray50" , lwd=0.6 ) +
  tm_shape( breach ) +
  tm_dots( col="navyblue" )

## Section 6.4 Further Uses of KDE
##

tmap_mode( "plot" )
contours = seq( 0 , 4.0 , by=0.4 )

# Create the maps and store them in variables
# windows( 12 , 7 )
my_bw = 1.5
brn_dens = smooth_map( burgres.n , cover=blocks , 
                       breaks=contours , style="fixed" ,
                       bandwidth=my_bw )
dn = tm_shape( blocks ) +
  tm_borders() +
  tm_shape( brn_dens$polygons ) +
  tm_fill( col='level' , alpha=0.8 ) +   ## Mistake in book
  tm_layout( title="Nonforced Burglaries")
brf_dens = smooth_map( burgres.f , cover=blocks , 
                       breaks=contours , style="fixed" ,
                       bandwidth=my_bw )
df = tm_shape( blocks ) +
  tm_borders() +
  tm_shape( brf_dens$polygons ) +
  tm_fill( col='level' , alpha=0.8 ) +   ## Mistake in book
  tm_layout( title="Forced Burglaries")
tmap_arrange( dn , df )

## Hexagonal binning  <<  S K I M   2022  >>
##
## install.packages( "fMultivar" , dep=TRUE )
library( fMultivar )

spdf = blocks
coordinates(spdf)
hbins = fMultivar::hexBinning( coordinates(spdf) )
plot( hbins$x , hbins$y , asp=1 )

hexbin_map = function( spdf , ... )
{
  hbins = fMultivar::hexBinning( coordinates(spdf) , ... )
  u = c( 1 , 0 , -1 , -1 , 0 , 1 )
  v = c( 1 , 2 , 1 , -1 , -2 , -1 )
  u = u * min( diff( unique( sort( hbins$x))))
  v = v * min( diff( unique( sort( hbins$y))))/3
  hexes_list = vector( length(hbins$x) , mode="list" )
  for ( i in 1:length(hbins$x) )
  {
    pol= Polygon( cbind( u + hbins$x[i] , v + hbins$y[i] ) , 
                  hole=FALSE )
    hexes_list[[i]] = Polygons( list( pol) , i )
  }
  ## Note error in book for next line (correcet on web site)
  hex_cover_sp <- SpatialPolygons( hexes_list, 
                                   proj4string=CRS(proj4string(spdf)))
  hex_cover = SpatialPolygonsDataFrame( hex_cover_sp ,
                                        data.frame( z=hbins$z , match.ID=FALSE ) )
  return( hex_cover )
}

tmap_mode( "view" )
breach_hex = hexbin_map( breach , bins=20 )
plot( breach_hex )
tm_shape( breach_hex ) +
  tm_fill( col="z" , title="Count" , alpha=0.7 )

## Make the area of the polygon proportional to the density
## See book's code

## 6.5 Second-Order Analysis of Point Patterns
##

##  The following function is from
##  https://www.r-bloggers.com/example-8-41-scatterplot-with-marginal-histograms/

scatterhist = function(x, y, xlab="", ylab="" ){
  zones=matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
  layout(zones, widths=c(4/5,1/5), heights=c(1/5,4/5))
  xhist = hist(x, plot=FALSE)
  yhist = hist(y, plot=FALSE)
  top = max(c(xhist$counts, yhist$counts))
  par(mar=c(3,3,1,1))
  plot(x,y)
  par(mar=c(0,3,1,1))
  barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0)
  par(mar=c(3,0,1,1))
  barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0, horiz=TRUE)
  par(oma=c(3,3,0,0))
  mtext(xlab, side=1, line=1, outer=TRUE, adj=0, 
        at=.8 * (mean(x) - min(x))/(max(x)-min(x)))
  mtext(ylab, side=2, line=1, outer=TRUE, adj=0, 
        at=(.8 * (mean(y) - min(y))/(max(y) - min(y))))
}
#set.seed(1)
windows( 9 , 9 )
x = runif(1000,-1,1)
y = runif(1000,-1,1)
plot ( x , y )
xx = seq(-1,1,0.01)
lines( xx , sqrt(1-xx^2) )
lines( xx , - sqrt(1-xx^2) )
xy = cbind(x,y)
dist = sqrt( xy[,1]^2 + xy[,2]^2 )
indx =  ( dist < 1 )
sum( indx )
xy1 = xy[ indx , ]
points( xy1[,1] , xy1[,2] , pch=19 , col="red" )

scatterhist( xy1[,1] , xy1[,2] )

## install.packages( "spatstat" , dep=TRUE )
library( spatstat )
data( bramblecanes )
str( bramblecanes )

# windows( 9 , 9 )
plot( bramblecanes )
x = bramblecanes$x
y = bramblecanes$y
marks = bramblecanes$marks
x0 = x[ marks == 0 ]
y0 = y[ marks == 0 ]
x1 = x[ marks == 1 ]
y1 = y[ marks == 1 ]
x2 = x[ marks == 2 ]
y2 = y[ marks == 2 ]
# windows( 9 , 9 )
plot( x0 , y0 , pch=19 , col="red" )
points( x1 , y1 , pch=21 , col="blue" )
points( x2 , y2 , pch=10 , col="green2" , cex=1.5 )
# windows( 12 , 4.5 )
par( mfrow=c(1,3) )
plot( x0 , y0 , pch=19 , col="red" , asp=1 , xlim=c(0,1) , ylim=c(0,1) )
plot( x1 , y1 , pch=19 , col="blue" , asp=1 , xlim=c(0,1) , ylim=c(0,1) )
plot( x2 , y2 , pch=19 , col="green2" , asp=1 , xlim=c(0,1) , ylim=c(0,1) , cex=1.5 )

kf = Kest( bramblecanes , correction="border" )
# windows( 9 , 9 ) 
plot( kf )
## NOTE:  The argument to Kest must be a ppp object (point process pattern)
## See the ppp() and as.ppp() functions

xPoisson = runif(100)
yPoisson = runif(100)
# windows( 12 , 7 )
par( mfrow=c(1,2) )
plot( xPoisson , yPoisson )
X = ppp(xPoisson, yPoisson, c(0,1),c(0,1))
kf.Poisson = Kest( X , correction="border" )
plot( kf.Poisson )

kfr = kf.Poisson$r
kftheo = kf.Poisson$theo           ## assuming Poisson process
kfborder = kf.Poisson$border       ## est of K function with border correction
# windows( 9 , 9 )
plot( kfr , kfborder , type="l" )  ## plot of Khat
lines( kfr , kftheo , col="red" )

## Monday - March 2, 2020
## Start here

# windows( 12 , 9 )
par( mfrow=c(3,4))
kfEst = matrix( 0 , nrow=length(kfr) , ncol=12 )
for ( i in 1:12 )
{
  xPoisson = runif(100)
  yPoisson = runif(100)
  X = ppp( xPoisson, yPoisson, c(0,1) , c(0,1) )  ## Create a ppp object
  ## The window is required.  There is no default.  R doesn't try to guess.                       
  kf.Poisson = Kest( X , correction="border" )
  plot( kf.Poisson , type="l" )
  kfEst[ , i ] = kf.Poisson$border
}

# windows( 12 , 9 )
plot( c(1,2) , c(1,2) , type="n" , xlim=c(0,1.1*max(kf.Poisson$r)) ,
      ylim=c(0,1.1*max(kf.Poisson$theo)))
for ( i in 1:12 ) lines( kf.Poisson$r , kfEst[,i] )

X.notPoisson = ppp( c(runif(50,0.05,0.5),runif(50,0.65,1.0)) , 
                    c(runif(50,0.0,0.4),runif(50,0.7,0.9)) , c(0,1) , c(0,1) ,
                    marks=rep(1,100) )
kfEst.notPoisson = Kest( X.notPoisson , correction="border" )
kfr = kfEst.notPoisson$r
kfborder = kfEst.notPoisson$border
lines( kfr , kfborder , col="blue" , lwd=2 )

plot( kfr , kfborder )
kf.notPoisson.env = envelope( X.notPoisson , Kest , 
                              correction="border" )
# windows( 12 , 9 )
plot( kf.notPoisson.env )

## Bramblecane data

kf.env = envelope( bramblecanes , Kest , correction="border" )
# windows( 7 , 6 )
plot( kf.env )

## Back to lecture notes
##
## Omnibus test.  H0: CSR  vs  H1: not CSR

mad.test( bramblecanes , Kest , verbose=FALSE )
dclf.test( bramblecanes , Kest , verbose=FALSE )

## The L function

lf.env = envelope( bramblecanes , Lest , correction="border" )
# windows( 7 , 6 )
plot( lf.env )

mad.test( bramblecanes , Lest , verbose=TRUE )
dclf.test( bramblecanes , Lest , verbose=FALSE )

## The G function

g.env = envelope( bramblecanes , Gest , correction="border" )
# windows( 7 , 6 )
plot( g.env )

##############################################
## Evenly spaced data
##############################################
x = rep(0,100)
y = rep(0,100)
k = 1
for (i in 1:10)
{
  for(j in 1:10)
  {
    x[k] = runif( 1 , (i-1)/10 , i/10 )
    y[k] = runif( 1 , (j-1)/10 , j/10 )
    k = k + 1
  }
}
##  windows( 9 , 9 )
##  plot( x , y , pch=19 , col="red" )

data.evenlyspaced = ppp( x , y , c(0,1) , c(0,1) )
Kest.evenlyspaced = Kest( data.evenlyspaced , correction="border" )
dclftest = dclf.test( data.evenlyspaced , Kest , verbose=FALSE )
pValue.evenlyspaced = dclftest$p.value
# windows( 9 , 5 )
par( mfrow=c(1,2) )
plot( x , y , pch=19 , col="red" )
plot( Kest.evenlyspaced )
text( 0.14 , 0.03 , cex=0.7 , adj=c(0,0) , 
      paste("P-Value for CSR with DCLF\n test on K function =" ,
            as.character( round(pValue.evenlyspaced,3)) ) )

# windows( 9 , 5 )
Lest.evenlyspaced = Lest( data.evenlyspaced , correction="border" )
dclftest = dclf.test( data.evenlyspaced , Lest , verbose=FALSE )
pValue.evenlyspaced = dclftest$p.value
par( mfrow=c(1,2) )
plot( x , y , pch=19 , col="red" )
plot( Lest.evenlyspaced )
text( 0.14 , 0.03 , cex=0.7 , adj=c(0,0) , 
      paste("P-Value for CSR with DCLF\n test on L function =" ,
            as.character( round(pValue.evenlyspaced,3)) ) )

# windows( 9 , 5 )
Gest.evenlyspaced = Gest( data.evenlyspaced , correction="border" )
dclftest = dclf.test( data.evenlyspaced , Gest , verbose=FALSE )
pValue.evenlyspaced = dclftest$p.value
par( mfrow=c(1,2) )
plot( x , y , pch=19 , col="red" )
plot( Gest.evenlyspaced )
text( 0.05 , 0.16 , cex=0.7 , adj=c(0,0) , 
      paste("P-Value for CSR with DCLF\n test on G function =" ,
            as.character( round(pValue.evenlyspaced,3)) ) )

##############################################
## Highly Clustered
##############################################
set.seed(1)
x = rep(0,100)
y = rep(0,100)
x0 = runif(20,0.1,0.9)
y0 = runif(20,0.1,0.9)
for( i in 1:100)
{
  x[i] = x0[ 1+floor(i/5) ] + runif(1,-0.02,0.02)
  y[i] = y0[ 1+floor(i/5) ] + runif(1,-0.02,0.02)
}
# windows( 9 , 9 )
plot( x , y , pch=19 , col="red" )

data.highlyclustered = ppp(x, y, c(0,1),c(0,1))
Kest.highlyclustered = Kest( data.highlyclustered , correction="border" )
dclftest = dclf.test( highlyclustered , Kest , verbose=FALSE )
pValue.highlyclustered = dclftest$p.value
# windows( 9 , 5 )
par( mfrow=c(1,2) )
plot( x , y , pch=19 , col="red" )
plot( Kest.highlyclustered )
text( 0.14 , 0.03 , cex=0.7 , adj=c(0,0) , 
      paste("P-Value for CSR with DCLF\n test on K function =" ,
            as.character( round(pValue.highlyclustered,3)) ) )

# windows( 9 , 5 )
Lest.highlyclustered = Lest( data.highlyclustered , correction="border" )
dclftest = dclf.test( data.highlyclustered , Lest , verbose=FALSE )
pValue.highlyclustered = dclftest$p.value
par( mfrow=c(1,2) )
plot( x , y , pch=19 , col="red" )
plot( Lest.highlyclustered )
text( 0.14 , 0.03 , cex=0.7 , adj=c(0,0) , 
      paste("P-Value for CSR with DCLF\n test on L function =" ,
            as.character( round(pValue.highlyclustered,3)) ) )

# windows( 9 , 5 )
Gest.highlyclustered = Gest( data.highlyclustered , correction="border" )
dclftest = dclf.test( data.highlyclustered , Gest , verbose=FALSE )
pValue.highlyclustered = dclftest$p.value
par( mfrow=c(1,2) )
plot( x , y , pch=19 , col="red" )
plot( Gest.highlyclustered )
text( 0.01 , 0.16 , cex=0.7 , adj=c(0,0) , 
      paste("P-Value for CSR with DCLF\n test on G function =" ,
            as.character( round(pValue.highlyclustered,3)) ) )


#############################################
#############################################
##  Cross K Function
#############################################
#############################################
marks( bramblecanes )  ## must be a factor variable
bramble.K = Kcross( bramblecanes , i=0 , j=1 , correction="border" )
# windows( 9 , 9 )
plot( bramble.K )
bramble.L = Lcross( bramblecanes , i=0 , j=1 , correction="border" )
plot( bramble.L )

bramble.Lalt = Lcross( bramblecanes , i=1 , j=0 , correction="border" )
plot( bramble.Lalt )

