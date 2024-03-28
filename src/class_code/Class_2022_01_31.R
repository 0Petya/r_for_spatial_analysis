#----------------------------------------------------------------------------
#-
#-  Code from Textbook: Intro to R for Spatial Analysis and Mapping, 2nd Ed.
#-
#-  Chapter 2: Data and Plots (Part 3)
#-
#-  Be sure to run the previous files first to define all variables (Class_2022_01_26.R)
#-
#----------------------------------------------------------------------------

##  Colors

# windows( 9 , 9 )
dev.off()
plot( Bulloch , asp=1 , type="n" , xlab="Easting", ylab="Northing" )
polygon( Bulloch , col=rgb(0.8,0.0,0.5,0.1) )
points( x=10000*runif(500,130,136) , y = 10000*runif(500,111,117) , 
        pch=16 , col="brown" )

# windows( 9 , 9 )
plot( Bulloch , asp=1 ,type="n" , xlab="Easting", ylab="Northing" )
polygon( Bulloch , col="#B3B333" )
text( 1330000 , 1140000 , "Bulloch County" , cex=2 )
text( 1330000 , 1135000 , "Georgia" , col="#0000FF" )

# locator()

# windows( 9 , 9 )
plot( c(-1.5,1.5) , c(-1.5,1.5) , asp=1 , type="n" )
rect( -0.5 , -0.5 , 0.5 , 0.5 , border=NA , col=rgb(0,0.5,0.5) )
rect( 0 , 0 , 1, 1 , col=rgb(1,0.5,0.5,0.8) )
##
##  Add a fourth argument (transparency) to rgb

##  Discuss raster graphic vs. vector graphic files
##

data( meuse.grid )
str( meuse.grid )

# windows( 9 , 9 )
plot( meuse.grid$x , meuse.grid$y , pch=19 , col="#FF0000", cex=0.5 )

mat = SpatialPixelsDataFrame( points=meuse.grid[c("x","y")] , data=meuse.grid )
str( mat )

# windows( 9 , 7 )
par( mfrow=c(1,2) , mar=c(0,0,0,0) )
image( mat , "dist" )
image( mat , "soil" )

library( RColorBrewer )
##  https://rdrr.io/cran/RColorBrewer/man/ColorBrewer.html

greenpal = brewer.pal( 7 , "Greens" )
bluepal = brewer.pal( 7 , "Blues" )
yellowRed = brewer.pal( 7 , "YlOrRd" )
# windows( 12 , 7 )
par( mfrow=c(1,3) , mar=c(0,0,0,0) )
image( mat , "dist" )
image( mat , "dist" , col=rev(bluepal) )
image( mat , "dist" , col=yellowRed )

##
##  meuse is a 
data( meuse )
str( meuse )

# windows( 8 , 8 )
par( mfrow=c(1,1) )
image( mat , "dist" , col=rev(bluepal))
#locator()
points( meuse$x , meuse$y , cex = sqrt(meuse$zinc)/20 )

##
##  ggplot
# install.packages( "tidyverse" , dep=TRUE )
# install.packages( "ggplot2" , dep=TRUE )
library( ggplot2 )

set.seed( 123 )
x2 = seq( from=0 , to=2*pi , length=100 )
y2 = sin( x2 )
y2r = y2 + rnorm( length(y2) , 0 , sd=0.1 )

# windows( 9 , 7 )
qplot( x2 , y2r , col=I("darkred"),ylim=c(-1.2,1.2) ) 

qplot( x2 , y2r , col=I("darkred"),ylim=c(-1.2,1.2) ) +
  geom_line( aes( x2 , y2 ) , col="springgreen" , size=1.5 )
## book uses I("springgreen") and I(1.5) ... don't know why

qplot( x2 , y2r , col=I("springgreen"),ylim=c(-1.2,1.2) ) +
  geom_line( aes( x2 , y2 ) , col="yellow" , size=1.5 ) +
  theme( axis.text=element_text(size=20) ,
         axis.title=element_text(size=20,face="bold") ) +
  theme_dark()

Bulloch = georgia.polys[[16]]
str( Bulloch )
Bulloch.df = data.frame( Bulloch )
str( Bulloch.df )
colnames( Bulloch.df ) = c("X","Y")
str( Bulloch.df )

library( gridExtra )

# windows( 12 , 7 )
p1 = qplot( X , Y , data=Bulloch.df , geom="polygon" , asp=1 ,
            col=I("black") , fill=I(rgb(0,0.7,0.5,0.4)) )
random.df = data.frame( x = 10000*runif(500,130,136) , 
                        y = 10000*runif(500,111,117) )
p2 = ggplot( Bulloch.df , aes( x=X , y=Y) ) +
  geom_polygon( fill=I(rgb(0,0.7,0.5,0.4)) ) +
  geom_point( data=random.df , aes(x,y) ,col=I("goldenrod") , pch=1 ) +
  coord_fixed() +
  theme( axis.text=element_text(size=12) ,
         axis.title=element_text(size=20) )

grid.arrange( p1 , p2 , ncol=2 )

library( dplyr )
class( georgia )

georgia.df = data.frame( georgia )
georgia.tb = as_tibble( georgia.df )   ## Note typo in book  as.tibble

georgia.tb
georgia.tb$Rural = as.factor( ( georgia.tb$PctRural > 50 ) + 0 )
## creates a new variable in georgia.tb

georgia.tb$IncClass = rep( "Average" , nrow(georgia.tb) )
georgia.tb$IncClass[ georgia.tb$MedInc >= 41204 ]= "Rich"
georgia.tb$IncClass[ georgia.tb$MedInc <= 29773 ]= "Poor"

table( georgia.tb$IncClass )

# windows( 9 , 7 )
ggplot( data=georgia.tb , mapping=aes( x=PctBach, y=PctEld ) ) +
  geom_point()

ggplot( data=georgia.tb , 
        mapping=aes( x=PctBach, y=PctEld , color=Rural ) ) +
  geom_point()

ggplot( data=georgia.tb ,
        mapping=aes( x=PctBach , y=PctEld , color=IncClass ) ) +
  geom_point()

ggplot( data=georgia.tb ,
        mapping=aes( x=PctBach , y=PctEld ) ) +
  geom_point() +
  geom_smooth( method="lm" ) +
  theme_bw() +
  xlab("% of Population with Bachelor's Degree") +  ## Note apostrophe
  ylab("% of Population who are Elderly")   ## Note "who" not "that"
## Change method="lm" to method="loess"  

ggplot( georgia.tb , aes( x=MedInc ) ) +
  geom_histogram( binwidth=5000 , color="red" , fill="gray" )

ggplot( georgia.tb , aes( x=MedInc) ) + 
  geom_histogram( aes( y=..density.. ) ,
                  binwidth=5000 , col="cyan" , fill="yellow" ) +
  geom_density( alpha=0.4 , fill="darksalmon" ) +
  geom_vline( aes(xintercept=median(MedInc,na.rm=TRUE)) ,
              col="goldenrod1" , linetype="dashed" , size=2 )

ggplot( georgia.tb , aes( x=PctBach , fill=IncClass ) ) +
  geom_histogram( color="gray40" , binwidth=1 ) +
  scale_fill_manual( "Income Class" , 
                     values = c("orange","goldenrod3","firebrick2") ) +
  facet_grid( IncClass ~ . ) +
  xlab("% Bachelor's Degree") +
  ggtitle("Bachelor's Degree in Different Income Classes")
  
## Which county is the far right outlier in Average Income
which( georgia.tb$PctBach > 35 )
georgia.tb$Name[35]

ggplot( georgia.tb , aes( x=IncClass , y=PctBach , fill=factor(Rural) ) ) +
  geom_boxplot()
