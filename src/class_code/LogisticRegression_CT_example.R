setwd("C:/Users/srigd/Dropbox/MyCourses/BST_5600_Spring2022")

library( INLA )
library( tigris )
library( spdep )

CT_data = read.csv("CT-test-data.csv")
n = nrow( CT_data )
CT_data$idarea = rep( NA , n )
  
CT_data$idarea = 1*(CT_data$County == "Fairfield") +
                 2*(CT_data$County == "Hartford") +
                 3*(CT_data$County == "Litchfield") +
                 4*(CT_data$County == "Middlesex") +
                 5*(CT_data$County == "NewHaven") +
                 6*(CT_data$County == "NewLondon") +
                 7*(CT_data$County == "Tolland") +
                 8*(CT_data$County == "Windham") 

## CT = counties( "CT" )
class( CT )
CT.nb = poly2nb( CT )
nb2INLA( "mapCT.adj" , CT.nb )
g = inla.read.graph( filename="mapCT.adj" )

formula1 = Intend ~ Age + f( idarea , model="besag" , graph=g )

mod1 = inla( formula1 , family="binomial" , data=CT_data )
summary( mod1 )
mod1$marginals.fixed[[2]]
plot( mod1$marginals.fixed[[2]] , type="l" )

####################################################################
####
####    Model
####
####    Y_i ~ BIN( 1 , exp( beta0 + beta1*Age + u_i ) )
####  
####    u_i ~ CARnormal( 0 , sigma^2_u ),    prec_u = 1/sigma^2_u
####
###################################################################

