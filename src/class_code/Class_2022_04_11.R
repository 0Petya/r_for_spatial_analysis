#-----------------------------------------------------------------------
#-
#-  Bayes Theorem
#-
#-----------------------------------------------------------------------

#------------------
#-  Prior
#------------------
theta = seq( 0 , 1 , 0.01 )
prior = rep( 1/length(theta) , length(theta) )
sum( prior )

#------------------
#-  Likelihood
#------------------
n = 6
x = 5
likelihood = theta^x * (1-theta)^(n-x)

#------------------
#-  Posterior
#------------------
posterior = prior*likelihood / sum(prior*likelihood)

#------------------
#-  Plots
#------------------
# windows( 9 , 10 )
par( mfrow=c(2,1) )
plot( theta , prior , pch=19 , col="red" )
plot( theta , posterior , pch=19 , col="blue" )

#------------------
#-  Point Estimates
#------------------
postmean = sum( theta*posterior )
postmedian = theta[ min( which( cumsum( posterior ) > 0.50 ) ) ]
postmode = theta[ which.max( posterior ) ]
print( c( postmean , postmedian , postmode ) )

#-----------------------------------------------------------------------
#-
#-  Continuous Prior & Continuous Posterior
#-
#-----------------------------------------------------------------------

#------------------
#-  Prior
#------------------
theta = seq( 0 , 1 , length=1001 )
a = 5
b = 15
prior = dbeta( theta , a , b )

#------------------
#-  Likelihood
#------------------
n = 60
x = 50
likelihood = theta^x * (1-theta)^(n-x)

#------------------
#-  Posterior
#------------------
posterior = likelihood*prior / sum(likelihood*prior)

#------------------
#-  Plots
#------------------
# windows( 9 , 10 )
par( mfrow=c(2,1) )
plot( theta , prior , type="l" , col="red" , lwd=2 )
plot( theta , posterior , type="l" , col="blue" , lwd=2 )

#------------------
#-  Point Estimates
#-
#-  With a BETA(a,b) prior the posterior is BETA(a+x,b+n-x)
#-
#------------------

post.a = a + x
post.b = b + n - x
postmean = post.a / ( post.a + post.b )
# postmedian = theta[ min( which( cumsum( posterior ) > 0.50 ) ) ]
postmode = theta[ which.max( posterior ) ]
print( c( postmean , postmode ) )

#-------------------
#-
#-  "Confidence" Intervals (actually called "Credibility" Intervals)
#-
#-  Choose an interval for which the posterior probability is 0.95
#-
#-------------------

Lower.Cred.Limit = qbeta( 0.025 , post.a , post.b )
Upper.Cred.Limit = qbeta( 0.975 , post.a , post.b )

# windows( 9 , 10 )
par( mfrow=c(2,1) )
plot( theta , prior , type="l" , col="red" , lwd=2 )
plot( theta , posterior , type="l" , col="blue" , lwd=2 )
  theta.LCL = theta[ theta < Lower.Cred.Limit ] 
  theta.UCL = theta[ theta > Upper.Cred.Limit ]
  post.LCL = posterior[ theta < Lower.Cred.Limit ]
  post.UCL = posterior[ theta > Upper.Cred.Limit ]
  x = c( theta.LCL , rev(theta.LCL) , theta.LCL[1] )
  y = c( rep(0,length(post.LCL)) , rev(post.LCL) , 0 ) 
  polygon( x , y , col="gray" )         
  x = c( theta.UCL , rev(theta.UCL) , theta.UCL[1] )
  y = c( rep(0,length(post.UCL)) , rev(post.UCL) , 0 ) 
  polygon( x , y , col="gray" )    
  abline( h=0 , col="black" )
  
  
         