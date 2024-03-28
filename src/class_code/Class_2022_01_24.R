#----------------------------------------------------------------------------
#-
#-  Code from Textbook: Intro to R for Spatial Analysis and Mapping, 2nd Ed.
#-
#-  Chapter 2: Data and Plots
#-
#----------------------------------------------------------------------------

#   R as a calculator (with memory)

x = 4
y = 21
z = x + y
print( c(1,x,y,z) )

print( c(x+y,sqrt(z)) )

Y = -4
print( c(y,Y) )

tree.heights = c( 4.3 , 7.1 , 6.3 , 5.2 , 3.2 , 2.1 )
tree.heights**2
tree.heights^2
tree.heights[1]
tree.heights[6]
tree.heights[7]
tree.heights[7] = 4.1
tree.heights
tree.heights[10] = 5.5
tree.heights

sum( tree.heights )
mean( tree.heights )

tree.heights = c( 4.3 , 7.1 , 6.3 , 5.2 , 3.2 , 2.1 )
max.height = max( tree.heights ) 
max.height

name = "Steve Rigdon"
name
str( name )

##
##  structure and class
##

str( tree.heights )
class( tree.heights )
str( name )
class( name )

cities = c( "St. Louis" , 'Florissant' , "Glen Carbon" , "St.Charles" )
str( cities )
class( cities )

in.Illinois = c( FALSE , FALSE , TRUE , FALSE )
str( in.Illinois )
class( in.Illinois )

TRUE & FALSE
TRUE | FALSE

##
## 2.3 Data Types in R - See p. 17
##

is.logical( in.Illinois )
is.character( tree.heights )

##
##  conversion and coersion
##

y.char = as.character( Y )
as.logical( 0 )
as.logical( 1 )

0 | TRUE
1 & 1

TRUE + 4

y = seq( -1 , 1 , 0.05 )
y = seq( from=-1 , to=1 , by=0.05 )
y = seq( from=-1 , by=0.05 , to=1 )

x = 1:length(y)

my.df = data.frame( x , y )

my.matrix = matrix( x , nrow=10 , ncol=5 )
my.matrix
my.matrix1 = matrix( x , nrow=10 , ncol=5 , byrow=TRUE )
my.matrix1
my.matrix2 = matrix( c( 1 , 2 , 3 ,
                        4 , 5 , 6 ,
                        7 , 8 , 9 ) , nrow=3 , ncol=3 , byrow=TRUE )
my.matrix2

flow = matrix ( c(2000 , 1243 ,  543 ,
                  1243 ,  212 ,  545 ,
                   654 ,  168 ,  109 ) , nrow=3 , ncol=3 , byrow=TRUE )
colnames( flow ) = c( "St. Louis" , "Florissant" , "Glen Carbon" )
rownames( flow ) = c( "St. Louis" , "Florissant" , "Glen Carbon" )
outflows = rowSums( flow )
outflows

cities
z = c( 10 , 3 , 1 , 4 )
names( z ) = cities
z

##
##  Factors
##

house.type1 = c( "Bungalo" , "Flat" , "Flat" , "Detached" , "Flat" , 
                 "Terrace" , "Terrace" )

house.type = factor( c( "Bungalo" , "Flat" , "Flat" , "Detached" , 
                        "Flat" , "Terrace" , "Terrace" ) ,
                     levels = c( "Bungalo" , "Flat" , "Detached" , 
                                 "Semi" , "Terrace") )                
house.type2 = factor( c( "Bungalo" , "Flat" , "Flat" , "Detached" , 
                         "Flat" , "People Carrier" , "Terrace" ) ,
                      levels = c( "Bungalo" , "Flat" , "Detached" , 
                                  "Semi" , "Terrace") )
house.type2

##
##  Ordered Factors
##

income = factor( c( "High" , "High" , "Low" , "Low" , "Low" , "Medium" ,
                    "Low" , "Medium") , 
                 levels = c( "Low" , "Medium" , "High" ) )
income > "Low"
income = ordered( c( "High" , "High" , "Low" , "Low" , "Low" , "Medium" ,
                     "Low" , "Medium") , 
                  levels = c( "Low" , "Medium" , "High" ) )
income > "Low"

##
##  Lists
##

tmp.list = list( "Lex Comber" , c(2015,2018) , "Lecturer" , 
                 matrix( c(6,3,1,2) , nrow=2 , ncol=2 ) )
tmp.list
str( tmp.list )
class( tmp.list )
tmp.list[[4]]

employee = list( name="Lex Comber" , start.year=2015 , position="Professor" )
employee$position
employee$start.year

tmp.list1 = append( tmp.list , list( c(7,6,9,1) ) )
tmp.list1

lapply( employee , is.numeric )
lapply( tmp.list1 , length )

##
##  Defining your own functions
##
f = function(x,y)
{ 
  w = x + y
  return( w^2 )
}
f(1,1)
f(3,-5)

##
##  Defining your own classes 
##

employee = list( name="Lex Comber" , start.year=2015 , position="Professor" )
print( employee )
class( employee ) = "staff"
print.staff = function( x )
{
  cat( "Name: " , x$name , "\n" )
  cat( "Start Year: " , x$start.year , "\n" )
  cat( "Job Title: " , x$position , "\n" )
}
print( employee )
print( unclass( employee ) )

new.staff = function(name,year,post)
{
  result = list( name=name , start.year=year , position=post )
  class(result) = "staff"
  return( result )
}

leeds.uni = vector( mode="list" , 3 )
leeds.uni[[1]] = new.staff( "Heppenstall, Alison" , 2017 , "Professor" )
leeds.uni[[2]] = new.staff( "Comber, Lex" , 2015 , "Professor" )
leeds.uni[[3]] = new.staff( "Langlands, Alan" , 2014 , "VC" )
leeds.uni
str( leeds.uni )
str( leeds.uni[[1]] )
leeds.uni[[1]][[1]]

##  data frame vs. tibble

df = data.frame( dist=seq(0,400,100) ,
                 city=c("Leeds","Nottingham","Leicester","Durham","Newcastle") ,
                 stringsAsFactors = FALSE )
str(df)
df$city
df$ci   ##  ?!

library( dplyr )
tb = tibble( dist=seq(0,400,100) ,
             city=c("Leeds","Nottingham","Leicester","Durham","Newcastle") )

df[,2]
tb[,2]
str( df[,2] )
str( tb[,2] )

df[,1:2]
tb[,1:2]
str( df[,1:2] )
str( tb[,1:2] )

tb1 = as_tibble( df )
str( tb1 )

pop = c( 700 , 250 , 230 , 150 , 1200 )
df1 = cbind( df , pop )

tb1 = cbind( tb , pop )
tb1
str( tb1 )
