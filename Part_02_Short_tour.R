#
#  SCS 2011: Statistical Analysis and Programming with R
#  September-October 2011
#  
#  Short tour of R
#  Borrowing from Venables and Ripley (2002) Modern Applied Statistics with S, 4th ed., Springer
#  http://www.stats.ox.ac.uk/pub/MASS4/
#
#
# rm(m)

2 + 3
sqrt(3/4)/(1/3 + 2/pi^2)
factorial(5)
sqrt(-1)       # square root of -1 (as a real number)
sqrt(-1+0i)    # square root of -1 (as a complex number)

1/0
0/0
Inf/Inf
Inf + Inf
Inf - Inf
TRUE
TRUE + 2
FALSE
FALSE + 2


library(MASS)      # for special functions and data sets in MASS
library(lattice)   # for graphics -- I almost always use this -- automatic load via spida.beta
help.start()   # easy way to get overall help for packages
library(spida.beta)
library(p3d.beta)


chem      # a dataset in MASS
?chem     # help
mean
?mean
mean(chem)
m
m <- mean(chem)
v <- var(chem)/length(chem)
m/sqrt(v)
pnorm
pt(m/sqrt(v),length(chem) -1)
(1-pt(m/sqrt(v),length(chem) -1))*2    # p - value

# How's it's done with linear models

z <- lm(chem ~ 1)
unclass(z)
summary(z)

# writing a function

std.dev <- function(x) sqrt(var(x))
std.dev( chem)
std.dev( c(1,2,3,4,3,4,5,6,67))
t.test.p <- function(x, mu=0) {
	n <- length(x)
	t <- sqrt(n) * ( mean(x) - mu ) / std.dev(x)
	2 * (1 - pt(abs(t), n - 1))
}

t.test.p(chem)

# t value and p value:

t.stat <- function(x, mu=0) {
	n <- length(x)
	t <- sqrt(n) * (mean(x) - mu) / std.dev(x)
	list(t = t, p = 2 * (1 - pt(abs(t), n - 1)))
}

t.stat(chem)

z <- rnorm(300, 1, 2)  # generate 300 N(1, 4) # simulation
densityplot(z)
t.stat(z)

unlist(t.stat(z))


# Lattice graphics

x <- rnorm(1000)

y <- rnorm(1000)

truehist(c(x,y+3), nbins = 25)

?truehist

plot(x, y)
dd <- con2tr(kde2d(x,y)) # create a data frame with densities

?kde2d
?con2tr


contourplot(z ~ x + y, data=dd, aspect=1)
wireframe(z ~ x + y , data=dd, drape=T)
levelplot(z ~ x + y , data=dd, aspect=1)

######################################################

x <- seq(1, 20, 0.5)
x
w <- 1 + x/2  # adding a scalar to a vector
y <- x + w*rnorm(x)

?rnorm

# creating a data frame

dum <- data.frame( x, y, w)
dum
rm (x,y,w)   # remove orginal x,y,w in working directory

fm <- lm( y ~ x , data=dum)
summary(fm)

fm1 <- lm( y ~ x , data=dum, weight= 1/w^2)
summary(fm1)

lrf <- loess( y ~ x, dum)      # smooth regression

with(dum, plot(x,y))							# standard scatterplot

with(dum, lines(spline(x,fitted(lrf)),col=2))		# add fit with spline interp.

abline(0,1,lty=3,col=3)			# "true" line

abline(fm, col=4)					# unweighted regression

abline(fm1, lty=4, col=5)		# weighted regression

plot(fitted(fm), resid(fm),
	xlab = "Fitted values",
	ylab = "Residuals")
	
qqnorm(resid(fm))
qqline(resid(fm))

rm(fm,fm1,lrf,dum)


#=================================================================
#
#  Statistical experimentation:
#  - the effect of outliers

plot(c(0,1),c(0,1),type="n")

xy <- locator(type='p')     # click on plot, include an outlier, end with ESC

xy

abline(lm(y ~ x, xy), col=4)
abline(rlm(y ~ x, xy, method="MM"), lty=3, col=2)

rm(xy)


#==================================================================
#
#  Interactive identification of points
#

head(Smoking)
with( Smoking, plot(CigCon, LE))     # 'with' tells R where to find the variables CigCon, LE
?identify
with( Smoking, identify(CigCon, LE, labels = Country))  # bug in RStudio, labels don't show up until end
abline(lm(LE ~ CigCon, Smoking))     # we don't need with because 'lm' uses a formula interface
with( Smoking, lines( supsmu(CigCon, LE ), col = 'blue', lwd =2))   # non-parametric fit
with( Smoking, lines( supsmu(CigCon, LE, bass = 5 ), col = 'red', lwd =2))   # non-parametric fit

lines(loess(LE ~ CigCon, Smoking))
with( Smoking, lines(supsmu(Ci))

td( pch = 16, cex = 3)
xyplot( LE ~ CigCon, Smoking, groups = Cont, auto.key = T)

#===================================================================
# 
#  Michelson Morley experiments on speed of light
#  - "Speed" is speed in km/sec - 299,000  ("true" is 734.5)
#

attach(michelson)
summary(michelson)
search()

plot.factor(Expt,Speed,
	main="Speed of light data",xlab="Experiment No.")

abline(h=734.5)

fm <- aov(Speed ~ Run + Expt)
summary(fm)


fm0 <- update(fm, . ~ . - Run)
summary(fm0)
anova(fm0,fm)

detach()
rm(fm, fm0)

#===================================================================
# 
#  3D interactive plots
#  Illustrating multiple regression, interaction, 
#    and the estimation of conditional effects
#

library(p3d.beta)
Init3d( family = 'serif',cex=1.2)
some( Smoking )
Plot3d( LE ~ CigCon + HealthExpPC | Continent, Smoking ,fov= 0, phi=0)

Id3d(pad=1)

Plot3d( LE ~ CigCon + HealthExpPC | Continent, subset(Smoking, Continent != "Africa") ,fov= 0, phi=0)

Id3d(pad=1)
Axes3d()
Plot3d( LE ~ CigCon + HealthExpPC | Continent, Smoking ,fov= 0, phi=0)
Id3d(pad=1)
fitm <- lm( LE ~ CigCon , Smoking)
summary( fitm )
Fit3d(fitm)

fit <- lm( LE ~ CigCon + HealthExpPC, Smoking)
summary(fit)
Fit3d( fit, col = 'green')
Pop3d(2)

# changing the model for the controlling variable changes the picture considerably
fit2 <- lm( LE ~ CigCon + HealthExpPC + log(HealthExpPC), Smoking)
summary(fit2)
Fit3d( fit2, col ='red')

fit3 <- lm( LE ~ CigCon * (HealthExpPC + log(HealthExpPC)), Smoking)
summary(fit3)
Fit3d( fit3, col ='blue')
library(spida.beta)
wald(fit3)

# Linear hypothesis matrix and test of effect of CigCon | HE 
# How: differentiate model terms wrt Smoking

Lm <- list( "Change in LE assoc. with 1 unit change in CC" =   # creates a list of objects
  rbind(                                       # 'binds' rows together into a matrix
  'HE =100'   = c(0,1,0,0,100,log(100)),
  'HE = 1000' = c(0,1,0,0,1000,log(1000)),     # take the derivative wrt CigCon
  'HE = 3000' = c(0,1,0,0,3000,log(3000)),
  
  'HE = 5000' = c(0,1,0,0,5000,log(5000))))

Lm

wald(fit3, Lm)

# Linear hypothesis matrix and test of effect of CigCon | HE 
# How: differentiate 

wald(fit3)

Lm2 <- list("Change is LE assoc. with $1 increase in HealthExpPC" =
  rbind( 
  'HE = 100, CG = 1000'  = c(0,0,1,1/100,1000,1000/100),   # take the derivative wrt HE
  'HE = 3000, CG = 1000' = c(0,0,1,1/3000,1000,1000/3000),
  'HE = 6000, CG = 1000' = c(0,0,1,1/6000,1000,1000/6000)))

Lm2

wald(fit3, Lm2)
      
      


      
 
