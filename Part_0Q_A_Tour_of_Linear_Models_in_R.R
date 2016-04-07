Master version in http://scs.math.yorku.ca/index.php/A_Tour_of_Linear_Models_in_R

"
SCS 2011: Statistical Analysis and Programming with R
: September-October 2011
  
:  A Guided Tour of Linear Models in R
::     much of the code is borrowed from John Fox:
::     Introduction to the R Statistical Computing Environment
::     ICPSR Summer Program, 2010-2011          

== Install packages (if needed) ==
* See [[http://scs.math.yorku.ca/index.php/SCS_2011:_Statistical_Analysis_and_Programming_with_R#Installing_packages_on_a_Mac installing on a Mac]]
"
  install.packages(c('car','Hmisc','rgl')) 
  download.file("http://www.math.yorku.ca/people/georges/Files/R/spida.beta.zip", "spida.beta.zip")
  download.file("http://www.math.yorku.ca/people/georges/Files/R/p3d.beta.zip", "p3d.beta.zip")
  install.packages("spida.beta.zip", repos = NULL)
  install.packages("p3d.beta.zip", repos = NULL)
"
== Load packages ==
"        
    library(car)
    library(spida.beta)
    library(p3d.beta)    
"
== Fitting a linear regression model with numerical reponse ==
* R approach to data analysis: 
** use many small tools, not one big package: more flexible and adaptive, requires more knowledge
** integrate graphical and numerical exploration of data
* numerical and categorical predictors
* interaction
* asking questions: linear hypotheses
"

"
=== Looking at your data ===
"
  
  # The data:
  data(Prestige)  # optional: bring data from 'car' package to workspace
                  # this is a way of 'reading' a data set in a package
                  # later we will talk about reading other data sets
  xqplot( Prestige ) # quick look (from spida.beta)  
  ?Prestige          # 'help' on Prestige
  names(Prestige) 
  head(Prestige)     # first 6 lines
  tail(Prestige, 2)  # last 2 lines
  some(Prestige)     # 10 randomly sampled observations
  
  # Selecting subsets and indexing
  
  Prestige[ c(1,4), ]                     # Selecting rows
  Prestige[ c(1,4), c('type','women')]    #    ... rows and columns

  Prestige[ Prestige$women > 50, ]  # selects on rows
  Prestige[ Prestige$type == 'prof', c('type','income') ]  # selects on rows and columns
  zp <- Prestige[ order(Prestige$women), ]  # ordering
  row.names(zp)
  names(zp)
  zp$occupation <- row.names(zp)
  order(Prestige$women)     # why it works: permutation vector
 
  # Tables: 
  with( Prestige, table(  type ) )        # using 'with' to refer to variable in a date frame 
                                          # note that 'table' drops NAs
  with( Prestige, table(  type , cut( women, 3)) )    # creating intervals from a numeric variable

  tab(Prestige,  ~ type + cut(women,3))   # 'tab' in spida.beta refers to variables with a formula
                                          # similarly to fitting functions: lm, etc.
  tab(Prestige,  ~ type + cut(women,3), pct = 2)  
  tab(Prestige,  ~ type + cut(women,3), pct = 2, useNA = 'no')  
  ?tab
  # more plots
  scatterplotMatrix( Prestige )   # fancy from 'car'
  pairs( Prestige )                 # old
  splom( Prestige )                 # newer
  
  Init3d(cex = 1, font = 2)
  Plot3d( income ~ education + women | type, Prestige, col=c('blue','red','green3'))
  Axes3d()
  Id3d()
  # why a linear (in x and y) model won't work
  fit.lin <- lm( income ~ education + women, Prestige)
  fit.lin2 <- lm( income ~ education +I(education^2) + women+ I(women^2) + I(education * women), Prestige)
  str(fit.lin)
  fit.lin
  Fit3d( fit.lin )
  Pop3d(2)
  colors()
  Fit3d( fit.lin , col = 'hotpink', alpha = .6)
  Fit3d( fit.lin2 , col = 'black', alpha = .1)
  pal(colors())
  pal(grep("pink",colors(), value = T))  # global regular expression print (from old unix)
  pal(grep("red",colors(), value = T)) 
  pal(grepv("brown",colors()))          # grepv (in spida) same as grep( ..., value = T)
  pal(grepv("magenta|violet",colors()))  
  pal(grepv("yellow|brown",colors()))  
?regexp
"
=== Regression on numerical variables and interpretation ===
"

  # Regression with an additive numerical model (no interactions, all vars continuous )
  # Note: you can transform a variable on the fly
  
  income.mod <- lm( log(income) ~ education +  women,
      data = Prestige)
  # 
  summary(income.mod) # Interpretation of coefficients: Proportion change in y per unit change in x keeping other x constant      
              # multiply coef by 100 to get percentage change in y -- or use 100*log(y)  as dependent variable
  Plot3d( log(income) ~ education + women | type, Prestige)   # note: some curvature and negative outliers
  Fit3d( income.mod )
  Axes3d()
  Id3d()
  
  confint(income.mod) 
  
  wald(income.mod)    
  wald(income.mod, -1)  # removing first variable   

"
=== Diagnostics ===
"
  plot( income.mod )  # residuals vs fitted,  normal quantiles for resid, 
                      # scale location, residual vs leverage
  avPlots( income.mod )  # added variable plots

" 
=== Anova ===
"
  anova( income.mod )   # sequential tests -- type I SS
                        # Q: does each term 'add' to previous one in list
  Anova( income.mod )   # type II SS: does each term add to others
                        # excluding terms with that interact with the term
  Anova( income.mod, type = "III")  # does each term have an effect when added last

"
=== Getting information from a regression ===

* <math>\hat{\beta}</math>:
"
  coef( income.mod )
"
<math>\hat{Var}(\hat{\beta})</math>:
"
  vcov( income.mod )
"
== Factors in regression ==
"     
  # 
  Prestige$incomes <- sqrt( Prestige$income )
  
  prestige.add <- lm( prestige ~  incomes + type, Prestige)
  summary(prestige.add)
"
=== Basic plotting ===
"
Prestige$type
with( Prestige, type == 'prof')
with( Prestige, type == 1)
letters[c(1,5,2)]
letters[Prestige$type]

sapply( Prestige, class)
unclass(Prestige$type)
cols <- c('red','blue','green')
pchs <- c(15,16,17) 
  plot( prestige ~ incomes, Prestige, col = cols[type], pch = pchs[type])
  plot( prestige ~ incomes, Prestige, 
        col = type, pch = c(1,2,3)[type])
  plot( prestige ~ incomes, Prestige, col = type, cex = 1.5, lwd = 2)
  plot( prestige ~ incomes, Prestige, 
        col = c('red','blue','magenta')[type], cex = 1.5, lwd = 2)
  plot( prestige ~ incomes, Prestige, col = type, cex = 1.5, lwd = 2, axes = FALSE)
  axis(1, at = seq(20,160,by=20), labels = seq(20,160,by=20)^2)
  axis(2, at = seq(20,80,by=20))
  box()
  abline( h = seq(20,80,20), lty = 2, col ='gray')
  abline( v = seq(20,160,20), lty = 2, col ='gray')
 sampler()
" 
=== Note on factors ===
* R's way of representing categorical data for analysis
* reading a data frame automatically turns a character variable into a factor
* example: type in data frame Prestige
"
  Prestige$type
  str( Prestige$type )
  unclass( Prestige$type )  # raw internal representation
                  # internally it's integers
                  # but it prints as character
  as.character( Prestige$type )
  unclass( as.character( Prestige$type ))  # this is really a character var.
  
  # in some ways factors are numeric, in others character
  
  f.ex <- factor( c('One','Two','Three','Four','One','Two'))
  f.ex       # note that levels are in lexicographical (alpha) order by default
  unclass(f.ex)
  tab( f.ex)
  letters
  letters[f.ex]         # when indexing, f.ex acts as a number and uses its **codes**
  f.ex == "Three"       # in logical operations, as a character
  f.ex[1:2]             # when subsetting, it remembers its original **levels**
  f.ex[1:2, drop = T]   # unless you ask
  
  # reordering a factor
  f.ex.ro <- factor( f.ex, levels = c('One','Two','Three','Four')) 
  f.ex.ro
  tab( f.ex.ro)
  letters[f.ex.ro]
  outer( f.ex, f.ex.ro, "==")   # applies function '==' to all pairs     
  z <- outer(f.ex, f.ex.ro, "==")   # 
  dimnames(z) <- list(f.ex, f.ex.ro)
  z                                 # shows that == is applied to levels, not codes  
"
=== Quick programs in R ===
* It's easy to turn a good idea into a function
"
  out     # make sure it is not already used
  out <- function( x, y, zorg ){
      ret <- outer( x, y, zorg)
      dimnames( ret ) <- list( x, y)
      ret     # value returned by a function is 'exiting' line
  }
  out
plus <- function(a,b) {
  a + b
}
plus(1,5)
plus(Inf, Inf)
  out( f.ex, f.ex.ro, `==`)                              # uses levels, not codes
  out( f.ex, f.ex.ro, `<`)                               # < not meaningful for factors
  out( as.character(f.ex), as.character(f.ex.ro), `<`)   # BUT it IS meaningful for characters!!
  # Useful for lots of stuff
  out( c(TRUE,FALSE,NA),c(TRUE,FALSE,NA), "|")        # 3-valued logic in R
  out( c(TRUE,FALSE,NA),c(TRUE,FALSE,NA), "&")        # 3-valued logic in R
  out( c(-Inf, -1, 0, 1, Inf, NA, NaN, 1i),
       c(-Inf, -1, 0, 1, Inf, NA, NaN,1i), circle  )  # extended arithmetic
  out( c(-Inf, -1, 0, 1, Inf, NA, NaN, 1i),
       c(-Inf, -1, 0, 1, Inf, NA, NaN,1i), "*"  )  # extended arithmetic

"
=== Factors in regression ===
::<math> Y = X \beta + \epsilon </math>
* A factor with k levels generates k-1 columns in the X matrix   
"
  model.matrix( prestige ~ incomes + type, Prestige)   # creates the X matrix
  z <- model.matrix( prestige ~ incomes + type, Prestige)  
  some(z)
  z$incomes    # ERROR because a matrix is not a data frame 
 z[,'incomes'] 
z <- as.data.frame(z)     # turns matrix into a data frame  [Example of coercion]
  some( z )  
  z$incomes    # a data frame contains variables 
"
=== Merging data frames ===
"
  # merging two data frames:  
  
  z$id <- rownames(z)                      # create an id variable for indexing in z
  Prestige$id <- rownames(Prestige)        # corresponding id for Prestige
  zm <- merge( z, Prestige[,c('id','type')], by = 'id')  # merges on common var 'id'
  # note that the name of the variable here must be quoted!!!
  # Recall: Functions may required a variable to be referenced:
               # by name in quotes
               # by name without quotes
               # using a formula
               # sometimes more than one will work, often only one
               # Sorry!!! but good to know so it's easier to get out of dead ends
  some( zm )
  
  # Note dummy (indicator) variable for typeprof and typewc
"
=== Prediction data frames ===
"
  # prediction data frame
  summary(prestige.add)
  # values for which we want to predict prestige
  pred <- expand.grid( type = levels(Prestige$type), incomes = seq(15,185,10))
  some( pred )  # all combination, good to use 'levels' to make sure in correct order
  pred$y <- predict( prestige.add, newdata = pred)
  some( pred )
"
=== For loop ===
"  
  for ( nn in levels(pred$type)) {
    lines( y ~ incomes, pred, subset = type == nn, col = type)
  }
"
=== lapply (better) ===
* <tt>lapply( list, FUN)</tt> applies function FUN to each element of list or vector
"
sq <- function(x) x^2
sq(3)  
sq(1i)  
lapply( c(1,2,3,-4,-Inf), sq)
sapply( c(1,2,3,-4,-Inf), sq)



lapply( levels(pred$type), 
          function(nn){
    lines( y ~ incomes, pred, subset = type == nn, col = type, lwd =2)
  })
"
== Linking numbers with pictures and answers with questions ==
* Most statistical output answers questions you don't want to ask and doesn't answer the questions you should ask
* Linking the numbers with the graphs is a ideal test of understanding
* Interpreting coefficients for factor indicators: comparisons with the reference level -- the level that doesn't appear
"
  summary( prestige.add )
"
<pre>
Call:
lm(formula = prestige ~ incomes + type, data = Prestige)

Residuals:
     Min       1Q   Median       3Q      Max 
-17.5267  -5.0199  -0.5705   5.1203  24.5971 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 13.33319    3.44129   3.874 0.000198 ***
incomes      0.30836    0.04494   6.862 7.17e-10 ***
typeprof    23.68715    2.21896  10.675  < 2e-16 ***
typewc       7.37610    2.00784   3.674 0.000397 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Residual standard error: 7.794 on 94 degrees of freedom
  (4 observations deleted due to missingness)
Multiple R-squared: 0.7985,  Adjusted R-squared: 0.7921 
F-statistic: 124.2 on 3 and 94 DF,  p-value: < 2.2e-16 
</pre>
* Show where the numbers appear on the graph: [[File:Prestige.additive.png]]

=== A colorful[sic] digression ===
"
  # Easier in 3d
  # BUT: need at most two numerical predictors and one factor -- here only one numerical so add any other
  Plot3d( prestige ~ incomes + education | type, Prestige)  # note categorical predictor after "|"
  # changing colors:
  colors()
  
  pals()     # palettes of all colors
  pal(c('blue','skyblue','yellow4'))
  pal(grepv('blue',colors()))
  blues <- grepv( 'blue', colors())  
  length(blues)
  Blues <- list(blues[1:33], blues[-(1:33)])
  Blues
  lapply( Blues, pal)                                     # example lapply
  
  # choose some colors:
  
  Plot3d( prestige ~ incomes + education | type, Prestige,  # note categorical predictor after "|"  
      col = c('gray90','red','blue'))    
  prestige.add <- lm( prestige ~  incomes + type, Prestige)
  summary(prestige.add)
  Fit3d(prestige.add)
  Id3d()
  
  # Exercise: explore other models
"

== Interaction with numeric variables ==
* compare additive model vs model with interaction
"

  data(Ginzberg)
  head(Ginzberg)
  Plot3d( depression ~ fatalism + simplicity, Ginzberg, xlim= c(0,3), zlim=c(0,3))  
  fit.int <- lm( depression ~ fatalism * simplicity, Ginzberg)
  summary(fit.int)
  
  # Additive model:
  
  Fit3d( lm(depression ~ fatalism + simplicity, Ginzberg))               # additive
  some( model.matrix(depression ~ fatalism + simplicity, Ginzberg) )     # X matrix
    
  # Interaction model:

  Fit3d( fit.int <- lm(depression ~ fatalism * simplicity, Ginzberg), col = 'red')  # interaction
  some( model.matrix(depression ~ fatalism * simplicity, Ginzberg) )     # X matrix
  
  summary(fit.int)    # What do the coefficients mean?
  Axes3d()
"
=== Exploring a curved regression function ===
* Getting real answers to real questions
"
  # Forming a linear hypothesis matrix:
  
  L <- rbind( "Eff. of fatalism | simp = 0" = c( 0, 1, 0, 0),      #take derivative wrt fatalism, ":" means product
              "Eff. of fatalism | simp = 1" = c( 0, 1, 0, 1),    
              "Eff. of fatalism | simp = 3" = c( 0, 1, 0, 3),    
              "Eff. of simplicity | fatal = 0" = c( 0, 0, 1, 0),    
              "Eff. of simplicity | fatal = 3" = c( 0, 0, 1, 3)    
  ) 
  L            
  wald( fit.int, L )       #  compare with :
  summary( fit.int )
"
* Note how, when there is an interaction term (SIG. OR NOT): 
** main effects only estimate a '''conditional''' effect (sometimes called a ''specific'' or ''special'' effect)
** '''NOT''' a general effect of the variable
** when there is interaction, the conditional effect depends on values of other variables and should be explored and described
* Note that graphs often reveal important structure not at all visible through numerical output -- google(TM) Anscombe examples
=== 'Additive' curvature ===
"
  # Additive model with curvature                     
  
  Fit3d( lm(depression ~ fatalism + simplicity + I(simplicity^2), Ginzberg), col = 'green')  # interaction
  some( model.matrix(depression ~ fatalism + simplicity + I(simplicity^2), Ginzberg) )       # X matrix
    
  # Notes:
  # distinguish among:
  # 1. IV (independent variables)  -- NOT Statitistical Independent but 'functionally' independent
  # 2. regressor = column of X matrix 
  # 3. term = expression that generates columns of X matrix
"
== Interaction with factors and numeric variables ==
* additive model vs model with interaction
"
  Prestige$type # a factor
  class(Prestige$type) 
  str(Prestige$type) # structure
    
  sapply(Prestige, class)      # sapply applies the 'class' function to each variable in Prestige
    
    
  Prestige.2 <- na.omit(Prestige) # filter out missing data
  nrow(Prestige)
  nrow(Prestige.2)
  Prestige$type
  Prestige.2$type
  levels(Prestige.2$type)
  Prestige.2$type <- with(Prestige.2, factor(type, levels=c("bc", "wc", "prof"))) # reorder levels
  Prestige.2$type
  
  # generating contrasts from factors
            
  getOption("contrasts")
  contrasts(Prestige.2$type)      # what fills in the X matrix
  model.matrix(~ type, data=Prestige.2)
    
  contrasts(Prestige.2$type) <- contr.treatment(levels(Prestige.2$type), base=2)  # changing baseline category
  contrasts(Prestige.2$type)
    
  contrasts(Prestige.2$type) <- "contr.helmert"  # Helmert contrasts
  contrasts(Prestige.2$type)
    
  contrasts(Prestige.2$type) <- "contr.sum"  # "deviation" contrasts
  contrasts(Prestige.2$type)
  
  contrasts(Prestige.2$type) <- NULL  # back to default
  contrasts(Prestige.2$type)

  Prestige.2$type.ord <- ordered(Prestige.2$type, levels=c("bc", "wc", "prof")) # ordered factor
  Prestige.2$type.ord
  Prestige.2$type.ord > 'wc'
  Prestige.2$type.ord[ Prestige.2$type.ord > 'wc' ]
 
  round(contrasts(Prestige.2$type.ord), 3)   # orthogonal polynomial contrasts
  
  # Interaction
  
  prestige.add <- lm( prestige ~ incomes + type, data = Prestige)
  prestige.int <- lm( prestige ~ incomes * type, data = Prestige)
  
  summary(prestige.add)
  summary(prestige.int)
"
=== Interpreting main effects in presence of interactions ===
  Coefficients:
                   Estimate Std. Error t value Pr(>|t|)    
  (Intercept)      -5.29638    5.78530  -0.915 0.362328    
  incomes           0.56720    0.07892   7.187 1.70e-10 ***
  typeprof         54.14877    8.09419   6.690 1.71e-09 ***
  typewc           28.36126    9.99804   2.837 0.005607 ** 
  incomes:typeprof -0.37719    0.09624  -3.919 0.000171 ***
  incomes:typewc   -0.29257    0.13924  -2.101 0.038361 *  

*  What do the coefficients say?
*:    Effect of unit increase in sqrt(income) = 0.567 ??
*:    prof -  bc[reference level] = 54.14 ??
*:    wc -  bc  = 28.36 ??
* '''NO!'''
*:    Effect of unit increase in sqrt(income) = 0.567 WHEN type '=' 0, i.e. among 'bc'
*:    prof -  bc[reference level] = 54.14 WHEN sqrt(income) = 0
*:    wc -  bc  = 28.36 ?? WHEN sqrt(income) = 0
=== Asking a question: How strong is evidence of interaction? ===
* Consider fitting the model 
# with 'type' using 'bc' as the reference level
# with 'type' using 'wc' as the reference level
We '''should''' get the same answer!
"
  Prestige$type2 <- factor( Prestige$type, levels = c('wc','bc','prof'))
  prestige.int2 <- lm( prestige ~ incomes * type2, Prestige)
  summary( prestige.int2)
  
  summary( prestige.int)
"
* Why does one model look so much more significant than other?
*: although they are fitting exactly the same thing except for a change of coding?

* Answer:  The output for each model is only showing part of the information
*: Standard regression output rarely answers relevant questions
*: and what it does answer is often only partly relevant

* We need to ask: what happens if we drop interaction entirely?
* BUT there are two regressors for interaction
* Each alone might not be significant '''but''' together they might be highly significant

* Two ways to test:
*# Likelihood Ratio Test: Fit the model twice: once with interaction and once without and compare the two models
*# Wald Test: Use a linear hypothesis to test whether all relevant coefficients could be simultaneiously equal to 0

=== Likehood Ratio Test ===
  prestige.add2 <- lm( prestige ~ incomes + type2, Prestige)
  summary( prestige.add2 )
  
  anova(prestige.int2, prestige.add2)
  
  # what if we had used original coding for type?
  
  anova(prestige.int, prestige.add)
"
* '''we get exactly the same answer'''
* The LRT here is ''invariant'' wrt to coding of 'type
* It obeys a principle of invariance:
*: '''What should not matter, does not matter'''
=== Wald test ===
* does not require refitting
* test whether all terms with interaction (last two terms) can be set to 0
"
  summary(prestige.int)
  L <- rbind( c( 0,0,0,0,1,0), c(0,0,0,0,0,1)) 
  L
  wald( prestige.int, L)
  wald( prestige.int2, L)    # same L works

  # Easy wald tests using regular expressions
  wald( prestige.int, ":")     # selects every regressor whose name contains ':'
  
  # other ways:  'anova' uses terms with multiple regressors
  # Type I sequential:
  anova( prestige.int)    # sequential gives right answer BECAUSE interaction is last
  anova( prestige.int2)
  
  # Type II (by default)
  Anova( prestige.int)    # why are all the same ? but 'income' not the same as in Type I
  Anova( prestige.int2)
  
  Anova( prestige.int, type = "III")    # meaningless? except for interaction because it's last
  Anova( prestige.int2, type = "III")
"
'''RECAP''': 
* If a factor has more than two levels, overall tests must use all relevant terms simultaneously, not each one individually
* Beware of type III
=== Testing for 'type' in a model with interaction ===
Consider using either:
* Anova with type II
* OVerall: refitting without 'type' and using LRT
* Overall: using wald
"
  # Anova II
  Anova( prestige.int, type = 'II')
  
  # refit:
  
  prestige.inc <- lm( prestige ~ incomes , Prestige)
  anova( prestige.int, prestige.inc)   # ERROR: what happened?
"
=== Caution with LRT ===
* Need nested models: One X matrix is in a subspace of other X matrix
* Exactly the same cases
* Exactly the same y, e.g. can't compare y with log(y)
* If we drop a variable with NAs, then the new model might have more cases
"
  # Refitting with the same cases:
  
  model.frame( prestige.int)
  
  prestige.inc <- lm( prestige ~ incomes , model.frame(prestige.int))  # all 'type' terms deleted
  anova( prestige.int, prestige.inc)   # ERROR: what happened?
   
  # Wald (I find fastest, especially in consulting)
  wald( prestige.int, 'type') 
"
== Visualizing interaction ==
"
  prestige.int <- lm( prestige ~ incomes*type, Prestige)
  summary( prestige.int )
  # choosing colors
  levels(Prestige$type)
  cols <- c('blue','green','black')

  plot( prestige ~ incomes, Prestige, col = cols[type], 
        cex = 1.5, lwd = 2,
        xlim = c(0,170),
        ylim = c(-10,90))   # type used as index
  pred <- expand.grid( type = levels(Prestige$type), incomes = seq(-10,180)) 
  pred$prestige <- predict( prestige.int, newdata = pred)
  some(pred)
  lapply( levels(pred$type), function(x){
    lines( prestige ~ incomes, pred, subset = type == x, col = cols[type], lwd =2)   # get added to plot
  })

  summary(prestige.int)
"
<pre>
Call:
lm(formula = prestige ~ incomes * type, data = Prestige)

Residuals:
     Min       1Q   Median       3Q      Max 
-12.5747  -5.3053   0.2172   3.9447  24.6692 

Coefficients:
                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)      -5.29638    5.78530  -0.915 0.362328    
incomes           0.56720    0.07892   7.187 1.70e-10 ***
typeprof         54.14877    8.09419   6.690 1.71e-09 ***
typewc           28.36126    9.99804   2.837 0.005607 ** 
incomes:typeprof -0.37719    0.09624  -3.919 0.000171 ***
incomes:typewc   -0.29257    0.13924  -2.101 0.038361 *  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Residual standard error: 7.29 on 92 degrees of freedom
  (4 observations deleted due to missingness)
Multiple R-squared: 0.8275,	Adjusted R-squared: 0.8181 
F-statistic: 88.28 on 5 and 92 DF,  p-value: < 2.2e-16 
</pre>
Where are the numbers on the graph?
[[File:Georges.Prestige.interaction.png]]
"

=== Estimating gaps ===
"
  summary(prestige.int)
  abline( v = 0)
    
  L.gaps <- rbind( 
        "wc - bc | incomes = 100" = c( 0, 0, 0, 1,   0, 100),
      "prof = bc | incomes = 100" = c( 0, 0, 1, 0, 100,   0),
      "prof - wc | incomes = 100" = c( 0, 0, 1,-1, 100,-100),
        
        "wc - bc | incomes = 100" = c( 0, 0, 0, 1,   0,  50),
      "prof = bc | incomes = 100" = c( 0, 0, 1, 0,  50,   0),
      "prof - wc | incomes = 100" = c( 0, 0, 1,-1,  50,- 50)
        )
  
  wald( prestige.int, L.gaps )
  abline( v = c(50,100))
        
  # a puzzler:
  wald( prestige.int, 'type')

"
=== Visualizing a gap and its SE ===
"
  # graph the wc - bc gap from 0 to 180
  # 1. create a data frame with values that for which the gap varies and form the L matrix
  summary( prestige.int)
  
  gap.df <- expand.grid( incomes = seq(0,180))
  L.gap  <- with( gap.df,  cbind( 0, 0, 0, 1, 0, incomes ))  
  L.gap
  
  # 2. Use wald to generate estimated gaps, etc.
  
  ww <- wald( prestige.int, L.gap)
  ww
  ?wald
  
  # 3. Transform to a data frame and specify number of SEs for line above and below estimated gap
  
  ww.df <- as.data.frame( ww, se = 2)   # number of standard errors
  ww.df
  
  # 4. Combine with data frame under 1
  
  comb.df <- cbind( ww.df, gap.df)
  some(comb.df)      
  
  # 5. Plot the result   
     
  plot( coef ~ incomes, comb.df, type = 'l', lwd = 2, main = 'Estimated prestige gap: white collar - blue collar',
          xlab = list(expression(sqrt(income)), cex = 1.2),
          ylab = list(expression( hat(gap) %+-% 2 %*% SE  ),cex=1.2))        
  lines( U2 ~ incomes, comb.df, lty = 2, lwd = 2, col = 'red')      
  lines( L2 ~ incomes, comb.df, lty = 2, lwd = 2, col = 'red')      
  abline ( h = 0, col = 'blue')
  ?plotmath               # help on math symbols in plots
