#
#  SCS 2011: Statistical Analysis and Programming with R
#  September-October 2011
#  
#  Linear Models in R
#  -- much of the code is borrowed from Fox,
#     Introduction to the R Statistical Computing Environment
#     ICPSR Summer Program, 2010-2011          

"
== Install packages (if needed) ==
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
  Prestige[ order(Prestige$women), ]  # ordering
  order(Prestige$women)     # why is works
 
# Tables: 
  with( Prestige, table(  type ) )        # using 'with' to refer to variable in a date frame 
                                          # note that 'table' drops NAs
  with( Prestige, table(  type , cut( women, 3)) )    # creating intervals from a numeric variable

  tab(Prestige,  ~ type + cut(women,3))   # 'tab' in spida.beta refers to variables with a formula
                                          # similarly to fitting functions: lm, etc.
  tab(Prestige,  ~ type + cut(women,3), pct = 2)  
  tab(Prestige,  ~ type + cut(women,3), pct = 2, useNA = 'no')  

# more plots
  scatterplotMatrix( Prestige )   # fancy from 'car'
  pairs( Prestige )                 # old
  splom( Prestige )                 # newer

  Init3d()
  Plot3d( income ~ education + women | type, Prestige)
  Axes3d()
  Id3d()
  # why a linear (in x and y) model won't work
  fit.lin <- lm( income ~ education + women, Prestige)
  str(fit.lin)
  fit.lin
  Fit3d( fit.lin )
  Pop3d(2)
  colors()
  Fit3d( fit.lin , col = 'hotpink', alpha = .6)
  pal(colors())
  pal(grep("pink",colors(), value = T))  # global regular expression print (from old unix)

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
"

"
<math>\hat{\beta}</math>:
"
  coef( income.mod )
"
<math>\hat{\Var}(\hat{\beta})</math>:
"

  vcov( income.mod )


"
=== Factors in regression ===
"     
  # 
  Prestige$incomes <- sqrt( Prestige$income )

  prestige.add <- lm( prestige ~  incomes + type, Prestige)
  summary(prestige.add)
  
  # regular graphics
"
==== Basic plotting ====
"

  plot( prestige ~ incomes, Prestige, col = type)
  plot( prestige ~ incomes, Prestige, col = type, pch = 16)
  plot( prestige ~ incomes, Prestige, col = type, cex = 1.5, lwd = 2)
  plot( prestige ~ incomes, Prestige, col = c('red','blue','magenta')[type], cex = 1.5, lwd = 2)
  plot( prestige ~ incomes, Prestige, col = type, cex = 1.5, lwd = 2, axes = FALSE)
  axis(1, at = seq(20,160,by=20), labels = seq(20,160,by=20)^2)
  axis(2, at = seq(20,80,by=20))
  box()
  abline( h = seq(20,80,20), lty = 2, col ='gray')
  abline( v = seq(20,160,20), lty = 2, col ='gray')
 
" 
==== Note on factors ====
* R's way of representing categorical data for analysis
* reading a data frame automatically turns a character variable into a factor
* example: type
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
  letters[f.ex.ro]
  outer( f.ex, f.ex.ro, "==")   # applies function '==' to all pairs     
  z <- outer(f.ex, f.ex.ro, "==")   # 
  dimnames(z) <- list(f.ex, f.ex.ro)
  z                                 # shows that == is applied to levels, not codes  
"
==== Quick programs in R ====
* It's easy to turn a good idea into a function
"

# turn a good idea into a funtion:
  out     # make sure it is not already used
  out <- function( x, y, FUN ){
      ret <- outer( x, y, FUN)
      dimnames( ret ) <- list( x, y)
      ret     # value returned by a function is 'exiting' line
  }
  out
  out( f.ex, f.ex.ro, `==`)                              # uses levels, not codes
  out( f.ex, f.ex.ro, `<`)                               # < not meaningful for factors
  out( as.character(f.ex), as.character(f.ex.ro), `<`)   # BUT it IS meaningful for characters!!
  # Useful for lots of stuff
  out( c(TRUE,FALSE,NA),c(TRUE,FALSE,NA), "|")        # 3-valued logic in R
  out( c(TRUE,FALSE,NA),c(TRUE,FALSE,NA), "&")        # 3-valued logic in R
  out( c(-Inf, -1, 0, 1, Inf, NA, NaN, 1i),c(-Inf, -1, 0, 1, Inf, NA, NaN,1i), "+"  )  # extended arithmetic
  out( c(-Inf, -1, 0, 1, Inf, NA, NaN, 1i),c(-Inf, -1, 0, 1, Inf, NA, NaN,1i), "*"  )  # extended arithmetic

"
==== Factors in regression ====
<math> Y = X \beta + \epsilon </math>
* A factor with k levels generates k-1 columns in the X matrix   
"
  model.matrix( prestige ~ incomes + type, Prestige)   # creates the X matrix
  z <- model.matrix( prestige ~ incomes + type, Prestige)  
  some(z)
  z$incomes    # ERROR because a matrix is not a data frame 
  z <- as.data.frame(z)     # turns matrix into a data frame  [Example of coercion]
  some( z )  
  z$incomes    # a data frame contains variables 
"
==== Merging data frames ====
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
==== Prediction data frames ====
"
  # prediction data frame

  # values for which we want to predict prestige
  pred <- expand.grid( type = levels(Prestige$type), incomes = seq(15,185,10))
  some( pred )  # all combination, good to use 'levels' to make sure in correct order
  pred$y <- predict( prestige.add, newdata = pred)
  some( pred )
"
==== For loop ====
"  
  for ( nn in levels(pred$type)) {
    lines( y ~ incomes, pred, subset = type == nn, col = type)
  }
"
==== lapply (better) ====
* <tt>lapply( list, FUN)</tt> applies function FUN to each element of list or vector
"
  lapply( levels(pred$type), function(x){
    lines( y ~ incomes, pred, subset = type == x, col = type, lwd =2)
  })
"
=== Linking numbers with pictures and answers with questions ===
* Most statistical output answers questions you don't want to ask and doesn't answer the questions you should ask
* Linking the numbers with the graphs is a ideal test of understanding
* Interpreting coefficients for factor indicators: comparisons with the reference level -- the level that doesn't appear
"
  summary( prestige.add )

  ################################  Show where the numbers appear on the graph
"
==== A colorful[sic] digression ====
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
=== Interaction with numeric variables ===
* additive model vs model with interaction
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
==== Exploring a curved regression function ====
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
*: main effects only estimate a '''conditional''' effect (sometimes called a ''specific'' or ''special'' effect)
*: '''NOT''' a general effect of the variable
*: when there is interaction, the conditional effect varies and should be explored and described
* Note that graphs often reveal important structure not at all visible through numerical output -- google(TM) Anscombe examples
"

"
==== 'Additive' curvature ====
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
=== Interaction with factors and numeric variables ===
* additive model vs model with interaction
"
#########################################################################################   RE{  # 
  Prestige$incomes <- sqrt( Prestige$income )

  prestige.add <- lm( prestige ~  incomes + type, Prestige)
  summary(prestige.add)
  
  # regular graphics
"
==== Basic plotting ====
"

  plot( prestige ~ incomes, Prestige, col = type)
  plot( prestige ~ incomes, Prestige, col = type, pch = 16)
  plot( prestige ~ incomes, Prestige, col = type, cex = 1.5, lwd = 2)
  plot( prestige ~ incomes, Prestige, col = c('red','blue','magenta')[type], cex = 1.5, lwd = 2)
  plot( prestige ~ incomes, Prestige, col = type, cex = 1.5, lwd = 2, axes = FALSE)
  axis(1, at = seq(20,160,by=20), labels = seq(20,160,by=20)^2)
  axis(2, at = seq(20,80,by=20))
  box()
  abline( h = seq(20,80,20), lty = 2, col ='gray')
  abline( v = seq(20,160,20), lty = 2, col ='gray')
 
" 
==== Note on factors ====
* R's way of representing categorical data for analysis
* reading a data frame automatically turns a character variable into a factor
* example: type
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
  letters[f.ex.ro]
  outer( f.ex, f.ex.ro, "==")   # applies function '==' to all pairs     
  z <- outer(f.ex, f.ex.ro, "==")   # 
  dimnames(z) <- list(f.ex, f.ex.ro)
  z                                 # shows that == is applied to levels, not codes  
"
==== Quick programs in R ====
* It's easy to turn a good idea into a function
"

# turn a good idea into a funtion:
  out     # make sure it is not already used
  out <- function( x, y, FUN ){
      ret <- outer( x, y, FUN)
      dimnames( ret ) <- list( x, y)
      ret     # value returned by a function is 'exiting' line
  }
  out
  out( f.ex, f.ex.ro, `==`)                              # uses levels, not codes
  out( f.ex, f.ex.ro, `<`)                               # < not meaningful for factors
  out( as.character(f.ex), as.character(f.ex.ro), `<`)   # BUT it IS meaningful for characters!!
  # Useful for lots of stuff
  out( c(TRUE,FALSE,NA),c(TRUE,FALSE,NA), "|")        # 3-valued logic in R
  out( c(TRUE,FALSE,NA),c(TRUE,FALSE,NA), "&")        # 3-valued logic in R
  out( c(-Inf, -1, 0, 1, Inf, NA, NaN, 1i),c(-Inf, -1, 0, 1, Inf, NA, NaN,1i), "+"  )  # extended arithmetic
  out( c(-Inf, -1, 0, 1, Inf, NA, NaN, 1i),c(-Inf, -1, 0, 1, Inf, NA, NaN,1i), "*"  )  # extended arithmetic

"
==== Factors in regression ====
<math> Y = X \beta + \epsilon </math>
* A factor with k levels generates k-1 columns in the X matrix   
"
  model.matrix( prestige ~ incomes + type, Prestige)   # creates the X matrix
  z <- model.matrix( prestige ~ incomes + type, Prestige)  
  some(z)
  z$incomes    # ERROR because a matrix is not a data frame 
  z <- as.data.frame(z)     # turns matrix into a data frame  [Example of coercion]
  some( z )  
  z$incomes    # a data frame contains variables 
"
==== Merging data frames ====
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
==== Prediction data frames ====
"
  # prediction data frame

  # values for which we want to predict prestige
  pred <- expand.grid( type = levels(Prestige$type), incomes = seq(15,185,10))
  some( pred )  # all combination, good to use 'levels' to make sure in correct order
  pred$y <- predict( prestige.add, newdata = pred)
  some( pred )
"
==== For loop ====
"  
  for ( nn in levels(pred$type)) {
    lines( y ~ incomes, pred, subset = type == nn, col = type)
  }
"
==== lapply (better) ====
* <tt>lapply( list, FUN)</tt> applies function FUN to each element of list or vector
"
  lapply( levels(pred$type), function(x){
    lines( y ~ incomes, pred, subset = type == x, col = type, lwd =2)
  })
"
=== Linking numbers with pictures and answers with questions ===
* Most statistical output answers questions you don't want to ask and doesn't answer the questions you should ask
* Linking the numbers with the graphs is a ideal test of understanding
* Interpreting coefficients for factor indicators: comparisons with the reference level -- the level that doesn't appear
"
  summary( prestige.add )

  ################################  Show where the numbers appear on the graph
"
==== A colorful[sic] digression ====
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
  
  # Exercise: explore other models}
  # 
  Prestige$incomes <- sqrt( Prestige$income )

  prestige.add <- lm( prestige ~  incomes + type, Prestige)
  summary(prestige.add)
  
  # regular graphics
"
==== Basic plotting ====
"

  plot( prestige ~ incomes, Prestige, col = type)
  plot( prestige ~ incomes, Prestige, col = type, pch = 16)
  plot( prestige ~ incomes, Prestige, col = type, cex = 1.5, lwd = 2)
  plot( prestige ~ incomes, Prestige, col = c('red','blue','magenta')[type], cex = 1.5, lwd = 2)
  plot( prestige ~ incomes, Prestige, col = type, cex = 1.5, lwd = 2, axes = FALSE)
  axis(1, at = seq(20,160,by=20), labels = seq(20,160,by=20)^2)
  axis(2, at = seq(20,80,by=20))
  box()
  abline( h = seq(20,80,20), lty = 2, col ='gray')
  abline( v = seq(20,160,20), lty = 2, col ='gray')
 
" 
==== Note on factors ====
* R's way of representing categorical data for analysis
* reading a data frame automatically turns a character variable into a factor
* example: type
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
  letters[f.ex.ro]
  outer( f.ex, f.ex.ro, "==")   # applies function '==' to all pairs     
  z <- outer(f.ex, f.ex.ro, "==")   # 
  dimnames(z) <- list(f.ex, f.ex.ro)
  z                                 # shows that == is applied to levels, not codes  
"
==== Quick programs in R ====
* It's easy to turn a good idea into a function
"

# turn a good idea into a funtion:
  out     # make sure it is not already used
  out <- function( x, y, FUN ){
      ret <- outer( x, y, FUN)
      dimnames( ret ) <- list( x, y)
      ret     # value returned by a function is 'exiting' line
  }
  out
  out( f.ex, f.ex.ro, `==`)                              # uses levels, not codes
  out( f.ex, f.ex.ro, `<`)                               # < not meaningful for factors
  out( as.character(f.ex), as.character(f.ex.ro), `<`)   # BUT it IS meaningful for characters!!
  # Useful for lots of stuff
  out( c(TRUE,FALSE,NA),c(TRUE,FALSE,NA), "|")        # 3-valued logic in R
  out( c(TRUE,FALSE,NA),c(TRUE,FALSE,NA), "&")        # 3-valued logic in R
  out( c(-Inf, -1, 0, 1, Inf, NA, NaN, 1i),c(-Inf, -1, 0, 1, Inf, NA, NaN,1i), "+"  )  # extended arithmetic
  out( c(-Inf, -1, 0, 1, Inf, NA, NaN, 1i),c(-Inf, -1, 0, 1, Inf, NA, NaN,1i), "*"  )  # extended arithmetic

"
==== Factors in regression ====
<math> Y = X \beta + \epsilon </math>
* A factor with k levels generates k-1 columns in the X matrix   
"
  model.matrix( prestige ~ incomes + type, Prestige)   # creates the X matrix
  z <- model.matrix( prestige ~ incomes + type, Prestige)  
  some(z)
  z$incomes    # ERROR because a matrix is not a data frame 
  z <- as.data.frame(z)     # turns matrix into a data frame  [Example of coercion]
  some( z )  
  z$incomes    # a data frame contains variables 
"
==== Merging data frames ====
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
==== Prediction data frames ====
"
  # prediction data frame

  # values for which we want to predict prestige
  pred <- expand.grid( type = levels(Prestige$type), incomes = seq(15,185,10))
  some( pred )  # all combination, good to use 'levels' to make sure in correct order
  pred$y <- predict( prestige.add, newdata = pred)
  some( pred )
"
==== For loop ====
"  
  for ( nn in levels(pred$type)) {
    lines( y ~ incomes, pred, subset = type == nn, col = type)
  }
"
==== lapply (better) ====
* <tt>lapply( list, FUN)</tt> applies function FUN to each element of list or vector
"
  lapply( levels(pred$type), function(x){
    lines( y ~ incomes, pred, subset = type == x, col = type, lwd =2)
  })
"
=== Linking numbers with pictures and answers with questions ===
* Most statistical output answers questions you don't want to ask and doesn't answer the questions you should ask
* Linking the numbers with the graphs is a ideal test of understanding
* Interpreting coefficients for factor indicators: comparisons with the reference level -- the level that doesn't appear
"
  summary( prestige.add )

  ################################  Show where the numbers appear on the graph
"
==== A colorful[sic] digression ====
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
              
#########################################################################################  END OF REPEAT
              
              
              
              
The summary graphics



#################################################################################

pred$prestige <- 
xyplot( prestige ~ incomes , Prestige, groups = type)
  xyplot( prestige ~ incomes , Prestige, groups = type)
  
wald( prestige.int, ":")
wald( prestige.int, "en:")

  Plot3d( income ~  sqrt(income)*log(income)|type, Prestige)  
  Fit3d( prestige.int )                                     
plot(income.mod)    
  
      # dummy regression
      
  Prestige$type # a factor
  class(Prestige$type) 
  str(Prestige$type) # structure
  
  sapply(Prestige, class)      # sapply applies the 'class' function to each variable in Prestige
  
  
  Prestige.2 <- na.omit(Prestige) # filter out missing data
  nrow(Prestige)
  nrow(Prestige.2)
  levels(Prestige.2$type)
  Prestige.2$type <- with(Prestige.2, factor(type, levels=c("bc", "wc", "prof"))) # reorder levels
  Prestige.2$type
  
          # generating contrasts from factors
          
  getOption("contrasts")
  contrasts(Prestige.2$type)
  model.matrix(~ type, data=Prestige.2)
  
  contrasts(Prestige.2$type) <- contr.treatment(levels(Prestige.2$type), base=2)  # changing baseline category
  contrasts(Prestige.2$type)
  
  contrasts(Prestige.2$type) <- "contr.helmert"  # Helmert contrasts
  contrasts(Prestige.2$type)
  
  contrasts(Prestige.2$type) <- "contr.sum"  # "deviation" contrasts
  contrasts(Prestige.2$type)
  
  contrasts(Prestige.2$type) <- NULL  # back to default
  
  Prestige.2$type.ord <- ordered(Prestige.2$type, levels=c("bc", "wc", "prof")) # ordered factor
  Prestige.2$type.ord
  round(contrasts(Prestige.2$type.ord), 3)   # orthogonal polynomial contrasts
  
  prestige.mod.1 <- lm(prestige ~ log2(income) + education + type, data=Prestige.2)
  summary(prestige.mod.1)
  
  anova(prestige.mod.1)   # sequential ("type-I") tests
  
  prestige.mod.0 <- lm(prestige ~ income + education, data=Prestige.2) # note: NA's filtered!
  summary(prestige.mod.0)
  
  prestige.mod.0 <- update(prestige.mod.1, . ~ . - type) # equivalent [in a formula '-' means remove]
  anova(prestige.mod.0, prestige.mod.1)   # incremental F-test
  
  Anova(prestige.mod.1)   # "type-II" tests
  
  prestige.mod.3 <- update(prestige.mod.1, 
      . ~ . + log2(income):type + education:type)     # adding interactions
  summary(prestige.mod.3)
  Anova(prestige.mod.3)
  
  lm(prestige ~ log2(income*type) + education*type, data=Prestige.2) # equivalent specifications
  lm(prestige ~ (log2(income) + education)*type, data=Prestige.2)
  
  
          # effect displays
      
  library(effects)
  plot(allEffects(prestige.mod.3), ask=FALSE)
  
      # Anova Models
  
  some(Moore)
  
  Moore$fcategory <- factor(Moore$fcategory, levels=c("low", "medium", "high"))
  Moore$partner.status <- relevel(Moore$partner.status, ref="low")
  
  xtabs(~ fcategory + partner.status, data=Moore)
  
  with(Moore, tapply(conformity,
      list(Authoritarianism=fcategory, "Partner's Status"=partner.status),
      mean))
  with(Moore, tapply(conformity,
      list(Authoritarianism=fcategory, "Partner's Status"=partner.status),
      sd))
  
              # graph of means:
              
  with(Moore, {
      interaction.plot(fcategory, partner.status, conformity, type="b",
          pch=c(1, 16), cex=2, ylim=range(conformity))
      points(jitter(as.numeric(fcategory), factor=0.5), conformity,
          pch=ifelse(partner.status == "low", "L", "H"))
      identify(fcategory, conformity)
  })
  
              # ANOVA tables
  
  contr <- options(contrasts=c("contr.sum", "contr.poly"))   # contr.sum = deviation contrasts
  moore.mod <- lm(conformity ~ fcategory*partner.status, data=Moore)
  summary(moore.mod)
  
  Anova(moore.mod)    # type II sums of squares
  Anova(moore.mod, type="III")    # type III sums of squares
  
  options(contr) # restore defaults
  
  
      # more on lm
  
  args(lm)
  
  some(Davis)
  lm(weight ~ repwt, data=Davis, subset=sex == "F")  # observation selection (women only)
  lm(weight ~ repwt, data=Davis, subset=1:100)
  
  lm(prestige ~ income + education, data=Duncan, subset=-c(6, 16))
  
  lm(conformity ~ partner.status*fcategory,  # specifying contrasts
      contrasts=list(partner.status=contr.sum, fcategory=contr.poly),
      data=Moore)
  
  lm(100*conformity/40 ~ partner.status*fcategory, data=Moore)  # data argument; note computation of y
  
  lm(prestige~I(income + education), data=Duncan)  # "protecting" expresssion on RHS of the model
  
  
  # Generalized linear models
      
      # binary logit model
      
  some(Mroz)
  
  mroz.mod <- glm(lfp ~ k5 + k618 + age + wc + hc + lwg + inc,
     data=Mroz, family=binomial)
  summary(mroz.mod)
  
  round(exp(cbind(Estimate=coef(mroz.mod), confint(mroz.mod))), 2) # odds ratios
      
  mroz.mod.2 <- update(mroz.mod, . ~ . - k5 - k618)
  anova(mroz.mod.2, mroz.mod, test="Chisq") # likelihood-ratio test
  
  Anova(mroz.mod)  # analysis-of-deviance table
  
  plot(allEffects(mroz.mod), ask=FALSE)
  
      # Poisson regression
  
  some(Ornstein)
  nrow(Ornstein)
  (tab <- xtabs(~interlocks, data=Ornstein))
  
  x <- as.numeric(names(tab)) # the names are the distinct values of interlocks
  plot(x, tab, type="h", xlab="Number of Interlocks", ylab="Frequency")
  points(x, tab, pch=16)
  
  mod.ornstein <- glm(interlocks ~ log2(assets) + nation + sector,
     family=poisson, data=Ornstein)
  summary(mod.ornstein)
  Anova(mod.ornstein)
  
          # quasi-Poisson model, allowing for overdispersion
          
  mod.ornstein.q <- update(mod.ornstein, family=quasipoisson)
  summary(mod.ornstein.q)
  
  plot(allEffects(mod.ornstein.q, default.levels=50), ask=FALSE)
  
  # repeated-measures ANOVA and MANOVA 
  
  some(OBrienKaiser)
  ?OBrienKaiser
  contrasts(OBrienKaiser$treatment)
  contrasts(OBrienKaiser$gender)
  
      # defining the within-subjects design
      
  phase <- factor(rep(c("pretest", "posttest", "followup"), c(5, 5, 5)),
      levels=c("pretest", "posttest", "followup"))
  hour <- ordered(rep(1:5, 3))
  idata <- data.frame(phase, hour)
  idata
  
      # fitting the multivariate linear model
      
  mod.ok <- lm(cbind(pre.1, pre.2, pre.3, pre.4, pre.5, 
                       post.1, post.2, post.3, post.4, post.5, 
                       fup.1, fup.2, fup.3, fup.4, fup.5) ~  treatment*gender, 
                  data=OBrienKaiser)
  mod.ok
      
      # multivariate and univariate tests
  
  (av.ok <- Anova(mod.ok, idata=idata, idesign=~phase*hour)) 
  
  summary(av.ok)
  
      # graphing the means
      
          # reshape the data from "wide" to "long"
      
  OBrien.long <- reshape(OBrienKaiser, 
      varying=c("pre.1", "pre.2", "pre.3", "pre.4", "pre.5", 
          "post.1", "post.2", "post.3", "post.4", "post.5", 
          "fup.1", "fup.2", "fup.3",  "fup.4", "fup.5"),
      v.names="score",
      timevar="phase.hour", direction="long")
  OBrien.long$phase <- ordered(c("pre", "post", "fup")[1 + ((OBrien.long$phase.hour - 1) %/% 5)], 
      levels=c("pre", "post", "fup"))
  OBrien.long$hour <- ordered(1 + ((OBrien.long$phase.hour - 1) %% 5))
  dim(OBrien.long)
  head(OBrien.long, 25) # first 25 rows
  
          # compute means
  
  Means <- as.data.frame(ftable(with(OBrien.long, 
      tapply(score, list(treatment=treatment, gender=gender, phase=phase, hour=hour), mean))))
  names(Means)[5] <- "score"
  dim(Means)
  head(Means, 25)
  
          # graph of means
          
  library(lattice)
  xyplot(score ~ hour | phase + treatment, groups=gender, type="b", 
      strip=function(...) strip.default(strip.names=c(TRUE, TRUE), ...), 
      ylab="Mean Score", data=Means, auto.key=list(title="Gender", cex.title=1))
         





###############

factor and numertical
estimate using stuff in Lab 3

###################
