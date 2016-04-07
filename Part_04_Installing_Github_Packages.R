#
#  SCS 2011: Statistical Analysis and Programming with R
#  September-October 2011
#  
#  Installing a few packages to start
#

  # Installing packages on CRAN

  install.packages(c('car','Hmisc','rgl')) 

  # Installing private packages from a file on the web  
  
  download.file("http://www.math.yorku.ca/people/georges/Files/R/spida.beta.zip", "spida.beta.zip")
  install.packages("spida.beta.zip", repos = NULL)

  download.file("http://www.math.yorku.ca/people/georges/Files/R/p3d.beta.zip", "p3d.beta.zip")
  install.packages("p3d.beta.zip", repos = NULL)

# Installation only needs to be done once when you start using R and whenever you 
# upgrade to a new version

  
## Each time you use R, load the packages with:
  
  library(spida.beta)  # note that car and lattice are loaded automatically
  library(p3d.beta)     # note that a number of other packages get loaded at the same time
