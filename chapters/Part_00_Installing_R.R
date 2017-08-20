#' ---
#' title:     "Notes on R for Statistics"
#' subtitle:  "Chapter 00 Installing R and RStudio"
#' date:      "August 2016"
#' author:    "Georges Monette"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: false
#'     theme: readable
#'     fig.width: 12
#'     fig.height: 10
#' bibliography: mm.bib
#' link-citations: yes
#' ---
#' Updated: `r format(Sys.time(), '%B %d %Y %H:%M')`
#' 
#+ include=FALSE
knitr::opts_chunk$set(comment="  ", error = TRUE)
if(.Platform$OS.type == 'windows') windowsFonts(Arial=windowsFont("TT Arial")) 
interactive <- FALSE  # do not run interactive code
#'
#' # Installing R, RStudio and Related Software
#'
#' R, RStudio, and Stan are all free, open-source software, available for all
#' commonly used operating systems, including Windows, macOS, and Linux systems.
#' R and RStudio install in the standard manner on each of these systems.
#' System-specific instructions for installing R are given below. Regardless of
#' your operating system, you should install R before installing RStudio.
#' 
#' Please read and follow these instructions carefully. 
#' 
#' ## Installing R on Windows
#'     
#' - Visit the [Comprehensive R Archive Network](http://cran.r-project.org/) (CRAN) and select a mirror site; 
#'   a list of [CRAN mirrors](http://cran.r-project.org/mirrors.html) appears at the upper left of the CRAN home page. 
#'   I suggest that you use the [0-Cloud mirror](https://cloud.r-project.org/), which is the first on the list. 
#'   Click on the link _Download R for Windows_, which appears near the top of the page; 
#'   then click on _install R for the first time_, and subsequently on _Download R x.y.z for Windows_ 
#'   (where x.y.z is the current version of R). 
#'   Once it is downloaded, double-click on the R installer. 
#'   You may take all of the defaults, but I suggest that you make the following modification:
#'
#' > Instead of installing R in the standard location, C:\Program Files\R\R-x.y.z, 
#'   I suggest that you use C:\R\R-x.y.z. Again, x.y.z is the current version of R. 
#'   This will allow you to install packages in the main R library without running 
#'   R with administrator privileges and may avoid problems that rarely occur when R 
#'   is installed in the Windows Program Files directory.  
#'     
#' ### Additional Software
#' 
#' -  To use Stan from R (via the **rstan** package), you must install Rtools, which includes a C++ compiler and various Unix tools that Stan requires: 

> Click on the _Rtools_ link on the _R for Windows_ CRAN page. Download the current version of the Rtools installer (Rtoolsxx.exe, where xx is the version number) and run it. You may take all of the other defaults, but do allow the Rtools installer to modify your system path, by checking the box to edit the system path.

![bullet](http://socserv.mcmaster.ca/jfox/Courses/R/York-R-course/blueball.gif) **Optional**:  To create **Sweave** and **knitr** LaTeX documents (more sophisticated alternatives to R Markdown) in RStudio, and to compile R Markdown documents directly to PDF files, download and install the [MiKTeX](https://miktex.org/) LaTeX system.

## Installing R on macOS

![bullet](http://socserv.mcmaster.ca/jfox/Courses/R/York-R-course/blueball.gif)Visit the [Comprehensive R Archive Network](http://cran.r-project.org/) (CRAN) and select a mirror site; a list of [CRAN mirrors](http://cran.r-project.org/mirrors.html) appears at the upper left of the CRAN home page. I suggest that you use the [0-Cloud mirror](https://cloud.r-project.org/), which is the first on the list. Click on the link _Download R for MacOS X_, which appears near the top of the page; then click on _R-x.y.z.pkg_ (where x.y.z is the current version of R), which assumes that you are using macOS 10.11 (El Capitan) or higher. 

> You'll also find an older version of R if you have an older version of macOS (10.9, Mavericks, or higher); I suggest, however, that you upgrade your OS to the current version before installing R. 

Once it is downloaded, double-click on the R installer. You may take all of the defaults.

### Additional Software

![bullet](http://socserv.mcmaster.ca/jfox/Courses/R/York-R-course/blueball.gif) To use Stan from R (via the **rstan** package), you must install  a C++ compiler and some other software.

> Install the Apple Xcode developer tools. For macOS 10.7 (Lion) or higher, you can install Xcode for free from the App Store. For earlier versions of macOS, Xcode can be installed from your system DVD or downloaded from the Apple developer website.

![bullet](http://socserv.mcmaster.ca/jfox/Courses/R/York-R-course/blueball.gif) Some R software (e.g., the **rgl**  package for constructing dynamic 3D graqphs) uses the the standard Unix X11 windowing system instead of the native Mac windowing system.

> 1. Download the disk image (dmg) file for XQuartz (X11 softward for macOS) from the [XQuartz website](https://www.xquartz.org/)..
>
  > 2. When you open this file by double-clicking on it, you'll find XQuartz.pkg; double-click on it to run the installer, clicking through all the defaults.
>
> 3. **Important**: After the installer runs, you'll have to log out and back on to your macOS account -- or simply reboot your Mac. 

![bullet](http://socserv.mcmaster.ca/jfox/Courses/R/York-R-course/blueball.gif) **Optional**: To create **Sweave** and **knitr** LaTeX documents (more sophisticated alternatives to R Markdown) in RStudio, and to compile R Markdown documents directly to PDF files, download and install the [MacTeX](https://www.tug.org/mactex/mactex-download.html) LaTeX system. 

## Installing R on Linux Systems

![bullet](http://socserv.mcmaster.ca/jfox/Courses/R/York-R-course/blueball.gif)Visit the [Comprehensive R Archive Network](http://cran.r-project.org/) (CRAN) and select a mirror site near you; a list of [CRAN mirrors](http://cran.r-project.org/mirrors.html) appears at the upper left of the CRAN home page. I suggest that you use the [0-Cloud mirror](https://cloud.r-project.org/), which is the first on the list. Click on the link _Download R for Linux_, which appears near the top of the page. R is available for several popular Linux distributions (Debian, RedHat, SUSE, and Ubuntu); select your distribution, and proceed as directed. 

![bullet](http://socserv.mcmaster.ca/jfox/Courses/R/York-R-course/blueball.gif) If you have a Linux (or Unix) system that's not compatible with one of these distributions, you will have to compile R from source code; the [procedure for doing so](http://cran.r-project.org/doc/FAQ/R-FAQ.html#How-can-R-be-installed-_0028Unix_002dlike_0029) is is described in the R FAQ (frequently asked questions) list.

### Additional Software

![bullet](http://socserv.mcmaster.ca/jfox/Courses/R/York-R-course/blueball.gif) To use Stan from R (via the **rstan** package), you must install build-essential, a recent version of the g++ (C++) compiler, and libssl-dev.

## Installing RStudio

![bullet](http://socserv.mcmaster.ca/jfox/Courses/R/York-R-course/blueball.gif)Go to the [RStudio download page](http://www.rstudio.com/products/rstudio/download/), select the free version of RStudio Desktop, and download the appropriate installer for your operating system (Windows, macOS, or Linux). Visit the [RStudio home page](http://www.rstudio.com/) for more information about RStudio.

![bullet](http://socserv.mcmaster.ca/jfox/Courses/R/York-R-course/blueball.gif)Once it is downloaded, run the RStudio installer and take all of the defaults: 

> In Windows, double-click on the RStudio installer to start the installation.
>
> In macOS, double-click on the downloaded  RStudio disk-image file, and drag the _RStudio_ icon to the _Applications_ folder. 

![bullet](http://socserv.mcmaster.ca/jfox/Courses/R/York-R-course/blueball.gif) When you first run RStudio, it should detect your R installation and start the R console. 

> To configure RStudio to your taste, select _Tools > Global Options_ (Windows) or _RStudio > Preferences_ (macOS) from the RStudio menus.
>
> In particular, I suggest that on the _General_ options screen you **deselect** _Restore .RData into workspace at startup_, and set _Save workspace to .RData on exit_ to **Never**.

![bullet](http://socserv.mcmaster.ca/jfox/Courses/R/York-R-course/blueball.gif) If you encounter difficulties, consult the [RStudio troubleshooting guide](https://support.rstudio.com/hc/en-us/articles/200488498-Troubleshooting-Guide-Using-RStudio).

## Installing R Packages for the  Workshops

![bullet](http://socserv.mcmaster.ca/jfox/Courses/R/York-R-course/blueball.gif)Once you have installed R, RStudio, and the necessary software for Stan, you can install additional R packages required for the R and longitudinal and multilevel data workshops. 

> Enter following commands, in order, at the  > command prompt in the RStudio R Console tab (and press the _Enter_ or _return_ key after each command):
>
> install.packages(c("aplpack", "car", "devtools", "effects", "ggplot2", "Hmisc", "knitr", "magrittr", "rgl", "rio", "rmarkdown"))
>
> install.packages("rstan", dependencies=TRUE)
>
> devtools::install_github(c("gmonette/spida2", "gmonette/p3d"))
>
> You can simply copy and paste these commands (one at a time) from these installation instructions. Alternatively, you can install the CRAN packages (but not Georges Monette's **spida2** and **p3d** packages) from the RStudio _Packages_ tab. Be aware that, depending on the speed of your internet connection, it may take some time  to download and install these packages and their dependencies.

## Stan Configuration (Optional)

The Stan folks describe some additional configuration steps, which are optional but recommended. See the Configuration section of [Installing RStan on Windows](https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Windows#configuration) or of [Installing RStan on Mac or Linux](https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Mac-or-Linux#configuration). I suggest that you skips these steps for the workshop but that you perform them before using RStan intensively for your own work.
                                                                                                                                                                
#'
#' __Acknowledgments:__ These instructions have been adapted from instructions prepared by John Fox for an ICPSR course in 2017                                                                                                                                                                
#'                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                Last modified: 2017-06-26 by J. Fox <[jfox AT mcmaster.ca](mailto::jfox@mcmaster.ca)>