# mmstat4

MM*Stat was originally published as a CD in 1998 by Prof. Dr. W. Härdle and Prof. Dr. B. Rönz from the Chair of Statistics at the Faculty of Economics at Humboldt University in Berlin in cooperation with the company MHSG.

The R package now collects data and R programmes that we use in various courses on statistics at the Chair of Statistics at the Faculty of Economics at the Humboldt University of Berlin.

# Installation  

Since the package is available at CRAN it can be installed by 

```R
install.packages("mmstat4")
```

and the development version by

```R
library("devtools")
install_github("sigbertklinke/mmstat4")
```

If under MacOS the error `X11 library is missing: install XQuartz from xquartz.macosforge.org` appears then install XQuartz. It is writen in the R documentation:

*Various parts of the build require XQuartz to be installed: see [https://www.xquartz.org/releases/](https://www.xquartz.org/releases/). These include the tcltk package and the X11 graphics device: attempting to use these without XQuartz will remind you. This is also needed for some builds of the cairographics-based devices (which are not often used on macOS) such as png(type = "cairo") and svg() and some third-party packages (e.g. rgl).*
