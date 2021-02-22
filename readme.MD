# Installing package

```
git clone https://bitbucket.org/previsionio/previsionio-r
R CMD build previsionio-r
R CMD INSTALL previsionio_10.21.0.tar.gz
```

# Using package
```
require(httr)
require(futile.logger)
library(previsionio)
```

Look at examples.R

# Building Documentation 

1) To genrate the Doxygen documentation
All the functions are commented directly in the code. 
The following lines generate a new folder " man " in the R project. 
In Rstudio, type : 
```
library(roxygen2)
library(devtools)
document()
```

2) To generate automaticcaly the documentation in pdf.
In the terminal, in the directory where the project is, type : 
```
R CMD Rd2pdf --pdf --title='Package R for Prevision.io' -o /tmp/Previsionio_R.pdf man/*.Rd

```

If you find trouble doing so on windows, make sure that
MiKTeX is installed & updated
See : https://tex.stackexchange.com/questions/125274/error-font-ts1-zi4r-at-540-not-found if you encounter a font issue
