### R and documents http://www.r-project.org/

* http://cran.r-project.org/doc/manuals/r-patched/R-intro.pdf
* http://cran.r-project.org/other-docs.html

| L | http://cran.r-project.org/doc/contrib/usingR.pdf |
--- | ---
| M | http://cran.r-project.org/doc/contrib/Paradis-rdebuts_en.pdf |
| S | http://cran.r-project.org/doc/contrib/Torfs+Brauer-Short-R-Intro.pdf |

* Session or Tools | Set Working Directory... `setwd("/workspace/R-work")`  
* What are packages, or libraries? `library()`  
* `install.packages("geometry")`, or `geometry`  
* `a = 4`  
* `a`  
* `a = a + 1`  
* `rm(list=ls())`  
* scalar, vectors, and matrices  
* `b=c(3,4,5) # concatenate`  
* `(3+4+5)/3`  
* `rnorm(10)`  
* `rnorm(10, mean=1.2, sd=3.4)`  
* `rnorm [TAB] # random numbers from normal distribution`  
* `x = rnorm(100) # normal random numbers`  
* `help(rnorm)`  
* `example(rnorm)`  
* `help.start()`  
* `source("foo.R")`  

##### R

* http://davidsimpson.me/2013/02/26/installing-r-on-os-x/
* R http://cran.rstudio.com/bin/macosx/, and R Studio http://www.rstudio.com/ide/

```bash
curl -o /tmp/income_train_data_10.csv -ksL http://dl.dropboxusercontent.com/u/47820156/r/income_train_data_10.csv
```

```bash
R
install.packages('ggplot2') # qplot() for quick plots; ggplot() for fine, granular control for everything.
install.packages('ROCR')
```

```bash
R
income <- read.csv('/tmp/income_train_data_10.csv', header=T, strip.white=TRUE)
income$class <- as.factor(income$class)
? read.csv
summary(income)
str(income)
library('ggplot2')
qplot(data=income, x=age,main="Histogram of Age",binwidth=3)
```
