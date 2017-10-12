# tmc-rstudio

[![Build Status](https://travis-ci.org/RTMC/tmc-rstudio.svg?branch=master)](https://travis-ci.org/RTMC/tmc-rstudio)
[![Coverage Status](https://coveralls.io/repos/github/RTMC/tmc-rstudio/badge.svg?branch=master)](https://coveralls.io/github/RTMC/tmc-rstudio?branch=master)

### Documentation

* [Product backlog, sprint backlogs and worked hours](https://docs.google.com/spreadsheets/d/1uS8EfZtXFUFsn7fuUvls3LqDM_Vpn82c1zXXGLNh6ws/)

### Installation instructions:


How to install the package: 

1. On the terminal, navigate to the folder where tmcRtestrunner_0.1.0.tar.gz is located, and run the command "R CMD INSTALL tmcRtestrunner_0.1.0.tar.gz"

2. Next, either:
	i. Go to tmcrstudioaddin's parent directory, open R and type "install.packages("tmcrstudioaddin",repos=NULL, type="source")".
		If this fails, install RStudio version that is at least 1.1.67 (from https://www.rstudio.com/products/rstudio/download/preview/). You may need to install it from the terminal with e.g. dkpg (not with Software Center).
			Also install the required packages manually (with install.packages("package_name").
			
or
	ii. On the terminal, navigate to the folder where tmcrstudioaddin_0.2.5.tar.gz is located, and run the command "R CMD INSTALL tmcrstudioaddin_0.3.0.tar.gz"

3. Once the package tmcrstudioaddin is installed, open RStudio and select "Addins" from upper bar and there "TMC R 0.3.0".

