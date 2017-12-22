# tmc-rstudio

[![Build Status](https://travis-ci.org/RTMC/tmc-rstudio.svg?branch=master)](https://travis-ci.org/RTMC/tmc-rstudio)
[![Coverage Status](https://coveralls.io/repos/github/RTMC/tmc-rstudio/badge.svg?branch=master)](https://coveralls.io/github/RTMC/tmc-rstudio?branch=master)

### Documentation

* [Product backlog, sprint backlogs and worked hours](https://docs.google.com/spreadsheets/d/1uS8EfZtXFUFsn7fuUvls3LqDM_Vpn82c1zXXGLNh6ws/)
* [Guide pages](https://rtmc.github.io)
### Installation instructions:


How to install the package: 

1. Install the tmcRtestrunner package
	i. On the terminal, navigate to the folder where tmcRtestrunner_0.1.0.tar.gz is located, and run the command "R CMD INSTALL tmcRtestrunner_0.1.0.tar.gz"
	ii. Alternatively, one can enter the R command "devtools::install_github("RTMC/tmc-r-tester/tmcRtestrunner")" in the R console to directly download from GitHub

2. Next, either:
	i. Go to tmcrstudioaddin's parent directory, open R and type "install.packages("tmcrstudioaddin",repos=NULL, type="source")".
		If this fails, install RStudio version that is at least 1.1.67 (from https://www.rstudio.com/products/rstudio/download/preview/). You may need to install it from the terminal with e.g. dkpg (not with Software Center).
			Also install the required packages manually (with install.packages("package_name").
			
or
	ii. On the terminal, navigate to the folder where tmcrstudioaddin_0.3.1.tar.gz is located, and run the command "R CMD INSTALL tmcrstudioaddin_0.3.1.tar.gz"

3. Once the package tmcrstudioaddin is installed, open RStudio and select "Addins" from upper bar and there "TMC R 0.3.1".

### Credits


The project was created as a Software Engineering Lab project at the Department of Computer Science in the University of Helsinki. The authors are:
* Antti Haapaniemi ([anttihaap](https://github.com/anttihaap))
* Janne Hyttinen ([hyttijan](https://github.com/hyttijan))
* Tuomo Lehtonen ([tmoo](https://github.com/tmoo))
* Eero Ojala ([eerojala](https://github.com/eerojala))
* Samu Vaittinen ([samuvait](https://github.com/samuvait))
* Aleksis Vuoksenmaa ([aleksisv](https://github.com/aleksisv))
