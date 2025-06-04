# tmc-rstudio

[![Build Status](https://travis-ci.org/RTMC/tmc-rstudio.svg?branch=master)](https://travis-ci.org/RTMC/tmc-rstudio)
[![Coverage Status](https://coveralls.io/repos/github/RTMC/tmc-rstudio/badge.svg?branch=master)](https://coveralls.io/github/RTMC/tmc-rstudio?branch=master)

### Documentation

* [Product backlog, sprint backlogs and worked hours](https://docs.google.com/spreadsheets/d/1uS8EfZtXFUFsn7fuUvls3LqDM_Vpn82c1zXXGLNh6ws/)
* [Guide pages](https://rtmc.github.io)

### Installation instructions:


How to install the package: 

1. Install the `tmcRtestrunner` package
   i. On the terminal, navigate to the folder where `tmcRtestrunner_0.3.0.tar.gz` is located,
   and run the command `R CMD INSTALL tmcRtestrunner_0.3.0.tar.gz`
   ii. Alternatively, one can enter the R command 
   `devtools::install_github("testmycode/tmc-r-tester/tmcRtestrunner", build = FALSE)` in the R console
   to directly download from GitHub

2. Next, either:
   i. Go to `tmcrstudioaddin`'s parent directory, open R and type 
   `install.packages("tmcrstudioaddin", repos = NULL, type = "source")`.
   If this fails, install RStudio version that is at least 1.3.10731 (from
   https://www.rstudio.com/products/rstudio/download/).
   Also install the required packages manually (with `install.packages("package_name")`).
   ii. Alternatively, on the terminal, navigate to the folder where `tmcrstudioaddin_0.9.1.tar.gz` is located,
   and run the command `R CMD INSTALL tmcrstudioaddin_0.9.1.tar.gz`. You still
   need to install the required packages manually (with `install.packages("package_name")`).

3. Once the package `tmcrstudioaddin` is installed, open RStudio and select "Addins" from upper bar and there
   `TMC R 0.9.1`.

### Credits


The project was created as a Software Engineering Lab project at the Department of Computer Science in
the University of Helsinki. The authors are:

* Antti Haapaniemi ([anttihaap](https://github.com/anttihaap))
* Janne Hyttinen ([hyttijan](https://github.com/hyttijan))
* Tuomo Lehtonen ([tmoo](https://github.com/tmoo))
* Eero Ojala ([eerojala](https://github.com/eerojala))
* Samu Vaittinen ([samuvait](https://github.com/samuvait))
* Aleksis Vuoksenmaa ([aleksisv](https://github.com/aleksisv))

Current maintainer

* Petteri Piiroinen ([petteripiiroinen](https://github.com/petteripiiroinen))
