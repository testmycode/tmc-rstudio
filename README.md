# tmc-rstudio

### Documentation

* [Product backlog, sprint backlogs and worked hours](https://docs.google.com/spreadsheets/d/1uS8EfZtXFUFsn7fuUvls3LqDM_Vpn82c1zXXGLNh6ws/)

### Installation instructions:


How to install the package: go to its parent directory, open R and type "install.packages("tmcrstudioaddin",repos=NULL, type="source")".
If this fails, install RStudio version that is at least 1.1.67 (from https://www.rstudio.com/products/rstudio/download/preview/). You may need to install it from the terminal with e.g. dkpg (not with Software Center).

Also install the required packages manually (with install.packages("package_name").
Once the package tmcrstudioaddin is installed, open RStudio and select "Addins" from upper bar and there "TMC login".


To install the package in Windows:

1. Open command prompt and navigate to the directory where tmcrstudioaddin_0.1.0.zip is located
2. Open the R terminal in the command prompt by typing R
  -If R terminal does not open in the command prompt, make sure you have R installed and the filepath to the correct version of R has been appended to the "Path" system environment variable.

3. Type and enter "install.packages("tmcrstudioaddin_0.1.0.zip", repos = NULL)"

Notes:
  You can also use the RStudio console instead of the R terminal in the command prompt, but you need to either provide an explicit file path or a relative path from your current working directory to tmcrstudioaddin_0.1.0.zip for install.packages

  On how to edit the Path variable, see https://www.computerhope.com/issues/ch000549.htm

  The filepath to R depends on the versions of R and Windows you are using. For example, the default file path to R with R version 3.4.0 and and a 64-bit version of Windows is "C:\Program Files\R\R-3.4.0\bin\x64""
    Replace "R-3.4.0"" with the version of R you are using

For 32-bit versions of Windows replace "x64"" with "i386".
