# CSC8634_CC_Project

The Rmarkdown report .rmd source file is located in 'CSC8634_CC_Project/reports' folder. The file makes heavy use of cached objects and saved plots in order to speed loading. To run the analysis again from the start, the .r scripts must be run as follows.

To load project from scripts:

1. Open the archived project template folder 

2. Start R Studio

3. Set working directory to project template folder, ensure the ProjectTemplate package is installed and loaded from the library, then execute load.project()

4. Download data files from: https://github.com and put in 'CSC8634_CC_Project/data'

5. Open '01-A.R' from 'CSC8634_CC_Project/munge' and run the script

6. Open '02-A.R' from 'CSC8634_CC_Project/munge' and review the script and decide on necessary operations. For example, gpu_task can take hours to run depending on the machine. In this case, it may be better to just use the cached dataset.

7. Open 'EDA-01.R' from 'CSC8634_CC_Project/src' and run through the analysis operations as requiredf


NB: Additional files of interest:

* log.tx (git commit log)
* CSC8634_CC_Project/docs/references (contains reference literature for report)

