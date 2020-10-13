

# Terascope Terapixel Deployment: Computational Analysis
> Mark Tyrrell

<img src="https://media.blendernation.com/wp-content/uploads/2018/03/Screenshot_20180322-161400-1024x768.png" align="right" />

The visualisation of multiscale urban data accessible on low cost thin client devices offers increasing benefits in many contexts including urban planning and disaster management. Distributed super computing makes this possible by outsourcing pixel rendering to cloud resources. The computational requirements of rendering images to this level are not insignificant, with pixels representing terrestrial topography to the millimeter. Newcastle University’s recently deployed Terascope Terapixel project provides an interesting opportunity to analyse performance of such a system in order to further understanding of the underlying computational processes.


# Loading
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

