# City of Portland Income Analysis

This analysis is a collaboration between the [Bureau of Planning and Sustainability (BPS)](https://www.portlandoregon.gov/bps/) and [Portland Housing Bureau (PHB)](https://www.portland.gov/phb. It attempts to replicate the PUMS-derived data from [Seattle's Displacement Risk Indicators](https://population-and-demographics-seattlecitygis.hub.arcgis.com/pages/displacement-risk).

Their analysis produces citywide income estimates, similar to CHAS data, including the following tables:

* Cost burden and severe cost burden by age, race, tenure and income category
* Renter households by income category
* Rental units by affordability category
* Affordable and available rental units per 100 households

As of February 2021, the analysis is incomplete. Please check back later.

## Data source

Sociodemographic data for Oregon and Washington were downloaded from [University of Minnesota IPUMS-USA database](https://usa.ipums.org/usa/index.shtml). IPUMS citation follows below. The IPUMS dataset provides time-harmonized variables that make analysis of [Census Public Use Microdata Series (PUMS)](https://www.census.gov/programs-surveys/acs/microdata.html) data easier. This analysis uses 2006 to 2019 ACS 1-year estimates.

Command files (DDI) for the IPUMS database queries were saved and uploaded to this repository. They tell the user exactly which variables and ACS samples were queried and allows the user to more easily work with PUMS data through providing the variable code labels.

### Variance and standard errors

All statistical sampling comes with a degree of uncertainty, called a standard error. Planners and policy makers are often familiar with margins of error, which map the standard error to a confidence level. American Community Survey data uses a 90% confidence level, meaning if the survey sampling techniques were repeated many times, we would expect 90% of the resulting population estimates to contain the true population parameter.

Because this analysis uses PUMS data, standard errors can only feasibly be calculated at scale by specifying the survey design in a statistical programming software such as R. It can be based on replicate weights, or a second method is possible using clusters and strata. Kyle Walker provides [excellent background](https://walker-data.com/tidycensus/articles/pums-data.html) on this topic. 

We use replicate weights to make available two survey objects—one for households and another for persons. These survey objects automatically calculate the standard errors when summarizing the data. 

Re-coded variables must be calculated prior to creating a survey object. To create a new re-coded variable, do so on the `pums_clean` interim variable in `scripts/01_clean_data.R` before feeding it into a survey object.

### IPUMS-USA citation

Steven Ruggles, Sarah Flood, Sophia Foster, Ronald Goeken, Jose Pacas, Megan Schouweiler and Matthew Sobek. IPUMS USA: Version 11.0 [dataset]. Minneapolis, MN: IPUMS, 2021. https://doi.org/10.18128/D010.V11.0

## Getting started

To work with the microdata that we use to produce the income analysis, ensure your system has the proper software installed as well as the required R packages. Then open the file and begin working with the data.

### Required software
We recommend the following software and R packages installed on your computer:

1. [R](https://cran.r-project.org/) - analysis done on version 3.6.3
2. [R-Studio](https://rstudio.com/) - a free integrated development environment (IDE) for R that makes it easier to manage files and scripts
3. Certain R packages, including:
   1. tidyverse
   2. tidycensus
   3. ipumsr
   4. srvyr
   5. pacman
   
### Loading in the data

1. Open `income-analysis.Rproj` in R-Studio.
2. Open `scripts/02_data_analysis.R` and run the line `source("scripts/01_clean_data.R")`. This will take 3-7 minutes to load all of the data and create survey objects.
3. Now you are set to explore the data. Future work will include an R notebook to demonstrate examples. A good first place to start is with the `h_repd` and `p_repd` survey objects. Use typical tidyverse `group_by` and `summarize` functions that you can then feed into a ggplot graph, for example. Standard errors are automatically calculated with survey objects.
4. If you need to calculate a new variable prior to aggregating the data, do so on the `pums_clean` interim variable in `scripts/01_clean_data.R` before feeding it into a survey object.

## Acknowledgements

The community of ACS microdata users is small in Portland. We would like to acknowledge the following people for their patience, support, analyses and code-sharing, which was instrumental in helping City of Portland analysts throughout their R and PUMS and ACS journey:

* Nick Chun
* Jamaal Green
* Charles Rynerson
* Josh Lehner
* [Kyle Walker](https://walker-data.com/), though he doesn't know it