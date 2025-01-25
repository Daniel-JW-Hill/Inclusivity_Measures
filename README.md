# README FILE FOR: Measurement of extensive and intensive smallholder inclusion in high value markets.

## Daniel Hill and Daniel Gregg

## Last update: 09 January 2025

This replication package contains the data, code and supplementary materials for the manuscript titled * Measurement of extensive and intensive smallholder inclusion in high value markets.* The files and folders contained in this package include:

-   **Head.R** reproduces all the main results for the manuscript. The script performs all the necessary steps for the analysis, calling necessary functions and data saved in the data and utilities folder and saving results in the results folder. The script can be executed when through the statistical program R, with the dependent packages downloaded.
-   **Data**  contains all the necessary data for the analysis. More details are provided below under the data availability section.
-   **Utilities** contains all the necessary functions to run the analysis scripts. More details are provided below in the utilities section.
-   **Results** contains all results printed by the main analysis scripts. These results are also presented in the manuscript for the main model, as well as the appendix for the high and low expected income models. More details are provided below.

## Data availability

Within the data folder, two data files are available and allow for the paper replication:

-   **hh_data.csv** – This is a de-identified version of the household level data necessary for replication. The file contains 210 observations. The respondent ID is denoted as (*HHID)*. 
-   **indiv_data.Rda** – This is a de-identified version of the individual level data necessary for replication. The file contains 411 observations. The household ID is denoted as (*HHID)*. Where household ID's are replicated (for most cases, individuals represent the household head and their spouse), FEMALE = 1 denotes whether the respondent is the female head of the household. All spouses are male-female in this context. 

Surveys were completed over February 2024 in teh Kapchorwa Region in Eastern Uganda. In total 210 households, of which included 411 household heads and their spouses, were surveyed. Village stratified sampling was conducted from a large coffee grower list used in previous studies (Arslan, Gregg, & Wollni, 2024). Households were first recruited and visited by teams of two – a male enumerator for the male respondent (usually the household head), and a female enumerator for the female respondent. 

This data collection was part of a broader project (*Inclusive Coffee Value Chains*), approved by the University of New England Human Research Ethics Committee (Approval number: HR2023-095), the Makerere University School of Social Sciences Research Ethics Committee (Approval Number: 10.2023.706), and registered with the Uganda National Council for Science and Technology (Registration Number SS2312ES). 

## Computing requirements

The main analysis scripts are executable in the statistical program R. Please refer to the R documentation to see the relevant requirements for your computer and your organisation.

-   <https://cran.rstudio.com/bin/windows/base/>
-   <https://cran.rstudio.com/bin/macosx/>
-   <https://cran.rstudio.com/bin/linux/>

Thes dependent packages for the analysis should be installed before executing the code, and include:

-   *logistf -* 
-   *glmmTMB -*
-   *ggplot2 -* *https://cran.r-project.org/web/packages/ggplot2/index.html*](https://cran.r-project.org/web/packages/ggplot2/index.html*
-   *performance -*
-   *marginaleffects -*
-   *reshape2 -*
-   *glmtoolbox -*
-   *rms -*
-   *stargazer -*

## Replication steps

For replication of the manuscript *Measurement of extensive and intensive smallholder inclusion in high value markets.:*

1.  Install R and the dependent packages for the analysis script.
2.  Ensure the working directory is established according to your local working directory. 
3.  Run the Head.R script.
4.  Check the results saved in the results folder.
**  
**

## List of utilities

The utilities folder contains necessary functions for the analysis. All functions are documented but more details are provided below:

| **Function**                    | **Description**                                                                                                                          |
|---------------------------------|------------------------------------------------------------------------------------------------------------------------------------------|
| get_bias_ci_continuous.R        | Estimates confidence intervals in which inequality in opportunity may be biased due to omitted variables - for the household models.  
| get_bias_ci_binary              | Estimates confidence intervals in which inequality in opportunity may be biased due to omitted variables - for the individual model                                                                       |
| get_contribution_plots.R        | Plots how inequality in opportunity changes from the get_decomposition_hh.R and get_decomposition_indiv.R  functions.       |
| get_decomposition_hh.R          | Estimates how inequality in opportunity changes when variation in certain circumstances (covariates) is equalised to the sample mean - for the household model |
| get_decomposition_indiv.R       | Estimates how inequality in opportunity changes when variation in certain circumstances (covariates) is equalised to the sample mean - for the individual model |
| get_first_stage_regressions.R   | Estimates the first stage regressions for the reduced form models |
| get_hh_models.R                 | Estimates the direct and reduced form models for the household data, including bootstrapping  of results for standard errors in average marginal effects and inequality indices.                                                                         |
| get_indiv_models.R              |  Estimates the direct and reduced form models for the individual data, including bootstrapping  of results for standard errors in average marginal effects and inequality indices.                                                                      |
| get_inequality_functions.R      |  Functions to calculate the inequality indices - such as PSY normalised GINI coefficient and absolute GINI coefficient.                                                           |
| get_inequality_measures.R       |  Calculates inequality in opportunity from fitted values derived from model estimation scripts                                                                                  |
| get_preferences.R               |  Not run in replication script, but illustrates how preferences were derived from the ranking choice experiment                                                           |
| get_ranked_logit.R              |  Used in get_preferences.R to estimate preferences.  Not run in replication script, but illustrates how preferences were derived from the ranking choice experiment
| ov_generate.R                   |  Generates the omitted variables based on drawn correlations for bias simulations. 
| plot_chart.R                    |  Plots decompositions. |                            

## Variable names and descriptions

### hh_data.Rda

| **Variable**                  | **Description**                                                                           |
|-------------------------------|-------------------------------------------------------------------------------------------|
| "HHID"                         | Household id                                                                            |
| "SINGLE"                      | Binary flag indicating whether household has single head                                                   |
| "FEMALE_HEAD"                 | Binary flag indicating whether household head is female (always in single headed households)                              |
| "AGE_HEAD"                    | Age of household head in years                                   |
| "EDUCATION_HEAD"              | Education of household head in years                                                                         |
| "DEPENDENCY_RATIO"            | Number of children and non-working adults in the household, proportional to number of working adult household members.                    |
| "SOCIAL_PARTICIPATION"        | Binary flag for the household head being an active member or leader of social groups including farmer groups, savings groups, church etc.                     |
| "COFFEE_TREES"                | Number of coffee trees owned/managed by the household, in thousands.                     |
| "COFFEE_TREES_SQR"            | COFFEE_TREES^2              |
| "PROPORTION_LAND_RENTED"      | Proportion of land rented relative to land owned by household                 |
| "ASSETS"                      | Market valuation of indicative non-building assets owned by the household, including cash, livestock, phones, motorcycles, and coffee processing equipment. In millions UGX (and February 2023 prices)              |
| "ALTITUDE"                    | Altitude of household measured in 000's metres above sea level.                    |
| "DISTANCE_PAVED_ROAD"         | Distance, by road, of household to the primary paved road in the region (Sipi-Kapchorwa road). In kms                                   |
| "DECISION_DISAGREEMENTS"      | A binary indicator equal to one when the husband and wife disagree on who has majority input on coffee marketing decisions in the household (either claim both are leaders, or one a leader and other 				equal decision making).           |
| "INPUT_PREFERENCES"           | The aggregate estimated likelihood that the household head ranks either credit or input support as the most important attribute a hypothetical coffee buyer can offer, estimated from a ranking choice 					experiment over 8 coffee buyer attributes.                                     |
| "EASE_MARKETING_PREFERENCES"  | The estimated likelihood that the household head ranks ease of marketing attributes, such as the buyer being always open, offering easy quality assurance, and offering mobile and cash payments, as the 					most important attribute a hypothetical coffee buyer can offer. Estimated from a 					ranking choice experiment over 8 coffee buyer attributes                                          |
| "PRICE_PREFERENCES"           | The estimated likelihood that the household head ranks higher prices (100 shillings per kg of coffee cherries) as the most important attribute a hypothetical coffee buyer can offer. Estimated from a 					ranking choice experiment over 8 coffee buyer attributes                                                      |
| "RELATIVE_PRICE_DIFFERENTIAL" | Average price differential of the HVM buyers available to the household hear, relative to the commodity buyer price. In thousands of Ugandan shillings per kilogram of coffee cherries                                                               |
| "BINARY_PARTICIPATION"        | Binary flag indicating whether household sells any coffee to high value markets                                                                |
| "CONTINUOUS_PARTICIPATION"    | Proportion of coffee sold by household to high value markets                                                               |



### indiv_data.Rda

| **Variable**                  | **Description**                                                                           |
|-------------------------------|-------------------------------------------------------------------------------------------|
| "HHID"                         | Household id                                                                            |
| "FEMALE"                      | Binary flag indicating whether individual identifies as female                                               |
| "SINGLE"                      | Binary flag indicating whether household has single head                                                   |
| "FEMALE_HEAD"                 | Binary flag indicating whether household head is female (always in single headed households)                              |
| "AGE_HEAD"                    | Age of individual in years                                   |
| "EDUCATION_HEAD"              | Education of individual in years                                                                         |
| "DEPENDENCY_RATIO"            | Number of children and non-working adults in the household, proportional to number of working adult household members.                    |
| "SOCIAL_PARTICIPATION"        | Binary flag for the individual being an active member or leader of social groups including farmer groups, savings groups, church etc.                     |
| "COFFEE_TREES"                | Number of coffee trees owned/managed by the household, in thousands.                     |
| "COFFEE_TREES_SQR"            | COFFEE_TREES^2              |
| "PROPORTION_LAND_RENTED"      | Proportion of land rented relative to land owned by household                 |
| "ASSETS"                      | Market valuation of indicative non-building assets owned by the individual, including cash, livestock, phones, motorcycles, and coffee processing equipment. In millions UGX (and February 2023 prices)              |
| "ALTITUDE"                    | Altitude of household measured in 000's metres above sea level.                    |
| "DISTANCE_PAVED_ROAD"         | Distance, by road, of household to the primary paved road in the region (Sipi-Kapchorwa road). In kms                                   |
| "DECISION_DISAGREEMENTS"      | A binary indicator equal to one when the husband and wife disagree on who has majority input on coffee marketing decisions in the household (either claim both are leaders, or one a leader and other 				equal decision making).           |
| "INPUT_PREFERENCES"           | The aggregate estimated likelihood that the individual ranks either credit or input support as the most important attribute a hypothetical coffee buyer can offer, estimated from a ranking choice 					experiment over 8 coffee buyer attributes.                                     |
| "EASE_MARKETING_PREFERENCES"  | The estimated likelihood that the individual ranks ease of marketing attributes, such as the buyer being always open, offering easy quality assurance, and offering mobile and cash payments, as the 					most important attribute a hypothetical coffee buyer can offer. Estimated from a 					ranking choice experiment over 8 coffee buyer attributes                                          |
| "PRICE_PREFERENCES"           | The estimated likelihood that the individual ranks higher prices (100 shillings per kg of coffee cherries) as the most important attribute a hypothetical coffee buyer can offer. Estimated from a 					ranking choice experiment over 8 coffee buyer attributes                                                      |
| "RELATIVE_PRICE_DIFFERENTIAL" | Average price differential of the HVM buyers available to the individual, relative to the commodity buyer price. In thousands of Ugandan shillings per kilogram of coffee cherries                                                               |
| "BINARY_PARTICIPATION"        | Binary flag indicating whether individual sells any coffee to high value markets                                                                |

