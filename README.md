# Identifiying High Suicide Rates in Colorado

We aim to indentify if there are any unusually high rates of suicide and potential demographics that lead to high suicide rates in Colorado counties between 2010 and 2018.  Data is publicly available from the Colorado Department of Public Health and Environment and the U.S. Census Bureau.  (Note, counts below 3 per county are surpressed to 0.) 

![](https://github.com/Emma-M-Collins/spatial_stats/blob/main/suicide_proportion.png)

## Identifying Clusters under the Constant Risk Hypothesis
* The Constant Risk Hypothesis (CRH) assumes all individuals in each county have the same risk of suicide, regardless of location, with no other factors considered.
* We use Kulldorf's Spatial Scan test and Turnbull et. al's Cluster Evaluation Permutation Procedure (CEPP) to identify clusters of counties with high rates of suicide.

![](https://github.com/Emma-M-Collins/spatial_stats/blob/main/CRH_tests.png)
* We can see both tests identifies clusters in a similar area.

## Identifying Clusters using Poisson GLM
* The expected number of suicide counts are modeled using a Poisson Generalized Linear Model.  Variables were chosen based on research with only statistically significant variables used.  The variables are Income Per Capita, proportion of county considered rural, proportion of county with a high school diploma, proportion of county in poverty, and proportion of county that owns their main residence.
* The results of the model are used as an estimation instead of the CRH to determine whether the demographics used influence suicide rates.

![](https://github.com/Emma-M-Collins/spatial_stats/blob/main/GLM_tests.png)

* After re-testing the Spatial Scan test and CEPP using the GLM counts, we can see the results are nearly identical.  The same clusters of high suicide rates are identified.

## Conclusions/Future Work
* Since there are similar results of the tests under the CRH and the Poisson GLM counts, we cannot conclude the used variables contribute to higher rate of suicides in Colorado counties.
* However, all tests conclude there are unusually high rates of suicide in the South-Western and South-Central areas of Colorado
* Other tests and hypothesis were explored, which can be viewed in the R code and report.
* Future work would include more recent data once 2020 census data becomes available, and use more (harder to find) data, such as gun ownership rates for example, in the Poisson GLM to model suicide rates.
