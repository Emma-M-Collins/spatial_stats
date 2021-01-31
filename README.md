# Identifiying High Suicide Rates in Colorado

We aim to indentify if there are any unusually high rates of suicide and potential demographics that lead to high suicide rates in Colorado counties between 2010 and 2018.  Data is publicly available from the Colorado Department of Public Health and Environment and the U.S. Census Bureau.  (Note, counts below 3 per county are surpressed to 0.) 

![](https://github.com/Emma-M-Collins/spatial_stats/blob/main/suicide_proportion.png)

### Identifying Clusters under the Constant Risk Hypothesis
* The Constant Risk Hypothesis (CRH) assumes all individuals in each county have the same risk of suicide, regardless of location, with no other factors considered.
* We use Kulldorf's Spatial Scan test and Turnbull et. al's Cluster Evaluation Permutation Procedure (CEPP) to identify clusters of counties with high rates of suicide.

![](https://github.com/Emma-M-Collins/spatial_stats/blob/main/CRH_tests.png)
