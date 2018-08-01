# Distributive Justice and Transportation Equity: Inequality in accessibility in Rio de Janeiro

This repository contains code related to my PhD thesis "Distributive Justice and Transportation Equity: Inequality in accessibility in Rio de Janeiro", presented for the degree of Doctor of Philosophy at the University of Oxford in 2018. This repository contains the `R` scripts used in the data processing, visualization and analysis presented in the thesis. The preprint of the submitted thesis can be downloaded [here](https://thesiscommons.org/2sn9j).

![alt text](https://github.com/rafapereirabr/thesis/blob/master/imgs/map1_infra%20legend_compos_north.png)

The thesis focuses on the distributional effects of transport policies in terms of how they shape inequalities in people's access to opportunities. In the empirical research, I analyzed how recent transport investments implemented in Rio de Janeiro (Brazil) in preparation to host the 2014 Football World Cup and the 2016 Olympic Games have impacted accessibility to sports venues, healthcare facilities, public schools and job opportunities for different income groups. 

## Code
* The scripts in the root of `Rscripts` folder organize/create the data and few functions used in the empirical chapters. The scripts are kept here for personal reference and I recognize that the code is not well documented or explained.

* **Chapter** 3 analyzes the catchment areas of sports venues and healthcare facilities in terms of how many people from different income groups can reach those locations from their homes using only public transport and walking. The estimates are calculated using a before-and-after comparison of Rio’s transport network (2014-2017) and a quasicounterfactual.
scenario.
   * [Paper published in the journal _Cities_](https://www.sciencedirect.com/science/article/pii/S0264275117311563)

* **Chapter 4** estimates how recent transport policies implemented in Rio between 2014 and 2017 have impacted people from different income groups in terms of the number of schools and job opportunities they could reach from their homes via public transport. A spatial regression model and cluster analysis are used to estimate the distributive effects of those transport policies on accessibility inequalities and to test whether these effects are robust when analysis is conducted using different geographical scales and zoning schemes.
   * [Paper under review](https://osf.io/preprints/socarxiv/cghx2) 

<img src="https://github.com/rafapereirabr/thesis/blob/master/imgs/map_income.png" width="422" height="358"> <img src="https://github.com/rafapereirabr/thesis/blob/master/imgs/map4_oAcess_jobsmatch_60_2server.png" width="422" height="358">


* **Chapter** 5 illustrates how ex-ante accessibility analysis can be used to evaluate the equity and accessibility impacts of different transport project scenarios in their early planning stages using open-source software and standardized datasets. This chapter evaluates the scenarios of full and partial construction of the TransBrasil BRT corridor, currently under development in Rio de Janeiro. It looks more specifically at how these two scenarios could impact employment accessibility of different income groups and how these results vary when different travel time thresholds are considered.
   * [Paper forthcoming in the _Journal of Transpor Geography_](https://osf.io/sut7r/) 


<img src="https://github.com/rafapereirabr/thesis/blob/master/imgs/map4_diff_jobsmatch_ratio_zoom_in%20-%20Copy_SCALE.png" width="843" height="513">





