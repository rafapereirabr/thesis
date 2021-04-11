# Distributive Justice and Transportation Equity: Inequality in accessibility in Rio de Janeiro

This repository contains code related to my PhD thesis "Distributive Justice and Transportation Equity: Inequality in accessibility in Rio de Janeiro", presented for the degree of Doctor of Philosophy at the University of Oxford in 2018. This repository contains the `R` scripts used in the data processing, visualization and analysis presented in the thesis. [My PhD thesis is available for download here](https://ora.ox.ac.uk/objects/uuid:3552ca9f-25c0-4d2f-acdd-0649de911afc).

If you want to use this code, please cite the [PhD thesis](https://ora.ox.ac.uk/objects/uuid:3552ca9f-25c0-4d2f-acdd-0649de911afc) or the papers below.


![alt text](https://github.com/rafapereirabr/thesis/blob/master/imgs/map1_infra%20legend_compos_north.png)

The thesis focuses on the distributional effects of transport policies in terms of how they shape inequalities in people's access to opportunities. In the empirical research, I analyzed how recent transport investments implemented in Rio de Janeiro (Brazil) in preparation to host the 2014 Football World Cup and the 2016 Olympic Games have impacted accessibility to sports venues, healthcare facilities, public schools and job opportunities for different income groups. 

## Code
* The scripts in the root of `Rscripts` folder organize/create the data and few functions used in the empirical chapters. The scripts are kept here for personal reference and I recognize that the code is not well documented or explained.

* **Chapter** 3 analyzes the catchment areas of sports venues and healthcare facilities in terms of how many people from different income groups can reach those locations from their homes using only public transport and walking. The estimates are calculated using a before-and-after comparison of Rio’s transport network (2014-2017) and a quasicounterfactual.
scenario.
   * [Paper published in the journal _Cities_](https://www.sciencedirect.com/science/article/pii/S0264275117311563)
   * [[PDF ungated version]](https://www.dropbox.com/s/vbzyug6omz36t7w/Pereira%20%282018%29%20Transport%20legacy%20of%20mega-events%20and%20the%20redistribution%20of%20accessibility.pdf)

* **Chapter 4** estimates how recent transport policies implemented in Rio between 2014 and 2017 have impacted people from different income groups in terms of the number of schools and job opportunities they could reach from their homes via public transport. A spatial regression model and cluster analysis are used to estimate the distributive effects of those transport policies on accessibility inequalities and to test whether these effects are robust when analysis is conducted using different geographical scales and zoning schemes.
   * [Paper published in the _Journal of Transport and Land Use_](https://www.jtlu.org/index.php/jtlu/article/view/1523)

<img src="https://github.com/rafapereirabr/thesis/blob/master/imgs/map_income.png" width="422" height="358"> <img src="https://github.com/rafapereirabr/thesis/blob/master/imgs/map4_oAcess_jobsmatch_60_2server.png" width="422" height="358">


* **Chapter** 5 illustrates how ex-ante accessibility analysis can be used to evaluate the equity and accessibility impacts of different transport project scenarios in their early planning stages using open-source software and standardized datasets. This chapter evaluates the scenarios of full and partial construction of the TransBrasil BRT corridor, currently under development in Rio de Janeiro. It looks more specifically at how these two scenarios could impact employment accessibility of different income groups and how these results vary when different travel time thresholds are considered.
   * [Paper published in the _Journal of Transport Geography_](https://www.sciencedirect.com/science/article/pii/S0966692318302047?dgcid=author)
   * [[PDF ungated version]](https://www.dropbox.com/s/rtjn8zqqzkn1ztp/Pereira%20%282019%29%20Future%20accessibility%20impacts%20of%20transport%20policy%20scenarios-%20Equity%20and%20sensitivity%20to%20travel%20time.pdf)


<img src="https://github.com/rafapereirabr/thesis/blob/master/imgs/map4_diff_jobsmatch_ratio_zoom_in%20-%20Copy_SCALE.png" width="843" height="513">





