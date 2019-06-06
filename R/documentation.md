## Introduction

The Data Science Program (DSP) team at Mayo Clinic launched a project to create a unified data quality assessment tool using R packages and employing an R Shiny interface where a data set can be uploaded and a summary of several common data quality metrics is reported.

The variables are first summarized in a univariate manner to include measures of missingness, skew/heavy tail/outliers, and longitudinal trends/breaks. Pairwise patterns in the data are then summarized via correlations and correlation of missingness patterns. A principal components analysis (PCA) is presented to summarize the effective number of variables in the data set. Multivariate outliers are identified and reported. The example used throughout this document is the `mockstudy` data that is included in the `arsenal` package. The data quality output loads the `e1071` package to get several functions like `kurtosis()` and `skewness()` for the univariate data checks. The `ggplot2` package is also used to provide several summary plots of the results.

## Univariate Data Quality

<figure>
  <img src="figure1.png" alt="Interface before loading sample data">
  <figcaption>Figure 1: Univariate Interface before Loading Sample Data</figcaption>
</figure>

This section introduces the univariate data quality checks of your dataset. The screenshot above (Figure 1) shows the shell of the data quality tab of the univariate section before you load in your dataset. The "N records to Show" input shows where you enter the number of variables you want to assess. The "outlier cutoff" represents the p-value cutoff to determine a significant outlier. The "Plot Trends for" section show trend breaks for the variable of interest. Clicking the "Use sample dataset" in the left pane will load the univariate metrics for the `mockstudy` dataset (the output of which is provided below in Figure 2).

<figure>
  <img src="figure2.png" alt="Univariate results">
  <figcaption>Figure 2: Univariate Results Using N Records (N=10) and Outlier Cutoff (p=0.05)</figcaption>
</figure>

**Missings**: The most common problem in data cleaning is handling missing values. There are several possible options to treat missing data during data analysis (imputation, observation/variable removal, or even creating missingness features for prediction problems), but understanding the reason why the data is missing is the most important aspect. The data quality tool generates the proportion of missing data in your dataset (count and percentage), ordering them from the highest to the lowest proportion.

**Skewness**: Skewness is the degree of distortion from the normal (Gaussian) distribution. It measures the lack of symmetry in data distribution. It differentiates extreme values in one tail versus the other tail in the distribution. If the skewness is between -0.5 and 0.5, then the data is fairly symmetrical. If the data is negatively skewed but between -1.0 and -0.5 or positively skewed but between 0.5 and 1.0, then the data is moderately skewed. However, your data is highly skewed if the skewness is less than -1.0 or greater than 1.0. The data quality tool generates the skewness of each (numeric) variable, in the order of highly skewed to fairly symmetrical. The `e1071` package in R was used to calculate skewness.

**Excess Kurtosis**: Kurtosis is used to describe the extreme values (outliers) in the tails of a distribution. Positive excess kurtosis means that distribution has fatter tails than a normal distribution, so that there is a higher than normal probability of big positive and/or negative values. When calculating kurtosis, a result of +3.00 indicates the absence of kurtosis. For simplicity in its interpretation, kurtosis is often reported as the "excess" kurtosis by adjusting this result to zero; then, any reading greater than zero is referred to as excess kurtosis. This convention is adopted here so that negative numbers indicate a Platykurtic distribution (the distribution is shorter and tails are thinner than the normal distribution). Positive numbers indicate a Leptokurtic distribution (the distribution is longer and tails are fatter than the normal distribution). The data quality tool displays the variables in order of absolute value of excess kurtosis.

**Outliers**: An outlier is an observation that lies an abnormal distance from other values in a random sample from a population. Outliers should be investigated carefully. Often they contain valuable information about the process under investigation or the data gathering and recording process. Before considering the possible elimination of these points from the data, one should try to understand why they appeared and whether it is likely similar values will continue to appear. Sometimes outliers are bad data points caused by data entry errors; however, sometimes they are an important feature of the data generating process that should not be ignored.

**Trend Test**: The trend test shows when there has been an abrupt change at an observation which causes the data to change in mean or a change in other parameters. The data quality tool is able to detect the observations at which the trend breaks happen for each variable. Our algorithm handles both continuous and categorical variables, and the screenshots below shows the difference between the two. The trend test for continuous variables computes separate linear regressions before and after a potential break point (by default every data point is a possible break point if there are fewer than 100 data points, otherwise there are 100 potential break points equally spaced in time). This is equivalent to a linear regression with an interaction with the break point indicator. Statistical significance is determined by computing the F-statistic for significant interaction (in the intercept or trend), and the maximum F-statistic over all possible break points is compared to the null distribution via a permutation test. The trend test for categorical variables works in a very similar manner, with the exception that the F-statistic from the linear regression interaction is replaced with a Chi Square test for equal categorical distributions before and after the break point. The max Chi Square statistic is also then compared to the appropriate null distribution via a permutation test.

<figure>
  <img src="figure3.png" alt="Continuous Trend Test">
  <figcaption>Figure 3: Trend test for continuous variable (`hgb`), with detection of when a break occurs</figcaption>
</figure>

<figure>
  <img src="figure4.png" alt="Categorical Trend Test">
  <figcaption>Figure 4: Trend test for categorical variable (`arm`), with detection of when a break occurs</figcaption>
</figure>

## Pairwise Correlation/Correlation of Missing Values

<figure>
  <img src="figure5.png" alt="Interface before loading sample data">
  <figcaption>Figure 5: Pairwise Interface before Loading Sample Data</figcaption>
</figure>

Pairwise correlations on the variables are computed, and the top N are displayed (where N is determined by user input). Pairwise correlations of missingness indicators are also computed and displayed for the top N variable pairs. PCA is then used to calculate how many redundant variables there are. The table and plot and display the number of components that explain 95%, 97.5%, and 99% of the variability.

<figure>
  <img src="figure6.png" alt="Pairwise results">
  <figcaption>Figure 6: Pairwise results</figcaption>
</figure>

<figure>
  <img src="figure7.png" alt="PCA and Effective Number of Variables">
  <figcaption>Figure 7: PCA results with Effective Number of Variables and PCA plot</figcaption>
</figure>

## Multivariate Outlier Detection by Observation

<figure>
  <img src="figure8.png" alt="Interface before loading sample data">
  <figcaption>Figure 8: Multivariate Interface before Loading Sample Data</figcaption>
</figure>

The third and last tab of the data quality tool generates the multivariate outlier detection by observation. These results can be used to evaluate which observation may be an outlier. Suppose there are p continuous (numeric) variables and q categorical variables. The procedure works by fitting a glm for each of the p continuous variable on the set of q categorical variables. The residuals from each of these p regressions are then "assumed" to have a multivariate normal distribution. A mean and covariance matrix is computed, and the Mahalanobis distances are calculated for each of the n observations. A p-value is computed for each observation with a Chi Square approximation for the Mahalanobis distances. The p-values (which are then approximately uniform under all assumptions above) are then quantile transformed back to standard normal deviates. A normal QQ-plot of these deviates is then produced to illustrate observations that are well beyond expected deviation under the multivariate normal assumption. The basic idea is the same as for a normal probability plot: the deviates should fall on a straight line if the data came from a multivariate normal distribution. Notable outliers (those with a p-value smaller than the specified outlier cutoff) are plotted in red.

<figure>
  <img src="figure9.png" alt="Multivariate Outliers">
  <figcaption>Figure 9: Multivariate Outliers Plot by Observation</figcaption>
</figure>

