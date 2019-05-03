---
title: "README"
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Binomial Package

##Introduction

The package "binomial" implements functions for calculating probabilities of a Binomial random variable, and related calculations such as the probability distribution, the expected value, variance, etc.

##Implementation

the binomial package allows a user to create a binomial random variable object with bin_var then perform a variety of calculations with the binomial random variable object.

##Other Uses

Different summary statistics/metrics are available for the given random variable object that can be accessed by using the function summary()

##Plotting Functionality

The pdf and cdf of the binomial random variable object is also accessible through the respective functions bin_distribution() and bin_cumulative()

finally a plot of the pdf and cdf are accessed by the plot() function.
