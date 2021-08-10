<script type="text/javascript" async
src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-MML-AM_CHTML
"></script>

## Comments on pvalue and effect size distributions and Formulas
========================================================

### Background:

•	The primary purpose of the app is to enable the viewer to visualize the sampling distribution of p values from the one-sample t-test, under different population parameters and sample sizes.

•	It is convenient to permit adjustment of the difference between the alternative population mean $\left( {{\mu }_{1}}\,or{{\mu }_{alt}} \right)$ and the null hypothesis population mean  $\left( {{\mu }_{0}}\,or{{\mu }_{null}} \right)$ by fixing the null value at 100 and permitting the user to specify the mean of the alternative.  Two-tailed tests are employed in the simulation.

•	Sigma is set at 15 by default since many standardized tests are normed to have $\mu =100$ and $\sigma =15$, but this parameter can also be adjusted.

•	The simulation then produces the chosen number of replicated samples where the standard 1-sample t-test is computed and 2-sided p-values produced.  This permits the visualization of the simulated sampling distribution of the p values from these numerous replicated t-tests.

•	Interesting additional information is provided with the theoretical power and the estimated power based on the number of “significant” t-tests found in the simulations.   Alpha level can also be specified.  This inclusion of theoretical and empirical power also permits understanding how, for a specified effect size, power depends on sample size.  In the simulation, the empirical power estimate is usually very close to the theoretical power when the number of simulations is in the thousands.  The theoretical power is found by passing the population parameters and sample size to the `pwr.t.test` function from the **pwr** package in R.

•	The conclusions about the p-value sampling distribution are based on an assumption that the population distribution is normal.  This is accomplished in the simulation by randomly sampling from a  normal distribution using the `pnorm` function in R.

### Effect Size Calculations:

**Cohen’s d and Hedges g**

The app presents the theoretical value of *Cohen’s d* for the parameters specified, using the following expression:

$\delta =\frac{{{\mu }_{Alt}}-{{\mu }_{null}}}{\sigma }$, 

where $\sigma$ is fixed at 15 and is the standard deviation of the populations.

Effect size indices from each sample were calculated and then averaged to provide a sense that the simulation converges on the theoretical value.

In each of the samples the *Cohen’s d* estimate is calculated as:

 $d=\frac{\bar{X}-{{\mu }_{null}}}{s}$,

where s is the standard deviation of each sample – Cohen’s original definition.

The value of *Cohen’s d* is known to be biased upward (Hedges 1981).  Different adjustment methods have been proposed.  An approximate method was originally proposed by Hedges and a revision by Durlak (2009).  Neither of those were employed in this app to produce the Hedges g statistic.  The reason that the approximations were recommended is that the exact correction for bias was seen to be computationally complex, involving use of the gamma function.  This is no longer an obstacle since R provides a `gamma` function.  So, the correction to obtain *Hedge’s g* statistic in each sample involved calculating *d* and then adjusting it as follows:

$d*(\frac{\Gamma (m)}{\sqrt{m}*\left( \Gamma (m-.5 \right)}$,

where m is defined as df/2 and df is the degrees of freedom for each t-test $(n-1)$ 



### References

Hedges (1981), "Distribution Theory for Glass's Estimator of Effect Size and Related Estimators", Journal of Educational Statistics, Vol. 6, No. 2, pp. 107-128.

Cohen (1988), "Statistical Power Analysis for the Behavioral Sciences", 2nd Edition,  L. Erlbaum

Durlak (2009), "How to Select, Calculate, and Interpret Effect Sizes", Journal of Pediatric Psychology, Vol. 34, No. 9, pp. 917-928.
