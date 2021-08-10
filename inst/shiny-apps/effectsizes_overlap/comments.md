<script type="text/javascript" async
src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-MML-AM_CHTML
"></script>

## Comments/Formulas on Effect Size and Overlap Indices
========================================================

### **Background**:

This app presents a visual representation of two hypothetical population distributions, differing only in their means.  The distributions are normal.  The context permits understanding of how differences in the population means relates to effect sizes and distribution overlaps.  These relationships are a core part of understanding the inferential tests about the mean difference (typically the so-called “t-test”).  Both distributions have the same variance and standard deviation ($\sigma$).  The difference between the two means is controlled with the slider that permits specifying Cohen’s d ($\delta$).  The viewer can then examine how changing d relates to the distance that the “treatment group” mean is from the “control group” mean of 100.

### **Formulas and coding**.

#### **Effect Sizes**:

*Cohen’s d*, the standardized mean difference, is written in Greek symbols for the population definition:

$\delta =\frac{{{\mu }_{2}}-{{\mu }_{1}}}{\sigma }$ 

Given that the population standard deviation is specified, as is ${{\mu }_{1}}$(100), the value of ${{\mu }_{2}}$ can be found, and then the ${{\mu }_{2}}-{{\mu }_{1}}$ is also calculable:

${{\mu }_{2}}={{\mu }_{1}}+\delta \sigma =100+\delta *15$ 

And the raw scale mean difference can be seen as a type of effect size:

Mean difference=${{\mu }_{2}}-100$ 

*Eta squared* is the proportion of total variation accounted for by the grouping factor.  It is algebraically related to Cohen’s d and was calculated that way here.  This form also works for sample data when the two sample sizes are equal.

${{\eta }^{2}}=\frac{{{\delta }^{2}}}{{{\delta }^{2}}+4}$


#### **Overlap Indices**:

Several different ways of characterizing the degree of distribution overlap are possible.  Cohen (1988) described two commonly used ones, U1 and U3.  Cohen’s U1 has been the topic of discussion and an alternative from Rom (1996????) is also provided along with an index of “superiority”.

*Cohen’s U1* describes the non-overlap of the two distributions (expressed here as a percentage).  Its computation has been the discussion of several articles critical of Cohen’s approach (discussed below).  

It is calculated as follows:

First, define U2, and then use it to calculate U1:

U2 =  $\Pi (\delta /2)$

U1 = $100*(((2*U2)-1)/U2)$

R code:
   `u2 <- pnorm(cd/2)`
  `u1 <- 100*(((2*u2)-1)/u2)`

*Rom’s non-overlap index* is an alternative to Cohen’s U1.  See the discussion below and the references to understand that Rom’s overlap is probably a better way of describing overlap/non-overlap in terms of areas under the curves.  Rom’s original index is an index of overlap.  I changed it to non-overlap so that a direct comparison to Cohen’s U1 is possible.

Rom’s non-overlap = \[\text{100-(100*(2*}\Pi \text{(-}\delta \text{/2)))}\] 

R code:  `100-(100*(2*pnorm(-cd/2)))`

*Cohen's U3* describes the proportion/percentage of distribution 2 exceeds the mean of distribution 1 - in our illustration, the mean of the second population is always larger than the mean of population 1.  Thus when the distributions have the same mean, U3 will be 50%.  U3 has been described as an intuitively comfortable way of describing the degree to which distributions overlap (or don’t overlap).   It is calculated here as a percentage.  The approach uses Cohen’s d, which can be seen as a standardization of the difference between the two means – thus d is envisioned as the mean of a standardized population 2 and a standardized population1 would have a mean of zero.  Thus:

U3 = 100*($\Pi (d)$) , where $\Pi$ is a cumulative left tail normal deviate.

R code for this is:  `100*(pnorm(d, mean=0, sd=1)`

*CL, the “superiority” index*, also called the Common Language Effect Size:

CL is a probability of “superiority” suggested by McGraw and Wong (1992). It provides the probability that a randomly drawn score from distribution 2 is larger than a randomly drawn score from distribution 1.  If the distributions have the same mean (and sigma, and in sample data if the sample sizes are equal), then CL will be .5.  You can see from the app that as the mean of population2 rises, CL also increases.

$CL=\Pi (\delta /\sqrt{2})$

R code:  `pnorm(d/(2^.5))`

Note that calculation of a non-parametric approach to the “probability of superiority” has been recommended by Ruscio (2008) – but it is applicable for sample data, not the population systems depicted in this app.

### **The debate over Cohen’s U1**:

Several authors have described an interpretation that Cohen’s U1 is flawed and showed that it is not a calculation of the area of overlap/nonoverlap.  It is not our purpose here to explain the details of that perspective; citations are provided.  A reasonable summary and graphical depiction is provided in Grice and Barrett (2014) article.  The essential issue is that Cohen apparently did not grasp distinctions between frequency count ways of looking at overlapping distributions and relative areas under the curves ways of looking at overlap.  The method provided by Rom (1996) uses the more appropriate area perspective.  Linacre’s 1995 work and an online graphical display provide a way of determining Rom’s overlap when distribution sigma’s differ.

The Fritz, Hanel, Ruscio and Walker papers provide further guidance on these overlap indices.  The Hedges and Olkin paper is another detailed resource on overlap indices.

### **References**

Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). L. Erlbaum Associates. 

Fritz, C. O., Morris, P. E., & Richler, J. J. (2012). Effect size estimates: current use, calculations, and interpretation. Journal of experimental psychology: General, 141(1), 2. 

Grice, J. W., & Barrett, P. T. (2014). A note on Cohen's overlapping proportions of normal distributions. Psychological Reports, 115(3), 741-747. 

Hanel, P. H., & Mehler, D. M. (2019). Beyond reporting statistical significance: Identifying informative effect sizes to improve scientific communication. Public Underst Sci, 28(4), 468-485. https://doi.org/10.1177/0963662519834193 

Hedges, L. V., & Olkin, I. (2016). Overlap between treatment and control distributions as an effect size measure in experiments. Psychol Methods, 21(1), 61-68. https://doi.org/10.1037/met0000042 

Linacre, J. M. Overlapping Normal Distributions. Retrieved July 2022 from https://www.rasch.org/rmt/rmt101r.htm 

Linacre, J. M. (1995). Rasch measurement transactions. MESA Press Chicago. 

McGraw, K. O., & Wong, S. P. (1992). A common language effect size statistic. Psychological Bulletin, 111, 361-365. 

Rom, D. M., & Hwang, E. (1996). Testing for individual and population equivalence based on the proportion of similar responses. Statistics in medicine, 15(14), 1489-1505. 

Ruscio, J. (2008). A probability-based measure of effect size: robustness to base rates and other factors. Psychological methods, 13(1), 19. 

Ruscio, J., & Mullen, T. (2012). Confidence intervals for the probability of superiority effect size measure and the area under a receiver operating characteristic curve. Multivariate Behavioral Research, 47(2), 201-223. 

Walker, D. A. (2005). Bias Affiliated With Two Variants Of Cohen’s d When Determining U1 As A Measure Of The Percent Of Non-Overlap. Journal of Modern Applied Statistical Methods, 4(1), 100-105. https://doi.org/10.22237/jmasm/1114906260 

