<script type="text/javascript" async
src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-MML-AM_CHTML
"></script>

#### Tools for Statistics Instruction using R and Shiny
========================================================

Author:  Bruce Dudek at the University at Albany.

Assistance In R coding was provided by Jason Bryer, University at Albany and Excelsior College.

Built using <a href="http://www.rstudio.com/shiny" target="_blank"> Shiny </a> by <a href="http://www.rstudio.com/" target="_blank">Rstudio </a> and <a href="http://www.r-project.org/" target="_blank">R</a>, the Statistical Programming Language.


The purpose of this app is to provide a visualization that aids in the proper conceptualization of confidence intervals.  It is illustrated with confidence intervals for a sample mean.  Key aspects of the conceptualization are:             
 
1.  Confidence intervals are centered on the observed sample mean.

2.  With simulation, we can show what happens when repeated samples are drawn from the same population distribution.  The sample mean from these simulated samples will vary according to its own sampling distribution.

3.  Since confidence intervals are centered on the sample mean, these intervals also vary in the region of the Random Variable scale that they span.

4.  If a Confidence level of 95% is chosen, we expect approximately 95% of the simulated intervals to overlap the true location of the population mean.

5.  In our simulation, we have specified the true population mean so we can make this comparison to the "confidence" level.  But in realistic analysis we don't know the true value of mu, but have some "confidence" about its location provided by the calculated CI.  For example, we will know that with a CI of 95%, that 95% of the time, if we repeated the sample, our computed CI would overlap the true value of mu.

6.  When sigma (population SD) is known, then the confidence interval can be found using std normal Z deviates based on the CI level, IF WE CAN ASSUME THAT THE SAMPLING DISTRIBUTION OF THE MEAN IS NORMAL.

7.  This CI calculation is:  xbar +/- Z*SEM, where SEM is the sd/(sqrt N)
 
?\[\widehat {Y_i}\, = \,a\, + {b_1}{X_{1i}}\, + \,{b_2}{X_{2i}}\]? 
 

This app was inspired by a type of plot found in ????????????????????????  This type of plot has been dynamically implemented in a javascript applet from the <a href="http://www.rossmanchance.com/applets/ConfSim.html" target="_blank">Rossman/Chance collection</a>.  R/Shiny code to create a rudimentary version was created by Tyler Hunt and is available in a <a href="https://github.com/JackStat/ConfidenceIntervals" target="_blank">GitHub repository</a>.  The current app has partly modeled on Hunt's code, and has expanded the approach with several additional features.

Ver 1.0, June 28, 2017
