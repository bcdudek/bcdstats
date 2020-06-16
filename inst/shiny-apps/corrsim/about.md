## Tools for Statistics Instruction using R and Shiny
========================================================

Author:  Bruce Dudek at the University at Albany.

The simulation approach in this application simulates samples drawn from a bivariate normal distribution, where the means, sd's, rho, and n are specified by the user.  The randomly drawn sample results are displayed in the scatterplot along with the sample pearson product-moment correlation.

If the user wants to see the same kind of scatterplot with their own data, the data upload approach permits this with upload of a .csv file.

For both appraoches, the scatterplots emphasize examination of the "rug plots" of both the raw Y values and the Yhat values.  The user can explore how the dispersion of the Yhat values depends on the size of the pearson product-moment correlation.

Built using <a href="http://www.rstudio.com/shiny" target="_blank"> Shiny </a> by <a href="http://www.rstudio.com/" target="_blank">Rstudio </a> and <a href="http://www.r-project.org/" target="_blank">R</a>, the Statistical Programming Language.

The correlation simulation uses the <em>rmvnorm</em> function in the <a href="http://cran.r-project.org/web/packages/mvtnorm/index.html" target="_blank">mvtnorm</a> package in R.

Ver 1.6, Jan 23, 2017



