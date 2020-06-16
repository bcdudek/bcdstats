Note that the original app was a single app.R file.  I've separated it into
global, server, and ui files.

The original construction of this app created many tools in the global environment.
Several variables are initialized there, and many functions created.

I'm missing something about the logic of why these items are in the global environment.
I've tried to put them in a tools.R file that is sourced from server.R, but this doesn't work.  I am hesitant to release the app to the server until I understand the implications of all these things in global.R.

App works fairly well now.

I added some validation, improved behavior for what happens when distirbution is changed (cleared the samples from the original distribution),  realigned the sidebar panel, added info on the basic population distribution parameters, and did a host of cosmetic/stylistic things.

Still to do:
Finish working out consequences of changing normal mu and sigma.  Need validation for range and missing.  But also want to clear samples when these values change and it is not working as I want right now.

Add ability to change parameters of uniform and negative exponential distribution.

I would also like to add a trimmed mean as an additional statistic to be available.  Perhaps also skewness and kurtosis.

I  would also like to implement additional distributions.  For example, an bizarrely shaped distribution with multiple peaks - perhaps simply an embedded groups example with a mixture distribution.

