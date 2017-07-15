---
layout: narrowwithcomments
title: New Zealand general election forecasts - changelog
image: /img/gam-vote-predictions.png
socialimage: /img/gam-vote-predictions.png
---
<p></p>

## Significant changes to the [New Zealand election forecasts web page](/elections/elections.html)

<img src='/img/election-forecast-tracking.svg' width = '100%'>

All dates and times are New Zealand time.

- *15 July 2017* Add an extra poll (Roy Morgan).  Model A:National/coalition down to 34, back where it was in mid-June (before what now looks to have been a budget bounce).  Model B: National/coalition down to 51.9.  State space model (Model B)seems to more volatile, which is a further reason for using it as Model B rather than the main predictions.
- *12 July 2017* Add Model B, an alternative forecasting method drawing on a state space model.  Main difference in approach is an assumption that growth and decline in individual parties' results to date will *not* continue up to election day and that latent voting intention is instead a random walk.  The implication of this is a much strong outlook for a National-led coalition - 63% chance instead of 36%.
- *11 July 2017* Add an extra poll (Colmar Brunton).  National or National led coalition back down to 36%.
- *24 June 2017* Add an extra poll (Roy Morgan).  National or National led coalition continues to nudge up, now to 38%.
- *17 June 2017* Add an extra poll. Probability of National or National-led coalition continues to nudge up, now reaching 34%.
- *7 June 2017* Add an extra poll.  Makes National and National-led coalition slightly more favourable than previously, but not really a material change.
- *28 May 2017* Add an extra poll.  Makes no material change to any of the predictions. 
- *30 April 2017* Another change to the estimation of house effects (bias by polling firm - party combination).  I've now switched over to a Bayesian approach with an informative prior, which has the effect of shrinking these estimated biases towards zero compared to the previous method, which just took the average bias of the past 2-4 elections.  This change means the Greens' chances go up a bit and New Zealand First down a bit compared to previous forecasts, and the substantive overall outcome impact is that a National-led coalition ends up with a bit more predicted chance of winning than before.  I also changed the lead graphic in the main summary to make it more compatible with the "build your own coalition" web tool.
- *29 April 2017* Add an extra poll.  Net impact is to very slightly decrease expected Labour vote.  Most probable outcome is still about 80% chance of New Zealand First being needed for a coalition government.  Also, rejigged the main page layout a bit (put all the 2014 predictions together at the end, and the 2017 prediction graphic at the top).
- *16 April 2017* Add an [interactive web app](https://ellisp.shinyapps.io/nz-election-2017/) to "build your own coalition" and to explore the assumptions in individual electorate seats.  Also increased the number of simulations used for the main page from 1,000 to 10,000.
- *9 April 2017* Add more variance in from the process of estimating house effects.  This adds materially to the uncertainty for New Zealand First in particular, which is indicative of the somewhat sketchy record of previous polls in estimating New Zealand First vote.
- *9 April 2017* Add a histogram of individual party seat expectations
- *27 March 2017 19:30* Fixed a problem where parties with extremely low predicted party vote also got extremely high variance, so a small number of simulations allocated them 20-30% of the party vote.  Net impact was to reduce the importance of Mana party marginally.
- *26 March 2017 20:10* Updated with one more opinion poll
- *26 March 2017 18:00* Revised method of simulating the randomness of an election result on top of the predicted latent party support.  Previous method was to make elections as random as an opinion poll; this added too much variation to simulations.  The new method models squared error of forecasts and actual results of previous elections (2014 and before) to provide an estimate of the variance of actual election results around the mean vector of latent party support.  This variance is combined with the correlation between party support from the forecast model to estimate a variance-covariance matrix, on a logit scale, which is used for the simulations of election day party vote.  The impact of this change is to greatly reduce the uncertainty in the model, and hence focus the probability mass on a smaller number of likely outcomes.  For the retrospective "forecast" of the 2014 results, it makes the National Party coalition by far the most likely outcome (as turned out to be the case), even from six months out.  See the [change to the source code](https://github.com/ellisp/nz-election-forecast/commit/5f64a509e10c8ef51ec6538ca9626c0f33c4b1e7).
- *26 March 2017 10:00* Corrected error for minor parties in the retrospective 2014 forecast, which had inserted 50% support for polls which should have indicated 0%.  The impact of fixing this bug was to reduce "National coalition" chances of winning the 2014 election, as seen from six months previously
- *26 March 2017 9:00* Initial public version