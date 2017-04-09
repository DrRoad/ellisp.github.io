---
layout: narrowwithcomments
title: New Zealand general election forecasts - changelog
image: /img/gam-vote-predictions.png
socialimage: /img/gam-vote-predictions.png
---
<p></p>

## Significant changes to the [New Zealand election forecasts web page](/elections/elections.html)

All dates and times are New Zealand time.

- *9 April 2017* Add a histogram of individual party seat expectations
- *27 March 2017 19:30* Fixed a problem where parties with extremely low predicted party vote also got extremely high variance, so a small number of simulations allocated them 20-30% of the party vote.  Net impact was to reduce the importance of Mana party marginally.
- *26 March 2017 20:10* Updated with one more opinion poll
- *26 March 2017 18:00* Revised method of simulating the randomness of an election result on top of the predicted latent party support.  Previous method was to make elections as random as an opinion poll; this added too much variation to simulations.  The new method models squared error of forecasts and actual results of previous elections (2014 and before) to provide an estimate of the variance of actual election results around the mean vector of latent party support.  This variance is combined with the correlation between party support from the forecast model to estimate a variance-covariance matrix, on a logit scale, which is used for the simulations of election day party vote.  The impact of this change is to greatly reduce the uncertainty in the model, and hence focus the probability mass on a smaller number of likely outcomes.  For the retrospective "forecast" of the 2014 results, it makes the National Party coalition by far the most likely outcome (as turned out to be the case), even from six months out.  See the [change to the source code](https://github.com/ellisp/nz-election-forecast/commit/5f64a509e10c8ef51ec6538ca9626c0f33c4b1e7).
- *26 March 2017 10:00* Corrected error for minor parties in the retrospective 2014 forecast, which had inserted 50% support for polls which should have indicated 0%.  The impact of fixing this bug was to reduce "National coalition" chances of winning the 2014 election, as seen from six months previously
- *26 March 2017 9:00* Initial public version