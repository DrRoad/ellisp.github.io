---
layout: narrowwithcomments
title: New Zealand general election forecasts
image: /img/general-image.png
---
<p></p>

This page provides experimental probabilistic predictions for the 2017 New Zealand General Election.  It draws on multiple opinion polls, but goes a step beyond a straightforward poll aggregator in that the estimated voting intention from successive polls is used to forecast the chances of each party to actually win seats on election day, taking into account uncertainty.  Polling results are also adjusted to take into account different polling firms' past performance in predicting different parties' results.

This page will be updated periodically as more data become available, or as I change my ideas about modelling strategy.  

- All material changes are described in [this changelog](/elections/changelog.html).  
- Source code for the analysis, including all committed changes, is [available in the nz-election-forecast repository on GitHub](https://github.com/ellisp/nz-election-forecast/)
- Source code for the write up, including all committed changes, is [available in my blog repository on GitHub](https://github.com/ellisp/ellisp.github.io/tree/source/elections)

Let's look at the big picture predicted result, and compare it to what we would have predicted with the same method if we applied it in March 2014 six months before the last election:

<img src='/img/gam-final-chances-bar.svg' width='100%'>
<img src='/img/gam-final-chances-bar-2014.svg' width='100%'>

The scenarios outlined above are defined as:

- *"National led coalition similar to 2014" (or 2011)* - Under this outcome, the National, M&#257;ori, ACT and United Future parties between them would have a majority of seats in Parliament (as per the post-2014 government); but National do not have enough to govern by themselves.  This scenario does not distinguish between the many possible subsets of this outcome (eg many of these simulated results involve one of the coalition partners not being essential to form a government).
- *"NZ First needed to make government"* - In this scenario, neither the National/M&#257;ori/ACT/United Future nor the Labour/Green combination would have a majority of seats, but one or (more frequently) either of them could form a majority with support from New Zealand First.  Note that this includes some scenarios where Labour + Greens + New Zealand First seats exactly tie with National + M&#257;ori + ACT + United Future.
- Other scenarios should be self-explanatory

For simplicity of presentation, no attempt has been made to identify separately all possible scenarios encompassed in the outcomes described above.

## How does it work?

The predictions are simulations based on a model which smooths all polling numbers since the 2014 election and projects the trend forward.  The model controls for "house effects", estimates of the amount that different polling firms over- or under-estimate the party vote of different political parties, based on polling firms' performance in the four previous elections.  This adjustment process (amongst other minor changes) generally slightly increases the expected vote for New Zealand First, and decreases it for the Greens, compared to published poll numbers.

The graphic below shows this smoothing model, and the forecasts up to election day, of the underlying tendency for party vote for each party:

<img src='/img/gam-vote-predictions.svg' width='100%'>

The model provides an estimate of the range of party vote outcomes for each of nine political parties for which there are sufficient polling data to make predictions.  Voting outcomes are uncertain not just because of the sampling error (this is the margin of error, typically around 3.1% for this sort of variable, usually quoted with survey results in the media and is the uncertainty associated with random sampling with no other sources of error), but because of hard-to-determine non-sampling error, and because of genuine changes in voting intention over time.  As more polls become available closer to the election, the prediction intervals provided in the graphic above are expected to narrow.

Party vote is strongly (usually negatively) correlated between parties because parties are competing for the same voters.  I get around this by modelling vote as a multivariate normal distribution on a logit scale.  The mean and covariance matrix of the distribution on election day are estimated from a generalized additive model implemented in Simon Woods' `mgcv` R package.  Simulations are done with `mvrnorm` from Bill Venables' and Brian Ripley's `MASS` R package.  

The simulations take into account not only the uncertainty in the forecast of the underlying party vote, but the randomness associated with individual observations, of which I count election day as one.  This may seem surprising as the opinion polls are based on small samples (usually around 900 respondents) whereas the election result has no sampling error at all.  But if we accept (as I do) that most of the randomness in the eventual result comes from *non-sampling* error it makes sense to treat election day as just yet another random observation - based on the underlying party vote tendency no doubt, but with a healthy dose of individual randomness itself.  Let's put it another way - my gut feel is that most probabilistic models greatly under-estimate the uncertainty in translating from polls to election results (certainly this happened for nearly everyone apart from FiveThirtyEight in the 2016 US Presidential election), so it makes sense to err on the side of uncertainty.

<img src='/img/gam-vote-predictions-density.svg' width='100%'>

The conversion of party vote to seats depends on whether parties exceed the 5% threshold and/or hold an electorate seat.  The key electorate seat outcomes are simulated by a very primitive basis:

* Ohariu is assumed to go to United Future 60% of the time, Labour 40%
* Epsom is assumed to go to ACT 80% of the time, National 20%
* The seven M&#257;ori electorates are estimated to go to Labour proportionately to their 2014 vote in each electorate with probability $$ \frac{Labour2014}{Labour2014 + 0.9 (Mana2014 + Maori2014)}$$, and to Mana (Te Tai Tokerau) or the M&#257;ori party (the other six electorates) other times

These assumptions are not terribly data-driven, but are better than simply assuming that existing electorate representatives stay the same.  Any improvements welcomed; as far as I'm aware there simply aren't data available to do much better than arbitrary assumptions for these individual electorates.

Seat allocation computation from the simulated party vote results uses the Sainte-Lague allocation formula as implemented in my `nzelect` R package, which I am confident matches the approach used by the Electoral Commission.  All the simulated seat allocations are [available for download](/elections/simulations.csv).

The graphic below shows the simulated outcomes in terms of seats for the various parties in relation to eachother.  The numbers in green in the upper right of the chart are correlation coefficients between outcomes in the different simulations; for example, with a correlation of around -0.37, number of Labour and National seats are strongly negatively related to eachother: simulations where National get lots of seats generally means Labour do badly, and vice versa.

<img src='/img/gam-results-pairs.svg' width='100%'>

<img src='/img/gam-results-density.svg' width='100%'>

## Predicting the 2014 election from March 2014 data?

The test for any forecasting method is how it goes at predicting real life results, pretending to come from a position of ignorance.  So I used the same method to predict the results of the 20 September 2014 election, limiting myself to data up to 20 March 2014.  This meant repeating the house effects estimation with a smaller dataset, refitting the models, etc.  I cut a few corners, particularly on the M&#257;ori electorates where I just allocated them 50/50 to Labour or someone else; I can't realistically say what arbitrary guess I would have made three years ago, but I don't think it makes that much difference.  

Some results are shown below.  It's not too bad for a six-month out prediction.  [In the end](http://www.elections.org.nz/news-media/new-zealand-2014-general-election-official-results), the Green party exceeded these expectations with 10.7% of the party vote, and New Zealand First got 8.7% (out performing the polls materially).  Labour under-performed compared to this retrospective prediction, getting only 25.1% of the party vote.  The downwards curve in intent to vote for Labour in that election cycle was only just becoming apparent six months in advance - see the chart below.  The National Party final party vote in 2014 was 47.0%, within the prediction interval.  

<img src='/img/gam-vote-predictions-2014.svg' width='100%'>

If I'd applied this method in March 2014, I would have identified the actual results (which were, very narrowly, a National-led coalition) as the most probable outcome:
<img src='/img/gam-final-chances-bar-2014.svg' width='100%'>

## Reflections

I strongly recommend reading Nate Silver's reflections on [The Real Story of 2016](https://fivethirtyeight.com/tag/the-real-story-of-2016/) about polls, forecasts and political analysis in the 2016 US Presidential election, much of which is relevant in other electoral situations.  It includes this gem:

> "...there are real shortcomings in how American politics are covered, including pervasive groupthink among media elites, an unhealthy obsession with the insider’s view of politics, a lack of analytical rigor, a failure to appreciate uncertainty, a sluggishness to self-correct when new evidence contradicts pre-existing beliefs, and a narrow viewpoint that lacks perspective from the longer arc of American history."

## Caveat and disclaimer

This page is not associated with any political party, media or commentator.  I have made every effort to provide a transparent, technical probabilistic forecast of the election results and limit any subjective judgement to technical matters relating to model building.  No political judgement or interpretation is to be inferred from these forecasts.  Even more than is always the case, this page has no connection whatsoever to my day job.

Non-politicised corrections, reactions or suggestions are welcomed - use the comments section below or [log an issue with the source code repository on GitHub](https://github.com/ellisp/nz-election-forecast/issues).  There are some known areas for follow up already such as:

* weight polling observations proportionately to sample size - I only haven't done this because I don't have the sample size data convenient to hand.  If/when I add sample sizes to the `nzelect` R package I will incorporate it into the analysis here too.
* compare this simple approach to a more formally specified Bayesian latent state space model.  I'm keen on doing this but it's less familiar territory for me; and I'm suspicious that it will be extremely computationally intensive (eg multiple days of processing).  Will get around to it sooner or later.

