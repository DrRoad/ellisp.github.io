---
layout: post
title: Improving state-space modelling of the Australian 2007 federal election
date: 2017-07-01
tag: 
   - VotingBehaviour
   - R
   - Timeseries
description: I revisit my modelling
image: /img/0102a-all-polls-inflator-2.svg
socialimage: http://ellisp.github.io/img/0102-all-polls-inflator-2.png
category: R
---

When I wrote last week about [state-space modelling of the Australian 2007 federal election](/blog/2017/06/24/oz-polls-statespace), I received some very helpful feedback from [Bob Carpenter](http://datascience.columbia.edu/bob-carpenter), a Columbia University research scientist and one of the core developers of the Stan probabilistic language.  Some of the amendments I made in response to his comments are interesting enough (to me anyway) to blog about.

Reminder of what doing here...

## Vectorise probability statements wherever possible

The big eye-opener for me, early in my experience with Stan, was the performance boost from fitting a vector of random variables in a single statement rather than one at a time with a for loop.  As most of my programming is in R, this should come naturally to me, but I'd absorbed the message that because Stan programs are compiled in C++, loops aren't slow in Stan the way they are in R.  It turns out this is correct, but a helpful footnote on page 123 of the helpful [user documentation](https://github.com/stan-dev/stan/releases/download/v2.16.0/stan-reference-2.16.0.pdf) explains:

> Unlike in Python and R, which are interpreted, Stan is translated to C++ and compiled, so loops and
assignment statements are fast. Vectorized code is faster in Stan because (a) the expression tree used to
compute derivatives can be simplified, leading to fewer virtual function calls, and (b) computations that
would be repeated in the looping version, such as log(sigma) in the above model, will be computed once
and reused.

In my first version I had Stan code that looked like

{% highlight stan %}
  for (i in 2:n_days) 
      mu[i] ~ normal(mu[i - 1], 0.25);
{% endhighlight %}

Changing this to

{% highlight stan %}
  mu[2:n_days] ~ normal(mu[1:(n_days - 1)], 0.0025);
{% endhighlight %}

sped up everything remarkably - for my simplest program (without any polling data and hence not a realistic exercise, but still), from 30 minutes to 4 minutes.

## Don't give a parameter two priors

In my original version, I'd forced the value of the latent voting intention `mu` to match the actual 2007 election result with this code (where `n_days` is the number of days from the 2004 election to the 2007 election and `mu_finish` is the 2007 election result):

{% highlight stan %}
mu[n_days] ~ normal(mu_finish, 0.0001);
{% endhighlight %}

Dr Carpenter pointed out that this meant I declared the probability distribution of the final value of `mu` twice - once in the above, and once earlier when I'd specified it as equalling the latent voting intention the day before the election plus or minus a small random change.  In fact, I'd noticed this myself and had been surprised that it works (it does).  But it's much clearer to change from specifying the distribution of `mu` at this point, to specifying the distribution of the election result `mu_finish`:

{% highlight stan %}
mu_finish ~ normal(mu[n_days], 0.0001);
{% endhighlight %}

This doesn't seem to make any difference to performance but it makes more sense.  I hadn't done this originally because I was stuck thinking that the election result `mu_finish` was somehow particularly real and definitive, whereas latent voting intention `mu` was a random variable.  In fact, it makes perfect sense to think of (in this context) the election result as a random variable, very closely tied to the latent support for the ALP, but still imperfectly measured.  So moving it to the left of the probability declaration makes sense.

## Consider longer-tailed distributions

Dr Carpenter pointed out that my original model still showed a lot of short term up and down of latent support for the ALP.  Given there seemed to be a few periods of quite rapid change, he suggested changing the model for the daily change in support from Normal to something like Student's t distribution with four degrees of freedom, which has longer tails allowing those occasional big days of change.  I think this is a good idea but it doesn't seem to make much difference; comparing the chart below with the original in last week's post:

<img src="/img/0102a-all-polls-inflator-1.svg" width = "100%">

## Don't forget total survey error

I quite agreed with the suggestion that the latent voting intention in my original post (which of course was a reproduction of work published elsewhere) was too wiggly for plausibility.  In thinking this through I decided that the problem was that the probabilistic model is taking into account only a simple estimate of sampling error for individual polls (plus the house effects of bias for or against the ALP).  We know beyond reasonable doubt that there's more volatility in voting intention polls than actually in the variable we're trying to define and measure.  For example, [Trump's Up 3! Clinton's Up 9!](http://www.slate.com/articles/news_and_politics/politics/2016/08/don_t_be_fooled_by_clinton_trump_polling_bounces.html) by Andrew Gelman and David Rothschild compellingly explains how *two thirds* of apparent swings are explained by variations in response rates.  Using compelling data from the time of the 2012 US Presidential Election, they show:

> When there was good news for Mitt Romney, more Republicans opted to respond to the poll; when Obama was riding high, Democrats were more likely to respond. 

In the case of the 2007 election, it seems likely that there was a surge of enthusiasm when Mark Latham was replaced with Kevin Rudd as leader of the ALP; and that this would have translated into over-estimates of people's likelihood of actually voting for  the ALP, with ALP voters perhaps relishing the chance of telling a pollster what they think.  

Voter turnout isn't as the source of randomness in Australia (which has compulsory voting) as other jurisdictions, but it is easy to see many sources of survey error other than the simple sampling error usually reported.  It's difficult to know what to do about this issue.  I think the actual uncertainty from these surveys is some multiple (greater than 1) of the sampling error, but it's difficult to quantify (noting for example that the level of this bonus randomness will vary in different circumstances).  Based on my own gut feel and experience, and the scary estimate by Gelman and Rothschild quoted above, I decided to double the variance of polls from the simple estimate (this equates to multiplying confidence intervals by the square root of 2, or around 1.4).

Implementing this has the effect of giving individual polls less pull than in the original model, and we see a smoother point estimate of latent voting intention:

<img src="/img/0102a-all-polls-inflator-2.svg" width = "100%">

Now, there's quite a literature on [total survey error](https://scholar.google.co.nz/scholar?q=total+survey+error&hl=en&as_sdt=0&as_vis=1&oi=scholart&sa=X&ved=0ahUKEwiHkI-nx-fUAhXKoJQKHVVQC3wQgQMIJTAA) filled with detailed advice on all the things that go wrong, how to estimate their impact and how to control for it.  One thing I'm pretty sure it never says is "just multiply the variances by 2 and you should be right."  So few people would recommend this procedure.  But it's surely better than multiplying them by 1; few experienced researchers in this space would think that the best multiplier is closer to 1 than to 2.  Yet by reporting so widely the margin of error from simple sampling error (usually ignoring even design effects from weighting) that's what is done thousands of times every day.s

## Estimate the sampling error from the underlying parameter, not just a single poll

## Avoid hard constraints on parameters

Tried logit and worked ok but I was worried this was giving undue emphasis to low polls and dragging the whole thing down... 
so went back to the original scale.

but then still needed the <lower=0,upper=1> constraint to initiate the chains



{% highlight R %}


{% endhighlight %}





{% highlight stan %}


{% endhighlight %}

