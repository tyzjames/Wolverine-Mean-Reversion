## Wolverine-Backtest
###Back-testing mean reversion and statistical arbitrage strategies

A platform built on the R Shiny Dashboard to display results of a mean reversion strategy using two 
positively or negatively correlated assets.

####Strategy
 - Take 2 correlated assets (i.e Gold and Silver) and calculate their relative value based on their price movements over
the past <b>x</b> bars
 - If the relative value of Gold rises more than that of Silver, long silver (since we might expect Silver to increase) or 
 short Gold (since we might expect Gold to drop) until the relative prices reverts to its mean.
 - The intuition is that since the assets are highly correlated, a relative price increase of one asset will cause the asset pair
 to deviate from its mean, and we can expect the asset pair to eventually revert back to its mean.
