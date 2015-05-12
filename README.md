## Wolverine-Backtest
###Back-testing mean reversion and statistical arbitrage strategies

A platform built on the R Shiny Dashboard to display results of a mean reversion strategy using two 
positively or negatively correlated assets. 

![alt tag](http://i62.tinypic.com/snh37p.jpg)

####Strategy
 - Take 2 correlated assets (i.e Gold and Silver) and calculate their relative value based on their price movements over
the past <b>x</b> bars
 - If the relative value of Gold rises more than that of Silver, long silver (since we might expect Silver to increase) or 
 short Gold (since we might expect Gold to drop) until the relative prices reverts to its mean.
 - The intuition is that since the assets are highly correlated, a relative price increase of one asset will cause the asset pair
 to deviate from its mean, and we can expect the asset pair to eventually revert back to its mean.

###Notes
 - I have tested this strategy with a hedge fund and the strategy is indeed profitable based on mid-close prices and a 1min x 1500 lookback period. However, once using the bid and ask prices, the strategy is no longer profitable. This is excluding the cost per trade.
 - Increasing the timeframe from 1min to 10min or 15min will increase the profit per trade which will overcome the cost per trade. This still needs to be further tested.
