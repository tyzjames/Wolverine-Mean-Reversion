require(quantmod)
require(PerformanceAnalytics)

# Step 1: Get the data
getSymbols("^GSPC")

# Step 2: Create your indicator
dvi <- DVI(Cl(GSPC))

# Step 3: Construct your trading rule
sig <- Lag(ifelse(dvi$dvi < 0.5, 1, -1))

# Step 4: The trading rules/equity curve
ret <- ROC(Cl(GSPC))*sig
ret <- ret['2009-06-02/2010-09-07']
eq <- exp(cumsum(ret))
plot(eq)

# Step 5: Evaluate strategy performance
table.Drawdowns(ret, top=10)
table.DownsideRisk(ret)
charts.PerformanceSummary(ret)