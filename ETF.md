ETFs
================

``` r
library(mosaic)
library(quantmod)
library(foreach)
```

### PORTFOLIO \#1: Aggressive

``` r
rm(list=ls())
set.seed(512)
mystocks1 = c("VTI", "QQQ", "IVV", "SHYG", "HYG")
myprices = getSymbols(mystocks1, from = "2014-01-01")

# creating new object with adjusted values for each ticker
for(ticker in mystocks1) {
    expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
    eval(parse(text=expr))
}

all_returns = cbind(ClCl(VTIa),
                                ClCl(QQQa),
                                ClCl(IVVa),
                                ClCl(SHYGa),
                                ClCl(HYGa))
head(all_returns)
```

    ##                ClCl.VTIa    ClCl.QQQa     ClCl.IVVa    ClCl.SHYGa
    ## 2014-01-02            NA           NA            NA            NA
    ## 2014-01-03 -0.0002103912 -0.007218953 -0.0004347699  0.0003957261
    ## 2014-01-06 -0.0026299180 -0.003693433 -0.0027184256  0.0001977650
    ## 2014-01-07  0.0064339206  0.009267875  0.0061604157  0.0013842001
    ## 2014-01-08  0.0007335988  0.002180842  0.0005418617  0.0003949645
    ## 2014-01-09  0.0008378050 -0.003321510  0.0004873605 -0.0003948085
    ##                ClCl.HYGa
    ## 2014-01-02            NA
    ## 2014-01-03 -0.0003224312
    ## 2014-01-06  0.0021502741
    ## 2014-01-07  0.0000000000
    ## 2014-01-08 -0.0006436756
    ## 2014-01-09  0.0015029415

``` r
all_returns = as.matrix(na.omit(all_returns))

#looping over four trading weeks
total_wealth = 100000
weights = c(0.25, 0.25, 0.25, 0.125, 0.125)
holdings = weights * total_wealth
n_days = 20
wealthtracker = rep(0, n_days) 
for(today in 1:n_days) {
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
}
total_wealth
```

    ## [1] 98259.41

``` r
plot(wealthtracker, type='l')
```

![](ETF_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
# creating histogram to get VaR
initial_wealth = 100000
sim1 = foreach(i=1:5000, .combine='rbind') %do% {
    total_wealth = initial_wealth
    weights = c(0.25, 0.25, 0.25, 0.125, 0.125)
    holdings = weights * total_wealth
    n_days = 20
    wealthtracker = rep(0, n_days)
    for(today in 1:n_days) {
        return.today = resample(all_returns, 1, orig.ids=FALSE)
        holdings = holdings + holdings*return.today
        total_wealth = sum(holdings)
        wealthtracker[today] = total_wealth
    }
    wealthtracker
}

# Profit/loss
mean(sim1[,n_days])
```

    ## [1] 100905.3

``` r
hist(sim1[,n_days]- initial_wealth, breaks=30)
```

![](ETF_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
h = (sim1[,n_days] - initial_wealth)
quantile(h, 0.05)
```

    ##       5% 
    ## -4452.19

``` r
# 5% is -$4520.18
```

For the first portfolio, we decided to make it a high growth portfolio
by picking three large capital growth equities ETFs to make up 75% of
the portfolio, and two high yield bonds ETFs to comprise the other 25%
of the portfolio. We structured it this way instead of having 100% of
the high growth stocks because the bonds act as a buffer since there is
lower risk with bonds. So we wanted to be aggressive, but not too
aggressive. We selected Vanguard Total Stock Market ETF (VTI), iShares
Core S\&P 500 ETF (IVV), Invesco QQQ (QQQ), ISHARES TR/0-5 YR HIGH YIELD
CORP B (SHYG), and iShares iBoxx $ High Yid Corp Bond (HYG). We decided
to pick these five ETFs based on the total assets each had, and the best
daily change rate. The value at risk at the 5% level is a loss of
$4520.20. This means that if there is no trading over the period, there
is a 5% chance that this portfolio will fall in value by over $4520.
This high amount is expected since this is a high growth portfolio;
since there is more of a return on this portfolio, there is also a
higher risk.

### PORTFOLIO \#2: Conservative

``` r
rm(list=ls())
set.seed(512)
mystocks2 = c("SHV", "SHY", "VGSH", "BOND", "VTI", "VXUS")
myprices = getSymbols(mystocks2, from = "2014-01-14")
```

    ## pausing 1 second between requests for more than 5 symbols
    ## pausing 1 second between requests for more than 5 symbols

``` r
for(ticker in mystocks2) {
  expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
  eval(parse(text=expr))
}
all_returns = cbind(ClCl(SHVa),
                    ClCl(SHYa),
                    ClCl(VGSHa),
                    ClCl(BONDa),
                    ClCl(VTIa),
                    ClCl(VXUSa))
head(all_returns)
```

    ##                ClCl.SHVa     ClCl.SHYa    ClCl.VGSHa    ClCl.BONDa
    ## 2014-01-14            NA            NA            NA            NA
    ## 2014-01-15  0.000000e+00 -0.0003553542 -0.0011501807 -0.0023687701
    ## 2014-01-16 -9.072109e-05  0.0002370542  0.0006580194  0.0037990407
    ## 2014-01-17  9.072932e-05  0.0000000000 -0.0001644254  0.0028384520
    ## 2014-01-21 -9.072109e-05 -0.0001184931  0.0008220816 -0.0009434758
    ## 2014-01-22  0.000000e+00 -0.0004739455 -0.0003285855 -0.0027387006
    ##                ClCl.VTIa    ClCl.VXUSa
    ## 2014-01-14            NA            NA
    ## 2014-01-15  0.0050172887  0.0021193833
    ## 2014-01-16 -0.0007280291  0.0001923092
    ## 2014-01-17 -0.0038509887 -0.0036524029
    ## 2014-01-21  0.0033434333  0.0036657919
    ## 2014-01-22  0.0018744143  0.0030757401

``` r
all_returns = as.matrix(na.omit(all_returns))
return.today = resample(all_returns, 1, orig.ids=FALSE)
total_wealth = 100000
weights = c(0.2, 0.2, 0.2, 0.2, 0.15, 0.05)
holdings = weights * total_wealth
n_days = 20
wealthtracker = rep(0, n_days) # Set up a placeholder to track total wealth
for(today in 1:n_days) {
  return.today = resample(all_returns, 1, orig.ids=FALSE)
  holdings = holdings + holdings*return.today
  total_wealth = sum(holdings)
  wealthtracker[today] = total_wealth
}
total_wealth
```

    ## [1] 100787.7

``` r
plot(wealthtracker, type='l')
```

![](ETF_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# Now simulate many different possible scenarios  
initial_wealth = 100000
sim1 = foreach(i=1:5000, .combine='rbind') %do% {
  total_wealth = initial_wealth
  weights = c(0.2, 0.2, 0.2, 0.2, 0.15, 0.05)
  holdings = weights * total_wealth
  n_days = 20
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days) {
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
  }
  wealthtracker
}

# Profit/loss
mean(sim1[,n_days])
```

    ## [1] 100251.3

``` r
hist(sim1[,n_days]- initial_wealth, breaks=30)
```

![](ETF_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
h = (sim1[,n_days] - initial_wealth)
quantile(h, 0.05)
```

    ##        5% 
    ## -933.2883

``` r
# 5% quantile: -933.29 loss
```

For the second portfolio, we took a more conservative approach, with 80%
made up of government bond ETFs and 20% made up of stock ETFs. These
ETF’s include the iShares Short Treasury Bond ETF (SHV), iShares 1-3
Year Treasury Bond ETF (SHY), Vanguard Short-Term Treasury ETF (VGSH),
PIMCO Active Bond Exchange-Traded Fund (BOND), Vanguard Total Stock
Market ETF (VTI), and Vanguard Total International Stock ETF (VXUS). We
picked government bonds because they are known to be low-risk
investments since the issuing government backs them. We paired it with
two large growth ETFs to slightly increase its risk. As a result, the
value at risk at the 5% level is a loss of -$935.28. This is
considerably lower than the previous example because this is a
conservative, low risk portfolio, so it is understandable that the risk
of loss would be lower.

### PORTFOLIO \#3: Diversified

``` r
rm(list=ls())
set.seed(512)
mystocks3 = c("AOR", "FXE", "LQD", "VHT", "USO", "IYW", "DBC", "VFH", "INDA", "VNQ")
myprices = getSymbols(mystocks3, from = "2014-01-14")
```

    ## pausing 1 second between requests for more than 5 symbols
    ## pausing 1 second between requests for more than 5 symbols
    ## pausing 1 second between requests for more than 5 symbols
    ## pausing 1 second between requests for more than 5 symbols
    ## pausing 1 second between requests for more than 5 symbols
    ## pausing 1 second between requests for more than 5 symbols

``` r
for(ticker in mystocks3) {
  expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
  eval(parse(text=expr))
}

all_returns = cbind(ClCl(AORa),
                    ClCl(FXEa),
                    ClCl(LQDa),
                    ClCl(VHTa),
                    ClCl(USOa),
                    ClCl(IYWa),
                    ClCl(DBCa),
                    ClCl(VFHa),
                    ClCl(INDAa),
                    ClCl(VNQa))
head(all_returns)
```

    ##                ClCl.AORa     ClCl.FXEa     ClCl.LQDa     ClCl.VHTa
    ## 2014-01-14            NA            NA            NA            NA
    ## 2014-01-15  0.0025980253 -0.0058418548  0.0002608382  9.552096e-05
    ## 2014-01-16 -0.0007773776  0.0013388351  0.0023469836  2.960256e-03
    ## 2014-01-17 -0.0010373703 -0.0070568789  0.0011274304 -3.808531e-04
    ## 2014-01-21  0.0018172378  0.0024687814 -0.0013860273  6.191075e-03
    ## 2014-01-22  0.0005182949 -0.0007463134 -0.0024288602  3.786539e-04
    ##               ClCl.USOa     ClCl.IYWa    ClCl.DBCa    ClCl.VFHa
    ## 2014-01-14           NA            NA           NA           NA
    ## 2014-01-15  0.022094400  0.0126425449  0.002816901  0.010105570
    ## 2014-01-16 -0.003553391 -0.0004458923 -0.001605177 -0.004891107
    ## 2014-01-17  0.001188618 -0.0076948256  0.001205828 -0.002680943
    ## 2014-01-21  0.008904809  0.0048325577  0.002408631  0.002688150
    ## 2014-01-22  0.016769608 -0.0002237445  0.004805807  0.002010724
    ##              ClCl.INDAa    ClCl.VNQa
    ## 2014-01-14           NA           NA
    ## 2014-01-15  0.006865913  0.007557436
    ## 2014-01-16 -0.003610108  0.001800105
    ## 2014-01-17 -0.013687601 -0.005840072
    ## 2014-01-21 -0.001224531  0.009037491
    ## 2014-01-22  0.012259951  0.004329019

``` r
all_returns = as.matrix(na.omit(all_returns))


# simulating a random day
return.today = resample(all_returns, 1, orig.ids=FALSE)

# Now loop over four trading weeks
total_wealth = 100000
weights = c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)
holdings = weights * total_wealth
n_days = 20
wealthtracker = rep(0, n_days) # Set up a placeholder to track total wealth
for(today in 1:n_days) {
  return.today = resample(all_returns, 1, orig.ids=FALSE)
  holdings = holdings + holdings*return.today
  total_wealth = sum(holdings)
  wealthtracker[today] = total_wealth
}
total_wealth
```

    ## [1] 102065.4

``` r
plot(wealthtracker, type='l')
```

![](ETF_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
# Now simulate many different possible scenarios  
initial_wealth = 100000
sim1 = foreach(i=1:5000, .combine='rbind') %do% {
  total_wealth = initial_wealth
  weights = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,0.1,0.1,0.1)
  holdings = weights * total_wealth
  n_days = 20
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days) {
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
  }
  wealthtracker
}

# Profit/loss
mean(sim1[,n_days])
```

    ## [1] 100345.4

``` r
hist(sim1[,n_days]- initial_wealth, breaks=30)
```

![](ETF_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
h = (sim1[,n_days] - initial_wealth)
quantile(h, 0.05)
```

    ##       5% 
    ## -4101.95

``` r
#VaR 5%: -4101.95
```

The last portfolio was comprised of ten ETF’s that were all very
different to have a diversified portfolio since research has shown that
those are more likely to have a higher return. The ETF’s range from oil
& gas to health/biotech to international equity ETF’s. Each of them were
given a 10% weight in the portfolio. The value at risk at the 5% level
is a potential loss of $4101.95. This high amount is expected because
each ETF within the portfolio carries various amounts of risk, so when
they are put together into one, it is a risky profile overall. But the
outlook four week outlook looked better for this portfolio than the
previous two as the investment is predicted to increase over time.
