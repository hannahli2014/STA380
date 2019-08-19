Market Segmentation
================
Puja Subramaniam
8/17/2019

``` r
rm(list=ls())
library(flexclust)
```

    ## Warning: package 'flexclust' was built under R version 3.6.1

    ## Loading required package: grid

    ## Loading required package: lattice

    ## Loading required package: modeltools

    ## Loading required package: stats4

``` r
library(ggplot2)
setwd("/Users/pujas/OneDrive/Documents")
social = read.csv("social_marketing.csv", header = TRUE, stringsAsFactors = FALSE, row.names = 1)

library(ggcorrplot)
```

    ## Warning: package 'ggcorrplot' was built under R version 3.6.1

``` r
cormat <- round(cor(social), 2)
ggcorrplot(cormat, hc.order = TRUE)
```

![](Market-Segmentation_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->
Hard to read the correlation plot, but we see that there’s definitely
some correlation present between words

\#PRE-PROCESSING

*Data cleaning*

``` r
# Remove chatter, uncategorized, spam, and adult
social = social[,c(-1,-5,-35,-36)]

# First normalize phrase counts to phrase frequencies.
Z = social/rowSums(social)
#gives us frequencies of the data used versus the actual counts

#standardize Z
Z_std = scale(Z, center=TRUE, scale=FALSE)
plot(Z_std)
```

![](Market-Segmentation_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

\#ANALYSIS

``` r
# PCA
pc = prcomp(Z, scale=TRUE)
#PC1 accounts for ~8% of the variation in the model, PC2 for ~7%, etc.

pr_var <-  pc$sdev ^ 2
pve <- pr_var / sum(pr_var)
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b')
```

![](Market-Segmentation_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
#x axis shows # of principal components, y axis shows proportion of variance 
loadings = pc$rotation
```

Loadings describe the weight given to each raw variable in calculating
the new PC

Positive values contribute positively to the component. Negative values
mean negative relationship. Larger numbers mean stronger relationship

``` r
scores = pc$x
plot(pc, main='Variance by PC')
```

![](Market-Segmentation_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Looking at the plot, we see that the ‘knee’ of the graph happens at
around 7, so we will look at 7 PC components

``` r
pc7 = prcomp(Z, scale=TRUE, rank=7) #stops after 7 prinicipal components
loadings = pc7$rotation
#PC1: 
#highest: sports_fandom, food, family, health_nutrition, cooking, religion, parenting, personal fitness
scores = pc7$x

barplot(pc7$rotation[,1], las=3)
```

![](Market-Segmentation_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Next , we want to see how the individual PCs are loaded on the original
variables

``` r
# The top words associated with each component
o1 = order(loadings[,1], decreasing=TRUE)
o2 = order(loadings[,2], decreasing=TRUE)
o3 = order(loadings[,3], decreasing=TRUE)
o4 = order(loadings[,4], decreasing=TRUE)
o5 = order(loadings[,5], decreasing=TRUE)
o6 = order(loadings[,6], decreasing=TRUE)
o7 = order(loadings[,7], decreasing=TRUE)

# Return the top 5 variables with the highest loadings for each PC
colnames(Z)[tail(o1,4)]
```

    ## [1] "fashion"          "personal_fitness" "health_nutrition"
    ## [4] "cooking"

``` r
colnames(Z)[tail(o2,4)]
```

    ## [1] "tv_film"     "college_uni" "travel"      "politics"

``` r
colnames(Z)[tail(o3,4)]
```

    ## [1] "personal_fitness" "health_nutrition" "news"            
    ## [4] "politics"

``` r
colnames(Z)[tail(o4,4)]
```

    ## [1] "politics" "cooking"  "beauty"   "fashion"

``` r
colnames(Z)[tail(o5,4)]
```

    ## [1] "news"    "beauty"  "cooking" "fashion"

``` r
colnames(Z)[tail(o6,4)]
```

    ## [1] "crafts"  "travel"  "art"     "tv_film"

``` r
colnames(Z)[tail(o7,4)]
```

    ## [1] "sports_playing" "dating"         "travel"         "computers"
