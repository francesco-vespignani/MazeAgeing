---
title: "Repetition Maze in aging - SAND e AAT"
output: html
author: F.V.
date: November 2025
---

Load libraries

``` r
library(lme4)
library(lmerTest)
library(ggplot2)
library("knitr")
knitr::opts_chunk$set(
    echo = FALSE,
    message = FALSE,
    warning = FALSE,
    fig.align='left',
    fig.width=12,
    fig.height=5,
    fig.pos='H',
    fig.path = "plots/",
    out.width = "800px",
    dev = c("png", "svg"),
    dpi=500
)
```
# Read data exclude errors and exclude RT larger than 3 seconds, coherce factors, create log transofrmed variables
We select only correct sentences from AAT SAND blocks

We consider only correct trials and RT<3000ms.

The three levels of the variable repetition (rep) and encoded as "read" (1st presentation), "rep1" (2nd presentation) and "rep2" (3rd presentation).

In this analysis separate analyses are run for AAT and SAND items. The ITEM in these tests are assumed to become more complex as a function of item order (longer and more complex sentences) and we expect that repetition effects could be more beneficial for larger values of items.  This may be a linear function or not (in the case very difficult items are hard to keep in mind)  we may expect non-linear effects both on reading and on the repetition effects.  To this aim we will run linear mixed models with item as a cartegorical ordered factor which allow to check if linear, quadratic or cubic effects emerge both as main effect of the two variable considered (item and repetition). Since items differ a lot in terms of length word position e word order will not be considered.  We may want to whash out spurious effects of nchar (length in characters) and word frequency but not in interaction with the main variables.




``` r
data =  read.table('data/dataMaioli.tsv', sep='\t', header= TRUE, as.is=TRUE)
fac = c('scode','list','item','rep','POS','corrkey','correct','open')
for (f in fac)  {
    data[[f]]=factor(data [[f]])
}
levels(data$rep) = c('read','rep1','rep2')

d = subset(data, rt<=3000 & correct == TRUE)

d$lrt =  log(d$rt)
d$lff = log(d$countForm)

aat =  subset(d, cond %in% c('AAT'))
aat$item = factor(as.numeric(as.character(aat$item)), ordered=TRUE)
sand = subset(d, cond %in% c('SAND'))
sand$item = factor(as.numeric(as.character(sand$item))-20, ordered=TRUE)
```
# AAT



``` r
l0 = lmer(lrt~rep+item + (1|scode), data = aat)
l1 = lmer(lrt~rep*item + (1|scode), data = aat)
l2 = lmer(lrt~rep*item + nchar*lff + (1|scode), data = aat)
anova(l0,l1,l2)
```

```
## Data: aat
## Models:
## l0: lrt ~ rep + item + (1 | scode)
## l1: lrt ~ rep * item + (1 | scode)
## l2: lrt ~ rep * item + nchar * lff + (1 | scode)
##    npar    AIC    BIC  logLik -2*log(L)   Chisq Df Pr(>Chisq)    
## l0   14 93.520 182.55 -32.760    65.520                          
## l1   32 98.622 302.13 -17.311    34.622  30.897 18    0.02959 *  
## l2   35 -0.776 221.81  35.388   -70.776 105.398  3    < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
plot_model(l1, type='pred', terms=c('item', 'rep'), title='AAT item and repetition effect - model l1') + ylab('log RT')
```

<div class="figure" style="text-align: left">
<img src="plots/aat-1.png" alt="plot of chunk aat" width="800px" />
<p class="caption">plot of chunk aat</p>
</div>

``` r
plot_model(l2, type='pred', terms=c('item', 'rep'), title='AAT item and repetition effect - model l2') + ylab('log RT')
```

<div class="figure" style="text-align: left">
<img src="plots/aat-2.png" alt="plot of chunk aat" width="800px" />
<p class="caption">plot of chunk aat</p>
</div>

``` r
summary(l1)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: lrt ~ rep * item + (1 | scode)
##    Data: aat
## 
## REML criterion at convergence: 213.4
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.5070 -0.6564 -0.1693  0.4774  5.4803 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  scode    (Intercept) 0.04363  0.2089  
##  Residual             0.05805  0.2409  
## Number of obs: 4271, groups:  scode, 20
## 
## Fixed effects:
##                  Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)     6.871e+00  4.721e-02  1.960e+01 145.535  < 2e-16 ***
## reprep1        -2.542e-01  9.765e-03  4.222e+03 -26.038  < 2e-16 ***
## reprep2        -2.244e-01  9.778e-03  4.222e+03 -22.954  < 2e-16 ***
## item.L         -5.456e-02  2.345e-02  4.222e+03  -2.326 0.020059 *  
## item.Q         -1.142e-02  2.299e-02  4.222e+03  -0.497 0.619314    
## item.C         -8.000e-02  2.250e-02  4.222e+03  -3.556 0.000381 ***
## item^4          7.496e-02  2.234e-02  4.222e+03   3.355 0.000800 ***
## item^5         -2.132e-02  2.218e-02  4.222e+03  -0.961 0.336545    
## item^6         -1.307e-03  2.155e-02  4.222e+03  -0.061 0.951657    
## item^7          2.495e-02  2.090e-02  4.222e+03   1.194 0.232553    
## item^8         -2.512e-03  2.054e-02  4.222e+03  -0.122 0.902669    
## item^9          4.925e-03  2.018e-02  4.222e+03   0.244 0.807181    
## reprep1:item.L  4.816e-02  3.315e-02  4.222e+03   1.453 0.146341    
## reprep2:item.L  1.334e-02  3.317e-02  4.222e+03   0.402 0.687555    
## reprep1:item.Q  4.659e-02  3.247e-02  4.222e+03   1.435 0.151395    
## reprep2:item.Q  8.004e-02  3.251e-02  4.222e+03   2.462 0.013859 *  
## reprep1:item.C  3.622e-02  3.180e-02  4.222e+03   1.139 0.254682    
## reprep2:item.C  1.986e-02  3.185e-02  4.222e+03   0.624 0.532908    
## reprep1:item^4 -8.014e-02  3.153e-02  4.222e+03  -2.541 0.011079 *  
## reprep2:item^4 -9.699e-02  3.157e-02  4.222e+03  -3.073 0.002135 ** 
## reprep1:item^5  8.184e-03  3.132e-02  4.222e+03   0.261 0.793861    
## reprep2:item^5  4.647e-02  3.131e-02  4.222e+03   1.484 0.137796    
## reprep1:item^6 -4.290e-03  3.041e-02  4.222e+03  -0.141 0.887801    
## reprep2:item^6  3.220e-03  3.044e-02  4.222e+03   0.106 0.915773    
## reprep1:item^7  1.229e-02  2.949e-02  4.222e+03   0.417 0.676843    
## reprep2:item^7  6.800e-03  2.957e-02  4.222e+03   0.230 0.818097    
## reprep1:item^8  2.991e-02  2.899e-02  4.222e+03   1.032 0.302310    
## reprep2:item^8  4.397e-02  2.905e-02  4.222e+03   1.513 0.130243    
## reprep1:item^9 -1.959e-02  2.841e-02  4.222e+03  -0.690 0.490432    
## reprep2:item^9 -4.006e-02  2.850e-02  4.222e+03  -1.406 0.159846    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Despite the fact that model l2 seems better the plots show that l1 and l2 show similar prediction of the effects of items, repetition and their interaction.

The first item seems to behave rather differently  but it is the first real item after practivce...

Maximal effect of repetition emerge for intermediate number of items.

Second repetition shows numerically larger RT than first repetition with excpetion on one item, despite it is likely that this difference is not significant  and we can not stato 2nd repetition shows "a cost" with respect to the first, we can clearly conclude that there is no further advantage in the 2nd repetition with respect to the cost.

The fact that repetition advantage shows not only a linear effect as a function of item is due to the fact that repetition effect start to decrease in the last items (8-10).  There may be many explanations of this, teh more likely to me is that practice in the task allow to reduce RT in the reading stage even if items becomes a little more complex.

# SAND

Same analysis as above


``` r
l0 = lmer(lrt~rep+item + (1|scode), data = sand)
l1 = lmer(lrt~rep*item + (1|scode), data = sand)
l2 = lmer(lrt~rep*item + nchar*lff + (1|scode), data = sand)
anova(l0,l1,l2)
```

```
## Data: sand
## Models:
## l0: lrt ~ rep + item + (1 | scode)
## l1: lrt ~ rep * item + (1 | scode)
## l2: lrt ~ rep * item + nchar * lff + (1 | scode)
##    npar    AIC    BIC  logLik -2*log(L)  Chisq Df Pr(>Chisq)    
## l0   10 499.12 561.34 -239.56    479.12                         
## l1   20 430.70 555.15 -195.35    390.70 88.420 10  1.101e-14 ***
## l2   23 382.29 525.41 -168.15    336.29 54.408  3  9.183e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
plot_model(l1, type='pred', terms=c('item', 'rep'), title='SAND item and repetition effect - model l1') + ylab('log RT')
```

<div class="figure" style="text-align: left">
<img src="plots/sand-1.png" alt="plot of chunk sand" width="800px" />
<p class="caption">plot of chunk sand</p>
</div>

``` r
plot_model(l2, type='pred', terms=c('item', 'rep'), title='SAND item and repetition effect - model l2') + ylab('log RT')
```

<div class="figure" style="text-align: left">
<img src="plots/sand-2.png" alt="plot of chunk sand" width="800px" />
<p class="caption">plot of chunk sand</p>
</div>

``` r
summary(l1)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: lrt ~ rep * item + (1 | scode)
##    Data: sand
## 
## REML criterion at convergence: 502.4
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -8.9432 -0.6302 -0.1574  0.4246  5.1734 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  scode    (Intercept) 0.03202  0.1789  
##  Residual             0.06377  0.2525  
## Number of obs: 3724, groups:  scode, 20
## 
## Fixed effects:
##                  Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)     6.828e+00  4.079e-02  2.010e+01 167.367  < 2e-16 ***
## reprep1        -1.969e-01  1.124e-02  3.687e+03 -17.520  < 2e-16 ***
## reprep2        -1.964e-01  1.123e-02  3.687e+03 -17.495  < 2e-16 ***
## item.L         -4.566e-02  2.102e-02  3.687e+03  -2.172 0.029934 *  
## item.Q          1.081e-01  1.930e-02  3.687e+03   5.601 2.28e-08 ***
## item.C          7.241e-02  2.026e-02  3.687e+03   3.574 0.000357 ***
## item^4         -5.028e-02  1.971e-02  3.687e+03  -2.551 0.010784 *  
## item^5          3.738e-02  1.700e-02  3.687e+03   2.199 0.027962 *  
## reprep1:item.L  7.946e-02  2.972e-02  3.687e+03   2.674 0.007538 ** 
## reprep2:item.L  5.850e-02  2.962e-02  3.687e+03   1.975 0.048327 *  
## reprep1:item.Q -1.974e-01  2.731e-02  3.687e+03  -7.227 5.95e-13 ***
## reprep2:item.Q -1.506e-01  2.723e-02  3.687e+03  -5.532 3.38e-08 ***
## reprep1:item.C  3.253e-02  2.857e-02  3.687e+03   1.139 0.254978    
## reprep2:item.C -7.930e-02  2.854e-02  3.687e+03  -2.778 0.005492 ** 
## reprep1:item^4  4.988e-02  2.776e-02  3.687e+03   1.797 0.072429 .  
## reprep2:item^4  7.202e-02  2.776e-02  3.687e+03   2.594 0.009516 ** 
## reprep1:item^5 -5.755e-03  2.397e-02  3.687e+03  -0.240 0.810275    
## reprep2:item^5 -3.112e-02  2.401e-02  3.687e+03  -1.296 0.194946    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

The pattern for SAND  (2nd block)  shows a much more complex pattern.  The effect of item difficulty/sequence is clearly non linear since there is an increace in 1-2 and then an abrupt decrease of RT  of words in item 3 (maybe there are more function words in item3?) and then another linear trend ebtween 3 and 6.  As for repetition, its effect is stronger for items which show average longer rt during the reading stage.

REMIND for interpretation that dependent variable is RT for each word not the sum across word,  this means the model estimates average RT across words for each item  (and this average is clearly different across long and short sentences....)
