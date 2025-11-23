---
title: "Repetition Maze in aging - regular sentences"
output: html
author: F.V.
date: November 2025
---

Load libraries

``` r
library(lme4)
library(lmerTest)
library(ggplot2)
library(sjPlot)
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
We select only correct sentences from AAT SAND block as well as semantic and syntactc control items (SEC and SYC) in the third block.

We consider only correct trials and RT<3000ms.

We cosider log of RT as dependent variable since it is typically more normally distributed in RTs.

We consider log of count as an estimate of frequency of the form in the itwac database as the variable of word frequenct.

The three levels of the variable repetition (rep) and encoded as "read" (1st presentation), "rep1" (2nd presentation) and "rep2" (3rd presentation).



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

reg =  subset(d, cond %in% c('AAT','SAND','SEC','SYC'))
```
# Model to evaluate rep effects on wp and freq effects for reg sentences

The aim of this analysis is to look at which lexical and distributional (word position) variables show large effects during reading and which of these effects change in the repetition. 

We expect that frequency and word position would be the more relevant variables and could interact.

We also expect word length (nchar)  would have a relevant role and possibly not linear (more cost for both very short and long words).

Number of characters, word frequency and grammatical class are strongly correlated variables since close words tend to be short and frequent.  As well within open class words frequent words tend to be short (Zipf law). In our material the overall correlation between number of characters and the log of word frequency is indeed rather high (-0.76).

Moreover the way italian language works and the way items arte built make impossible to have a balanced design with respect to word position.

All this makes difficoult to model all the variables in  a parisimonous and sound statistical model. 

Our aim is to find clear differences between reading and repetion as a functiuon of either frequency or word position since this would be an index of the functioning of working memory advantages of a coherent lexical-semantic integration. We mainly expect that during reading there will be a reduction of RT as a function of word order (stronger constraints) and of frequency (especially at sentence beginning) and that these effect would largely wash out (reduce) during repetition.

We start by inspecting an overall model including all relevant variables (nchar, wp, lff, rep) in intaraction  with item and subject as a random function, being aware that the model can be overparametrized and check if simpler models show the same trends. To further check solidity of the effects we will run the same analysis separately for open words (using open word order in the sentence, instead of absolute poistion) and close words. 

This analyis should allow us which are stronger effects of repetition on regular sentences.

For these strong effect we will check if these emerge similarly in the three type of regular sentences (AAT, SEND and controls of block 3) and if the effects emerge strongly enough even considering only one or two repetitions.

We will as well look if these effects also emerge in models limited to single subject within the assumption of using these indexes as a subject-level estimation of the effect.

Before this analysis we reject word position after 9, for which the number of trial becomes very little.


``` r
table(reg$wp)
```

```
## 
##    0    1    2    3    4    5    6    7    8    9   10   11   12   13 
## 1527 1545 1542 1455 1165  974  786  511  418  355  355  235  235   59
```

``` r
reg = subset(reg, wp<9)
```



``` r
am = lmer(lrt ~ nchar + lff + wp + rep + (1|item) + (1|scode), data=reg)
mm = lmer(lrt ~ nchar * lff * wp * rep + (1|item) + (1|scode), data=reg)

anova(am,mm)
```

```
## Data: reg
## Models:
## am: lrt ~ nchar + lff + wp + rep + (1 | item) + (1 | scode)
## mm: lrt ~ nchar * lff * wp * rep + (1 | item) + (1 | scode)
##    npar    AIC    BIC   logLik -2*log(L)  Chisq Df Pr(>Chisq)    
## am    9 423.48 488.30 -202.739    405.48                         
## mm   27 241.60 436.07  -93.801    187.60 217.88 18  < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
summary(mm)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: lrt ~ nchar * lff * wp * rep + (1 | item) + (1 | scode)
##    Data: reg
## 
## REML criterion at convergence: 460.3
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -9.2779 -0.6488 -0.1418  0.4808  5.2581 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  item     (Intercept) 0.002939 0.05421 
##  scode    (Intercept) 0.035187 0.18758 
##  Residual             0.058603 0.24208 
## Number of obs: 9923, groups:  item, 36; scode, 20
## 
## Fixed effects:
##                        Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)           7.118e+00  9.724e-02  5.252e+02  73.193  < 2e-16 ***
## nchar                 2.541e-02  1.583e-02  9.865e+03   1.605  0.10848    
## lff                  -2.708e-03  6.264e-03  9.880e+03  -0.432  0.66548    
## wp                   -5.528e-02  2.016e-02  9.879e+03  -2.742  0.00611 ** 
## reprep1              -2.165e-01  1.206e-01  9.844e+03  -1.795  0.07267 .  
## reprep2              -2.343e-01  1.206e-01  9.844e+03  -1.943  0.05210 .  
## nchar:lff            -6.147e-03  1.402e-03  9.859e+03  -4.384 1.18e-05 ***
## nchar:wp             -1.406e-04  3.541e-03  9.880e+03  -0.040  0.96833    
## lff:wp                1.919e-04  1.406e-03  9.877e+03   0.136  0.89148    
## nchar:reprep1        -5.193e-02  2.176e-02  9.844e+03  -2.387  0.01701 *  
## nchar:reprep2        -3.730e-02  2.173e-02  9.844e+03  -1.716  0.08614 .  
## lff:reprep1          -1.081e-02  8.669e-03  9.844e+03  -1.246  0.21266    
## lff:reprep2          -7.746e-03  8.666e-03  9.844e+03  -0.894  0.37146    
## wp:reprep1            1.292e-02  2.791e-02  9.844e+03   0.463  0.64332    
## wp:reprep2           -1.215e-02  2.792e-02  9.844e+03  -0.435  0.66349    
## nchar:lff:wp          6.338e-04  3.230e-04  9.880e+03   1.962  0.04977 *  
## nchar:lff:reprep1     6.028e-03  1.926e-03  9.844e+03   3.130  0.00175 ** 
## nchar:lff:reprep2     5.215e-03  1.924e-03  9.844e+03   2.710  0.00673 ** 
## nchar:wp:reprep1      5.248e-03  4.902e-03  9.844e+03   1.070  0.28445    
## nchar:wp:reprep2      4.445e-03  4.901e-03  9.844e+03   0.907  0.36450    
## lff:wp:reprep1        8.628e-04  1.951e-03  9.844e+03   0.442  0.65839    
## lff:wp:reprep2        2.070e-03  1.951e-03  9.844e+03   1.061  0.28890    
## nchar:lff:wp:reprep1 -6.029e-04  4.476e-04  9.844e+03  -1.347  0.17810    
## nchar:lff:wp:reprep2 -5.781e-04  4.476e-04  9.844e+03  -1.292  0.19655    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

The large difference of variance explained shows that the variables strongly interacts, as also shown by the table of fixed efefct of the multiplicative model (mm).

The strongest effects emerges for nchar and  its interactions. Since the effect of nchar may be rather complex and non linear, confounded with more interpretable variable (e.g. open-close class, frequency) we will repeat the same analysis using nchar as a factor variable (estimating effects separately for each length value)  and plot the effect in interaction with repetition to better understand the role of this variable.



``` r
reg2 = reg 
reg2$nchar = factor(reg2$nchar)
mm = lmer(lrt ~ nchar * lff * wp * rep + (1|item) + (1|scode), data=reg2)
summary(mm)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: lrt ~ nchar * lff * wp * rep + (1 | item) + (1 | scode)
##    Data: reg2
## 
## REML criterion at convergence: 614.3
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -9.2718 -0.6403 -0.1307  0.4797  5.3765 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  item     (Intercept) 0.002309 0.04805 
##  scode    (Intercept) 0.035273 0.18781 
##  Residual             0.056529 0.23776 
## Number of obs: 9923, groups:  item, 36; scode, 20
## 
## Fixed effects:
##                          Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)             9.436e+00  4.086e-01  9.412e+03  23.091  < 2e-16 ***
## nchar2                 -1.684e+00  4.286e-01  9.763e+03  -3.929 8.58e-05 ***
## nchar3                 -1.821e+00  4.384e-01  9.713e+03  -4.154 3.29e-05 ***
## nchar4                 -3.329e+00  5.137e-01  9.690e+03  -6.481 9.58e-11 ***
## nchar5                 -2.733e+00  4.212e-01  9.708e+03  -6.489 9.06e-11 ***
## nchar6                 -1.998e+00  4.412e-01  9.708e+03  -4.529 6.00e-06 ***
## nchar7                 -2.261e+00  4.339e-01  9.753e+03  -5.211 1.92e-07 ***
## nchar8                 -2.779e+00  4.737e-01  9.713e+03  -5.866 4.61e-09 ***
## nchar9                 -2.678e+00  1.720e+00  8.992e+03  -1.557 0.119506    
## nchar10                -2.543e+00  6.626e-01  9.604e+03  -3.837 0.000125 ***
## nchar11                -2.472e+00  7.806e-01  9.769e+03  -3.166 0.001548 ** 
## nchar12                -2.003e+01  7.592e+00  9.772e+03  -2.639 0.008326 ** 
## nchar15                -1.702e+00  6.921e-01  9.765e+03  -2.460 0.013917 *  
## lff                    -1.593e-01  2.811e-02  9.760e+03  -5.667 1.50e-08 ***
## wp                      8.039e-01  5.775e-01  9.770e+03   1.392 0.163966    
## reprep1                -1.643e+00  5.513e-01  9.737e+03  -2.980 0.002887 ** 
## reprep2                -9.428e-01  5.448e-01  9.737e+03  -1.731 0.083558 .  
## nchar2:lff              1.066e-01  2.954e-02  9.761e+03   3.608 0.000310 ***
## nchar3:lff              1.047e-01  3.017e-02  9.721e+03   3.470 0.000522 ***
## nchar4:lff              2.099e-01  3.977e-02  9.644e+03   5.278 1.34e-07 ***
## nchar5:lff              1.754e-01  2.981e-02  9.678e+03   5.882 4.18e-09 ***
## nchar6:lff              1.055e-01  3.238e-02  9.649e+03   3.258 0.001125 ** 
## nchar7:lff              1.232e-01  3.161e-02  9.733e+03   3.898 9.77e-05 ***
## nchar8:lff              1.791e-01  3.808e-02  9.671e+03   4.704 2.59e-06 ***
## nchar9:lff              1.767e-01  2.013e-01  8.905e+03   0.878 0.380020    
## nchar10:lff             1.513e-01  6.096e-02  9.552e+03   2.482 0.013070 *  
## nchar12:lff             1.942e+00  7.724e-01  9.772e+03   2.514 0.011947 *  
## nchar2:wp              -8.606e-01  5.814e-01  9.771e+03  -1.480 0.138887    
## nchar3:wp              -9.069e-01  5.793e-01  9.771e+03  -1.565 0.117504    
## nchar4:wp              -6.774e-01  5.804e-01  9.770e+03  -1.167 0.243181    
## nchar5:wp              -6.903e-01  5.789e-01  9.769e+03  -1.192 0.233124    
## nchar6:wp              -8.954e-01  5.786e-01  9.770e+03  -1.548 0.121753    
## nchar7:wp              -9.215e-01  5.794e-01  9.771e+03  -1.590 0.111777    
## nchar8:wp              -7.243e-01  5.799e-01  9.770e+03  -1.249 0.211732    
## nchar9:wp              -4.036e-01  2.986e-01  9.770e+03  -1.351 0.176569    
## nchar10:wp             -6.146e-01  6.012e-01  9.769e+03  -1.022 0.306706    
## nchar12:wp             -4.435e-02  2.194e-01  9.756e+03  -0.202 0.839778    
## lff:wp                 -5.001e-02  3.504e-02  9.770e+03  -1.427 0.153587    
## nchar2:reprep1          1.409e+00  5.821e-01  9.737e+03   2.421 0.015495 *  
## nchar3:reprep1          1.503e+00  5.897e-01  9.737e+03   2.548 0.010847 *  
## nchar4:reprep1          1.963e+00  6.988e-01  9.737e+03   2.808 0.004990 ** 
## nchar5:reprep1          1.210e+00  5.673e-01  9.737e+03   2.132 0.033001 *  
## nchar6:reprep1          8.882e-01  5.954e-01  9.737e+03   1.492 0.135768    
## nchar7:reprep1          1.074e+00  5.914e-01  9.737e+03   1.817 0.069311 .  
## nchar8:reprep1          1.257e+00  6.433e-01  9.737e+03   1.954 0.050761 .  
## nchar9:reprep1         -3.173e-01  2.372e+00  9.737e+03  -0.134 0.893588    
## nchar10:reprep1         6.440e-01  9.035e-01  9.737e+03   0.713 0.475952    
## nchar11:reprep1         1.980e+00  1.066e+00  9.736e+03   1.857 0.063280 .  
## nchar12:reprep1         9.702e+00  1.047e+01  9.736e+03   0.927 0.353922    
## nchar15:reprep1         1.623e+00  9.462e-01  9.736e+03   1.715 0.086305 .  
## nchar2:reprep2          5.721e-01  5.761e-01  9.737e+03   0.993 0.320698    
## nchar3:reprep2          2.480e-01  5.840e-01  9.737e+03   0.425 0.671109    
## nchar4:reprep2          1.439e+00  6.938e-01  9.737e+03   2.074 0.038148 *  
## nchar5:reprep2          7.340e-01  5.610e-01  9.737e+03   1.308 0.190779    
## nchar6:reprep2          6.195e-01  5.893e-01  9.737e+03   1.051 0.293241    
## nchar7:reprep2          4.717e-01  5.853e-01  9.737e+03   0.806 0.420311    
## nchar8:reprep2          7.490e-01  6.386e-01  9.737e+03   1.173 0.240884    
## nchar9:reprep2         -7.615e-01  2.326e+00  9.737e+03  -0.327 0.743349    
## nchar10:reprep2         1.800e-01  9.006e-01  9.737e+03   0.200 0.841558    
## nchar11:reprep2         1.960e+00  1.065e+00  9.737e+03   1.840 0.065814 .  
## nchar12:reprep2         1.685e+01  1.046e+01  9.737e+03   1.611 0.107160    
## nchar15:reprep2         1.638e+00  9.455e-01  9.737e+03   1.733 0.083176 .  
## lff:reprep1             7.949e-02  3.811e-02  9.737e+03   2.086 0.036994 *  
## lff:reprep2             3.242e-02  3.773e-02  9.737e+03   0.859 0.390295    
## wp:reprep1             -8.894e-01  7.906e-01  9.736e+03  -1.125 0.260645    
## wp:reprep2             -1.179e+00  7.905e-01  9.737e+03  -1.491 0.135890    
## nchar2:lff:wp           5.224e-02  3.529e-02  9.771e+03   1.481 0.138757    
## nchar3:lff:wp           5.681e-02  3.518e-02  9.771e+03   1.615 0.106394    
## nchar4:lff:wp           4.054e-02  3.546e-02  9.769e+03   1.143 0.252894    
## nchar5:lff:wp           3.716e-02  3.523e-02  9.769e+03   1.055 0.291553    
## nchar6:lff:wp           5.749e-02  3.520e-02  9.771e+03   1.633 0.102465    
## nchar7:lff:wp           5.958e-02  3.531e-02  9.770e+03   1.687 0.091593 .  
## nchar8:lff:wp           4.043e-02  3.547e-02  9.771e+03   1.140 0.254435    
## nchar10:lff:wp          3.075e-02  3.898e-02  9.764e+03   0.789 0.430257    
## nchar2:lff:reprep1     -8.342e-02  4.010e-02  9.737e+03  -2.080 0.037526 *  
## nchar3:lff:reprep1     -8.572e-02  4.062e-02  9.737e+03  -2.110 0.034884 *  
## nchar4:lff:reprep1     -1.240e-01  5.414e-02  9.737e+03  -2.291 0.021997 *  
## nchar5:lff:reprep1     -6.534e-02  4.006e-02  9.737e+03  -1.631 0.102956    
## nchar6:lff:reprep1     -3.040e-02  4.364e-02  9.737e+03  -0.697 0.486045    
## nchar7:lff:reprep1     -5.291e-02  4.312e-02  9.737e+03  -1.227 0.219812    
## nchar8:lff:reprep1     -6.772e-02  5.176e-02  9.737e+03  -1.308 0.190766    
## nchar9:lff:reprep1      1.160e-01  2.778e-01  9.737e+03   0.417 0.676364    
## nchar10:lff:reprep1    -1.177e-03  8.318e-02  9.737e+03  -0.014 0.988708    
## nchar12:lff:reprep1    -9.680e-01  1.064e+00  9.736e+03  -0.909 0.363143    
## nchar2:lff:reprep2     -2.521e-02  3.976e-02  9.737e+03  -0.634 0.526134    
## nchar3:lff:reprep2      2.569e-03  4.030e-02  9.737e+03   0.064 0.949186    
## nchar4:lff:reprep2     -9.552e-02  5.389e-02  9.737e+03  -1.773 0.076338 .  
## nchar5:lff:reprep2     -3.347e-02  3.972e-02  9.737e+03  -0.843 0.399483    
## nchar6:lff:reprep2     -1.724e-02  4.332e-02  9.737e+03  -0.398 0.690708    
## nchar7:lff:reprep2     -6.575e-03  4.277e-02  9.737e+03  -0.154 0.877832    
## nchar8:lff:reprep2     -3.511e-02  5.160e-02  9.737e+03  -0.680 0.496244    
## nchar9:lff:reprep2      1.348e-01  2.722e-01  9.737e+03   0.495 0.620588    
## nchar10:lff:reprep2     2.277e-02  8.321e-02  9.737e+03   0.274 0.784381    
## nchar12:lff:reprep2    -1.690e+00  1.064e+00  9.737e+03  -1.589 0.112086    
## nchar2:wp:reprep1       6.145e-01  7.976e-01  9.736e+03   0.770 0.441074    
## nchar3:wp:reprep1       8.464e-01  7.946e-01  9.736e+03   1.065 0.286807    
## nchar4:wp:reprep1       8.333e-01  7.949e-01  9.736e+03   1.048 0.294504    
## nchar5:wp:reprep1       9.049e-01  7.924e-01  9.736e+03   1.142 0.253530    
## nchar6:wp:reprep1       9.851e-01  7.923e-01  9.736e+03   1.243 0.213800    
## nchar7:wp:reprep1       9.831e-01  7.939e-01  9.736e+03   1.238 0.215628    
## nchar8:wp:reprep1       8.816e-01  7.941e-01  9.736e+03   1.110 0.266972    
## nchar9:wp:reprep1       4.946e-01  4.088e-01  9.736e+03   1.210 0.226403    
## nchar10:wp:reprep1      1.063e+00  8.240e-01  9.736e+03   1.290 0.196908    
## nchar12:wp:reprep1      3.210e-01  2.986e-01  9.736e+03   1.075 0.282314    
## nchar2:wp:reprep2       8.519e-01  7.975e-01  9.737e+03   1.068 0.285446    
## nchar3:wp:reprep2       1.152e+00  7.946e-01  9.737e+03   1.450 0.146962    
## nchar4:wp:reprep2       1.077e+00  7.948e-01  9.737e+03   1.355 0.175369    
## nchar5:wp:reprep2       1.137e+00  7.924e-01  9.737e+03   1.435 0.151196    
## nchar6:wp:reprep2       1.199e+00  7.923e-01  9.737e+03   1.513 0.130291    
## nchar7:wp:reprep2       1.234e+00  7.939e-01  9.737e+03   1.555 0.120047    
## nchar8:wp:reprep2       1.099e+00  7.941e-01  9.737e+03   1.384 0.166316    
## nchar9:wp:reprep2       5.896e-01  4.087e-01  9.737e+03   1.443 0.149184    
## nchar10:wp:reprep2      1.179e+00  8.240e-01  9.737e+03   1.431 0.152563    
## nchar12:wp:reprep2      2.490e-01  2.984e-01  9.737e+03   0.835 0.403901    
## lff:wp:reprep1          5.542e-02  4.798e-02  9.736e+03   1.155 0.248016    
## lff:wp:reprep2          7.402e-02  4.797e-02  9.737e+03   1.543 0.122845    
## nchar2:lff:wp:reprep1  -3.784e-02  4.841e-02  9.736e+03  -0.782 0.434422    
## nchar3:lff:wp:reprep1  -5.185e-02  4.827e-02  9.736e+03  -1.074 0.282702    
## nchar4:lff:wp:reprep1  -5.051e-02  4.859e-02  9.736e+03  -1.039 0.298611    
## nchar5:lff:wp:reprep1  -5.488e-02  4.825e-02  9.736e+03  -1.137 0.255366    
## nchar6:lff:wp:reprep1  -6.386e-02  4.824e-02  9.736e+03  -1.324 0.185577    
## nchar7:lff:wp:reprep1  -6.134e-02  4.840e-02  9.736e+03  -1.267 0.205013    
## nchar8:lff:wp:reprep1  -5.300e-02  4.859e-02  9.736e+03  -1.091 0.275395    
## nchar10:lff:wp:reprep1 -7.145e-02  5.346e-02  9.736e+03  -1.337 0.181370    
## nchar2:lff:wp:reprep2  -5.373e-02  4.841e-02  9.737e+03  -1.110 0.267149    
## nchar3:lff:wp:reprep2  -7.298e-02  4.826e-02  9.737e+03  -1.512 0.130515    
## nchar4:lff:wp:reprep2  -6.548e-02  4.859e-02  9.737e+03  -1.348 0.177780    
## nchar5:lff:wp:reprep2  -6.981e-02  4.825e-02  9.737e+03  -1.447 0.147949    
## nchar6:lff:wp:reprep2  -7.667e-02  4.824e-02  9.737e+03  -1.589 0.111984    
## nchar7:lff:wp:reprep2  -7.804e-02  4.840e-02  9.737e+03  -1.613 0.106863    
## nchar8:lff:wp:reprep2  -6.574e-02  4.859e-02  9.737e+03  -1.353 0.176133    
## nchar10:lff:wp:reprep2 -7.245e-02  5.347e-02  9.737e+03  -1.355 0.175453    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```
## fit warnings:
## fixed-effect model matrix is rank deficient so dropping 24 columns / coefficients
```

``` r
plot_model(mm, type='pred', terms=c('nchar','rep'))
```

<div class="figure" style="text-align: left">
<img src="plots/mod_nchar_rep-1.png" alt="plot of chunk mod_nchar_rep" width="800px" />
<p class="caption">plot of chunk mod_nchar_rep</p>
</div>

The plot of the predicted effects suggest that nchar works similarly and smootly across values between 2 and 8. When words are very long (>9 letters) or composed by a single character the estimates of the model become very noisy.  We atribute this to specific features of the maze task and not a to psychological relevant factors and thus we consider the following model on restricted data. It shows effects of frequency,  word position, repetition2 and a complex interaction between frequency, number of charaters and repetition1.


``` r
reg3 = subset(reg, nchar>1 & nchar <9)

mod2 = lmer(lrt ~ nchar + lff*wp*rep + (1|item) + (1|scode), data=reg3)

summary(mod2)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: lrt ~ nchar + lff * wp * rep + (1 | item) + (1 | scode)
##    Data: reg3
## 
## REML criterion at convergence: 345.3
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -9.0213 -0.6522 -0.1415  0.4856  5.1175 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  item     (Intercept) 0.003039 0.05513 
##  scode    (Intercept) 0.034992 0.18706 
##  Residual             0.058724 0.24233 
## Number of obs: 8887, groups:  item, 36; scode, 20
## 
## Fixed effects:
##                  Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)     7.103e+00  6.084e-02  8.396e+01 116.751  < 2e-16 ***
## nchar          -2.235e-02  2.098e-03  8.425e+03 -10.652  < 2e-16 ***
## lff            -9.972e-03  3.009e-03  8.715e+03  -3.315 0.000922 ***
## wp             -9.954e-04  9.339e-03  8.854e+03  -0.107 0.915119    
## reprep1        -3.160e-01  4.983e-02  8.820e+03  -6.341 2.39e-10 ***
## reprep2        -2.209e-01  4.994e-02  8.820e+03  -4.422 9.87e-06 ***
## lff:wp         -1.577e-03  7.125e-04  8.854e+03  -2.213 0.026901 *  
## lff:reprep1     3.309e-03  3.794e-03  8.820e+03   0.872 0.383106    
## lff:reprep2    -4.599e-04  3.802e-03  8.820e+03  -0.121 0.903739    
## wp:reprep1      2.510e-02  1.287e-02  8.820e+03   1.951 0.051146 .  
## wp:reprep2     -5.351e-03  1.288e-02  8.820e+03  -0.416 0.677708    
## lff:wp:reprep1 -6.285e-04  9.858e-04  8.820e+03  -0.638 0.523783    
## lff:wp:reprep2  7.690e-04  9.861e-04  8.820e+03   0.780 0.435491    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Since we may expect different effects of these variables for open and close word we conduct separate analyses on these subset,  using word order instead of word position


*Close words*


``` r
regclose = subset(reg3, open==0)

modC = lmer(lrt ~ nchar * lff*cwo*rep + (1|item) + (1|scode), data=regclose)

summary(modC)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: lrt ~ nchar * lff * cwo * rep + (1 | item) + (1 | scode)
##    Data: regclose
## 
## REML criterion at convergence: 309.9
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.1842 -0.6552 -0.1245  0.5141  5.5012 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  item     (Intercept) 0.003484 0.05902 
##  scode    (Intercept) 0.037622 0.19396 
##  Residual             0.057957 0.24074 
## Number of obs: 4681, groups:  item, 36; scode, 20
## 
## Fixed effects:
##                         Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)            7.604e+00  3.176e-01  4.407e+03  23.942  < 2e-16 ***
## nchar                  2.235e-01  8.951e-02  4.638e+03   2.497 0.012546 *  
## lff                   -2.286e-02  2.199e-02  4.630e+03  -1.040 0.298575    
## cwo                   -4.283e-02  1.274e-01  4.636e+03  -0.336 0.736781    
## reprep1                3.134e-01  4.279e-01  4.603e+03   0.732 0.464033    
## reprep2               -5.409e-01  4.288e-01  4.605e+03  -1.261 0.207237    
## nchar:lff             -2.419e-02  6.792e-03  4.638e+03  -3.561 0.000373 ***
## nchar:cwo             -9.103e-02  3.598e-02  4.634e+03  -2.530 0.011434 *  
## lff:cwo               -4.207e-03  8.497e-03  4.637e+03  -0.495 0.620585    
## nchar:reprep1         -2.137e-01  1.226e-01  4.602e+03  -1.742 0.081504 .  
## nchar:reprep2          2.612e-02  1.228e-01  4.603e+03   0.213 0.831565    
## lff:reprep1           -4.844e-02  3.004e-02  4.603e+03  -1.612 0.106961    
## lff:reprep2            1.438e-02  3.010e-02  4.604e+03   0.478 0.632839    
## cwo:reprep1           -2.690e-01  1.735e-01  4.603e+03  -1.550 0.121103    
## cwo:reprep2           -2.286e-01  1.737e-01  4.604e+03  -1.316 0.188262    
## nchar:lff:cwo          8.211e-03  2.644e-03  4.633e+03   3.106 0.001910 ** 
## nchar:lff:reprep1      1.872e-02  9.355e-03  4.602e+03   2.001 0.045417 *  
## nchar:lff:reprep2      1.741e-03  9.371e-03  4.603e+03   0.186 0.852594    
## nchar:cwo:reprep1      5.941e-02  4.881e-02  4.602e+03   1.217 0.223562    
## nchar:cwo:reprep2      3.907e-02  4.883e-02  4.603e+03   0.800 0.423674    
## lff:cwo:reprep1        1.918e-02  1.159e-02  4.603e+03   1.655 0.098078 .  
## lff:cwo:reprep2        1.438e-02  1.160e-02  4.603e+03   1.240 0.215180    
## nchar:lff:cwo:reprep1 -4.517e-03  3.591e-03  4.602e+03  -1.258 0.208474    
## nchar:lff:cwo:reprep2 -2.807e-03  3.592e-03  4.602e+03  -0.781 0.434558    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

The more relevant effect including repetition is the interaction including nchar, lff and reprep1. The plot below illustrates the effect, showing a reduction of the effect of word length (shorter words give longer RT) and of the effect of frequency (slower RT for low frequency words) between reading and repetition.  We think this effect is mainly linked to the task (short words are difficoult in the maze task but maybe not in normal reading) and repetition shows just a quantitative reduction of the effect ratehr than a clear qualitative processing change.


``` r
plot_model(modC, type='pred', terms = c('nchar','lff', 'rep'))
```

<div class="figure" style="text-align: left">
<img src="plots/mod_close_plot-1.png" alt="plot of chunk mod_close_plot" width="800px" />
<p class="caption">plot of chunk mod_close_plot</p>
</div>

*Open words*

``` r
regopen = subset(reg3, open==1)

modO = lmer(lrt ~ nchar * lff*owo*rep + (1|item) + (1|scode), data=regopen)

summary(modO)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: lrt ~ nchar * lff * owo * rep + (1 | item) + (1 | scode)
##    Data: regopen
## 
## REML criterion at convergence: -158.2
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -9.7094 -0.6286 -0.1185  0.4754  5.2588 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  item     (Intercept) 0.002805 0.05296 
##  scode    (Intercept) 0.034046 0.18451 
##  Residual             0.051785 0.22756 
## Number of obs: 4206, groups:  item, 36; scode, 20
## 
## Fixed effects:
##                         Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)            7.021e+00  4.182e-01  3.704e+03  16.790  < 2e-16 ***
## nchar                 -1.991e-03  7.128e-02  3.830e+03  -0.028  0.97772    
## lff                   -2.788e-02  3.832e-02  3.856e+03  -0.727  0.46701    
## owo                   -1.276e-01  1.613e-01  3.837e+03  -0.791  0.42906    
## reprep1               -1.386e+00  5.491e-01  4.128e+03  -2.524  0.01165 *  
## reprep2               -6.217e-01  5.497e-01  4.128e+03  -1.131  0.25812    
## nchar:lff              1.390e-03  6.671e-03  3.939e+03   0.208  0.83501    
## nchar:owo              2.055e-02  2.745e-02  3.831e+03   0.749  0.45418    
## lff:owo                1.726e-02  1.555e-02  3.921e+03   1.110  0.26718    
## nchar:reprep1          1.427e-01  9.467e-02  4.128e+03   1.508  0.13170    
## nchar:reprep2          5.070e-02  9.471e-02  4.128e+03   0.535  0.59246    
## lff:reprep1            1.160e-01  5.080e-02  4.128e+03   2.283  0.02248 *  
## lff:reprep2            3.350e-02  5.083e-02  4.128e+03   0.659  0.50996    
## owo:reprep1            5.581e-01  2.138e-01  4.128e+03   2.611  0.00907 ** 
## owo:reprep2            3.473e-01  2.141e-01  4.128e+03   1.622  0.10493    
## nchar:lff:owo         -2.937e-03  2.700e-03  3.914e+03  -1.088  0.27685    
## nchar:lff:reprep1     -1.513e-02  8.896e-03  4.128e+03  -1.701  0.08907 .  
## nchar:lff:reprep2     -4.096e-03  8.895e-03  4.128e+03  -0.461  0.64516    
## nchar:owo:reprep1     -8.433e-02  3.636e-02  4.128e+03  -2.319  0.02042 *  
## nchar:owo:reprep2     -5.733e-02  3.642e-02  4.128e+03  -1.574  0.11550    
## lff:owo:reprep1       -5.609e-02  2.063e-02  4.128e+03  -2.719  0.00657 ** 
## lff:owo:reprep2       -3.463e-02  2.066e-02  4.128e+03  -1.676  0.09375 .  
## nchar:lff:owo:reprep1  8.950e-03  3.582e-03  4.128e+03   2.499  0.01249 *  
## nchar:lff:owo:reprep2  5.860e-03  3.586e-03  4.128e+03   1.634  0.10230    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```
Open words clearly show effects mainly in repetition1 for frquency and word order plus some limited interaction with nchar. The following plot shows graphcally the more relevant predictions of the model.



``` r
plot_model(modO, type='pred', terms = c('owo','lff', 'rep'))
```

<div class="figure" style="text-align: left">
<img src="plots/mod_open_plot-1.png" alt="plot of chunk mod_open_plot" width="800px" />
<p class="caption">plot of chunk mod_open_plot</p>
</div>

The following plots shows how this efefect is further qualifiyed by word length,  suggesting a different patter for short (nchar=2,3) and long open class words. The table of the material shows that short open class words are not many  and thus we exclude them.

In order to check if effects emerge at single subject level and for subsets of the data we further restrict the analysis to read and repetition1 where the difference in processing appears maximal, removing data of repetition2 and symplifying the model excluding the nchar variable.


``` r
plot_model(modO, type='pred', terms = c('owo','lff', 'rep','nchar'))
```

<div class="figure" style="text-align: left">
<img src="plots/mod_openrep1_plot-1.png" alt="plot of chunk mod_openrep1_plot" width="800px" />
<p class="caption">plot of chunk mod_openrep1_plot</p>
</div>

``` r
table(regopen$nchar)
```

```
## 
##    2    3    4    5    6    7    8 
##   60   86  502 1284  808  899  567
```

``` r
regopen2 = subset(regopen, rep !="rep2" & nchar>3)

modO = lmer(lrt ~ lff*owo*rep + (1|item) + (1|scode), data=regopen2)
summary(modO)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: lrt ~ lff * owo * rep + (1 | item) + (1 | scode)
##    Data: regopen2
## 
## REML criterion at convergence: -205.6
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.4381 -0.6442 -0.1167  0.4913  5.0287 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  item     (Intercept) 0.003656 0.06046 
##  scode    (Intercept) 0.034360 0.18536 
##  Residual             0.050192 0.22403 
## Number of obs: 2705, groups:  item, 36; scode, 20
## 
## Fixed effects:
##                   Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)      7.109e+00  1.001e-01  4.972e+02  71.035  < 2e-16 ***
## lff             -3.001e-02  8.591e-03  1.820e+03  -3.493 0.000488 ***
## owo             -4.310e-02  3.482e-02  2.352e+03  -1.238 0.215928    
## reprep1         -5.718e-01  1.130e-01  2.641e+03  -5.059  4.5e-07 ***
## lff:owo          3.903e-03  3.330e-03  2.440e+03   1.172 0.241336    
## lff:reprep1      3.040e-02  1.073e-02  2.641e+03   2.833 0.004642 ** 
## owo:reprep1      8.612e-02  4.521e-02  2.641e+03   1.905 0.056910 .  
## lff:owo:reprep1 -6.467e-03  4.350e-03  2.641e+03  -1.486 0.137280    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) lff    owo    reprp1 lff:ow lff:r1 ow:rp1
## lff         -0.894                                          
## owo         -0.814  0.893                                   
## reprep1     -0.565  0.616  0.584                            
## lff:owo      0.797 -0.895 -0.988 -0.577                     
## lff:reprep1  0.557 -0.625 -0.580 -0.987  0.588              
## owo:reprep1  0.507 -0.558 -0.651 -0.898  0.648  0.892       
## lff:w:rprp1 -0.498  0.562  0.644  0.881 -0.657 -0.897 -0.988
```

``` r
plot_model(modO, type='pred', terms = c('lff', 'rep'))
```

<div class="figure" style="text-align: left">
<img src="plots/mod_openrep1_plot-2.png" alt="plot of chunk mod_openrep1_plot" width="800px" />
<p class="caption">plot of chunk mod_openrep1_plot</p>
</div>

The main effect on this specific subset of data appear strong enough to show large advantage on these specific words for repetition (t = 5.059) that may indeed show to be valid also at single subject level or for small groups of patients and a relavant interaction with word frquency which shows this advantage is particularly relevant for low frequency word. Both these measures as well as their interaction could be considered an index of an efficient memory of lexical items within sentences.  No clear interaction on these specific data emerge with the order of the word in the sentence.


## Same model restricted to AAT, SAND and Controls


``` r
modAAT = lmer(lrt ~ lff*owo*rep + (1|item) + (1|scode), data=subset(regopen2, cond=='AAT'))
summary(modAAT)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: lrt ~ lff * owo * rep + (1 | item) + (1 | scode)
##    Data: subset(regopen2, cond == "AAT")
## 
## REML criterion at convergence: -21.7
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.0024 -0.6738 -0.1408  0.4654  4.8652 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  scode    (Intercept) 0.041896 0.20469 
##  item     (Intercept) 0.001116 0.03341 
##  Residual             0.049183 0.22177 
## Number of obs: 874, groups:  scode, 20; item, 10
## 
## Fixed effects:
##                   Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)       7.167899   0.241554  76.600323  29.674  < 2e-16 ***
## lff              -0.026671   0.021368  77.936904  -1.248  0.21571    
## owo              -0.067350   0.106443  88.462431  -0.633  0.52854    
## reprep1          -0.799932   0.272148 838.084943  -2.939  0.00338 ** 
## lff:owo           0.004026   0.009658 100.396366   0.417  0.67770    
## lff:reprep1       0.046865   0.024730 838.072311   1.895  0.05843 .  
## owo:reprep1       0.190816   0.123844 838.101739   1.541  0.12375    
## lff:owo:reprep1  -0.013962   0.011378 838.096793  -1.227  0.22011    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) lff    owo    reprp1 lff:ow lff:r1 ow:rp1
## lff         -0.975                                          
## owo         -0.933  0.949                                   
## reprep1     -0.567  0.577  0.548                            
## lff:owo      0.924 -0.951 -0.994 -0.549                     
## lff:reprep1  0.562 -0.582 -0.547 -0.992  0.556              
## owo:reprep1  0.532 -0.544 -0.588 -0.934  0.591  0.931       
## lff:w:rprp1 -0.526  0.547  0.584  0.923 -0.597 -0.935 -0.992
```

``` r
plot_model(modAAT, type='pred', terms = c('lff', 'rep'))
```

<div class="figure" style="text-align: left">
<img src="plots/mod_open_restricted-1.png" alt="plot of chunk mod_open_restricted" width="800px" />
<p class="caption">plot of chunk mod_open_restricted</p>
</div>

``` r
modSAND = lmer(lrt ~ lff*owo*rep + (1|item) + (1|scode), data=subset(regopen2, cond=='SAND'))
summary(modSAND)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: lrt ~ lff * owo * rep + (1 | item) + (1 | scode)
##    Data: subset(regopen2, cond == "SAND")
## 
## REML criterion at convergence: 28.3
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.1182 -0.6416 -0.1219  0.4336  4.4954 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  scode    (Intercept) 0.039558 0.19889 
##  item     (Intercept) 0.005138 0.07168 
##  Residual             0.051584 0.22712 
## Number of obs: 794, groups:  scode, 20; item, 6
## 
## Fixed effects:
##                  Estimate Std. Error        df t value Pr(>|t|)    
## (Intercept)       7.28774    0.29990 735.58475  24.300  < 2e-16 ***
## lff              -0.03466    0.02749 757.15436  -1.261  0.20787    
## owo               0.08862    0.09988 763.46749   0.887  0.37519    
## reprep1           0.30569    0.38475 761.78978   0.795  0.42714    
## lff:owo          -0.01249    0.00975 765.16049  -1.281  0.20046    
## lff:reprep1      -0.06295    0.03611 761.79326  -1.743  0.08171 .  
## owo:reprep1      -0.30582    0.13339 761.79315  -2.293  0.02214 *  
## lff:owo:reprep1   0.03560    0.01312 761.79497   2.713  0.00683 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) lff    owo    reprp1 lff:ow lff:r1 ow:rp1
## lff         -0.978                                          
## owo         -0.912  0.940                                   
## reprep1     -0.635  0.645  0.611                            
## lff:owo      0.883 -0.923 -0.993 -0.597                     
## lff:reprep1  0.630 -0.649 -0.619 -0.993  0.614              
## owo:reprep1  0.587 -0.609 -0.660 -0.926  0.660  0.939       
## lff:w:rprp1 -0.569  0.599  0.655  0.898 -0.665 -0.924 -0.993
```

``` r
plot_model(modSAND, type='pred', terms = c('lff', 'rep'))
```

<div class="figure" style="text-align: left">
<img src="plots/mod_open_restricted-2.png" alt="plot of chunk mod_open_restricted" width="800px" />
<p class="caption">plot of chunk mod_open_restricted</p>
</div>

``` r
plot_model(modSAND, type='pred', terms = c('lff', 'rep', 'owo'))
```

<div class="figure" style="text-align: left">
<img src="plots/mod_open_restricted-3.png" alt="plot of chunk mod_open_restricted" width="800px" />
<p class="caption">plot of chunk mod_open_restricted</p>
</div>

``` r
ctr = subset(regopen2, cond %in% c('SEC','SYC') & listType=='A')

modCTR = lmer(lrt ~ lff*owo*rep + (1|item) + (1|scode), data=ctr)
summary(modCTR)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: lrt ~ lff * owo * rep + (1 | item) + (1 | scode)
##    Data: ctr
## 
## REML criterion at convergence: -273.9
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.5321 -0.6585 -0.1113  0.5318  4.7841 
## 
## Random effects:
##  Groups   Name        Variance  Std.Dev.
##  scode    (Intercept) 0.0279377 0.16715 
##  item     (Intercept) 0.0008093 0.02845 
##  Residual             0.0339497 0.18425 
## Number of obs: 700, groups:  scode, 11; item, 10
## 
## Fixed effects:
##                   Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)       7.064655   0.121904 227.756411  57.952  < 2e-16 ***
## lff              -0.038834   0.010849 529.191431  -3.580 0.000376 ***
## owo              -0.148315   0.039274 681.326023  -3.776 0.000173 ***
## reprep1          -0.641327   0.149484 673.891410  -4.290 2.05e-05 ***
## lff:owo           0.015147   0.003726 679.498716   4.066 5.35e-05 ***
## lff:reprep1       0.045902   0.014612 673.896934   3.141 0.001755 ** 
## owo:reprep1       0.193811   0.054868 673.934142   3.532 0.000440 ***
## lff:owo:reprep1  -0.019009   0.005185 673.945799  -3.666 0.000266 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) lff    owo    reprp1 lff:ow lff:r1 ow:rp1
## lff         -0.892                                          
## owo         -0.780  0.831                                   
## reprep1     -0.614  0.664  0.607                            
## lff:owo      0.777 -0.856 -0.985 -0.605                     
## lff:reprep1  0.604 -0.676 -0.588 -0.983  0.604              
## owo:reprep1  0.533 -0.567 -0.700 -0.867  0.689  0.839       
## lff:w:rprp1 -0.533  0.584  0.692  0.867 -0.700 -0.864 -0.987
```

``` r
plot_model(modCTR, type='pred', terms = c('lff', 'rep'))
```

<div class="figure" style="text-align: left">
<img src="plots/mod_open_restricted-4.png" alt="plot of chunk mod_open_restricted" width="800px" />
<p class="caption">plot of chunk mod_open_restricted</p>
</div>

``` r
plot_model(modCTR, type='pred', terms = c('lff', 'rep', 'owo'))
```

<div class="figure" style="text-align: left">
<img src="plots/mod_open_restricted-5.png" alt="plot of chunk mod_open_restricted" width="800px" />
<p class="caption">plot of chunk mod_open_restricted</p>
</div>

``` r
table(ctr$owo)
```

```
## 
##   1   2   3   4   5   6   7 
## 198 198 152  65  22  43  22
```

The three models restricted to only a part of the material give somewhat different results:  the results of AAT mirror the results of the overall analysis while for SAND and CTR the analysis gives higher order interactions with word order. 

In SAND material the reduction of RT for low frequency words is qualifiyed by an interaction with word order which does not shows up in the first open word positions but only in later ones.

For CTR material, the control of semantic and syntactic violation the general pattern emerges for first word position while a paradoxial cost of reading for high frequency words appear for late words in the sentence.  Since the lits were not balanced and we are dealing with a limited number of long sentences this may be due to some overfitting of the data.

The overall pattern of the interaction between frequency and repetition is confirmed. Moreover the qualification of these effect as a function of word order seems unstable across the three blocks. Further analysis with a larger sample of subjects (and correctly counterbalanced blocks 3) should be done to better understand these aspects.

## Same model for each subject

Despite across groups of items some effects of open word order emerges, also in  interaction with  frequency and repetition, we will test on single subjects a parsimonious simlifyied model including only lff and rep in interaction which shows large overall effects.

The following plots report the the estimates of the effects of single subject models and the error bar represent the estimated standard deviation,  If the bar does not include 0 the effect is to be considered singnificant/large.



``` r
mod = lmer(lrt ~ lff*rep + (1|item) + (1|scode), data=regopen2)
summary(mod)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: lrt ~ lff * rep + (1 | item) + (1 | scode)
##    Data: regopen2
## 
## REML criterion at convergence: -228.7
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.3962 -0.6461 -0.1125  0.4874  5.0105 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  item     (Intercept) 0.003659 0.06049 
##  scode    (Intercept) 0.034344 0.18532 
##  Residual             0.050368 0.22443 
## Number of obs: 2705, groups:  item, 36; scode, 20
## 
## Fixed effects:
##               Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)  7.012e+00  5.794e-02  6.999e+01 121.024  < 2e-16 ***
## lff         -2.148e-02  3.791e-03  1.910e+03  -5.666 1.69e-08 ***
## reprep1     -3.671e-01  4.972e-02  2.646e+03  -7.384 2.05e-13 ***
## lff:reprep1  1.513e-02  4.736e-03  2.646e+03   3.193  0.00142 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) lff    reprp1
## lff         -0.667              
## reprep1     -0.429  0.616       
## lff:reprep1  0.423 -0.626 -0.985
```

``` r
out = data.frame()
for (i in levels(regopen2$scode)) { 
	sb = subset(regopen2, scode ==i)
	tmp = lmer(lrt ~ lff*rep + (1|item), data=sb)
	sm = summary(tmp)
	h =  data.frame(sm$coefficients)
	h$term = c('int','lff','rep','lff:rep')
	h$scode = i
    h$age = sb$age[1]
	out = rbind(out,h)
}

out$upper = out$Estimate + out$Std..Error
out$lower = out$Estimate - out$Std..Error

a = subset(out, term =='int', select=c('scode','age', 'Estimate', 'lower','upper'))
colnames(a)[3:5] =c('int','int.l', 'int.h')
b = subset(out, term =='rep', select=c('scode','age', 'Estimate', 'lower','upper'))
colnames(b)[3:5] =c('rep','rep.l', 'rep.h')
c = subset(out, term =='lff', select=c('scode','age', 'Estimate', 'lower','upper'))
colnames(c)[3:5] =c('lff','lff.l', 'lff.h')
d = subset(out, term =='lff:rep', select=c('scode','age', 'Estimate', 'lower','upper'))
colnames(d)[3:5] =c('lff.rep','lff.rep.l', 'lff.rep.h')

e = merge(a,b)
e = merge(e,c)
e = merge(e,d)


ggplot() + geom_point(data=e, mapping=aes(x=int, y=scode), size=4, shape=21, fill="blue") +
   geom_errorbar(data=e, mapping=aes(x=int, y=scode, xmin=int.l, xmax=int.h)) +
   theme_bw() + ggtitle('Estimate of intercept') +
   xlab ('Estimate of the basic speed of reading at sentence beginning [log(ms)]') +
   ylab ("Subject's codes")
```

<div class="figure" style="text-align: left">
<img src="plots/mod_open_ss-1.png" alt="plot of chunk mod_open_ss" width="800px" />
<p class="caption">plot of chunk mod_open_ss</p>
</div>

``` r
ggplot() + geom_point(data=e, mapping=aes(x=rep, y=scode), size=4, shape=21, fill="blue") +
   geom_errorbar(data=e, mapping=aes(x=rep, y=scode, xmin=rep.l, xmax=rep.h)) +
   geom_vline(xintercept = 0, size=1) + theme_bw() + ggtitle('Estimate of main effect of repetition') +
   xlab ('Estimate of the effect of repetition [log(ms)]') +
   ylab ("Subject's codes")
```

<div class="figure" style="text-align: left">
<img src="plots/mod_open_ss-2.png" alt="plot of chunk mod_open_ss" width="800px" />
<p class="caption">plot of chunk mod_open_ss</p>
</div>

``` r
ggplot() + geom_point(data=e, mapping=aes(x=lff, y=scode), size=4, shape=21, fill="blue") +
   geom_errorbar(data=e, mapping=aes(x=lff, y=scode, xmin=lff.l, xmax=lff.h)) +
   geom_vline(xintercept = 0, size=1) + theme_bw() + ggtitle('Estimate of main effect of log Frequency') +
   xlab ('Estimate of the effect of log-frequency [log(ms)]') +
   ylab ("Subject's codes")
```

<div class="figure" style="text-align: left">
<img src="plots/mod_open_ss-3.png" alt="plot of chunk mod_open_ss" width="800px" />
<p class="caption">plot of chunk mod_open_ss</p>
</div>

``` r
ggplot() + geom_point(data=e, mapping=aes(x=lff.rep, y=scode), size=4, shape=21, fill="blue") +
   geom_errorbar(data=e, mapping=aes(x=lff.rep, y=scode, xmin=lff.rep.l, xmax=lff.rep.h)) +
   geom_vline(xintercept = 0, size=1) + theme_bw() + geom_label()  + ggtitle('Estimate of the interaction between  Frequancy and repetition') +
   xlab ('Estimate of the intefaction between log-frequency and repetition [log(ms)]') +
   ylab ("Subject's codes")
```

<div class="figure" style="text-align: left">
<img src="plots/mod_open_ss-4.png" alt="plot of chunk mod_open_ss" width="800px" />
<p class="caption">plot of chunk mod_open_ss</p>
</div>


The following plots  display two by two relations of these estimates (comments below).



``` r
ggplot(e, aes(x=int, y=lff, label=scode) ) + geom_point(data=e, mapping=aes(y=lff, x=int), size=4, shape=21, fill="blue") +  
   geom_errorbar(data=e, mapping=aes(x=int, y=lff, xmin=int.l, xmax=int.h)) +
   geom_errorbar(data=e, mapping=aes(x=int, y=lff, ymin=lff.l, ymax=lff.h)) +
   geom_hline(yintercept = 0, size=2) + geom_label() + theme_bw() + ggtitle('Frequency Effect as a function of global speed') +
   xlab ('Estimate of the basic spped of reading at sentence beginning [log(ms)]') +
   ylab ('Estimate of the effect of frequancy [log(ms)]') +
   geom_smooth(method='gam', se=0)
```

<div class="figure" style="text-align: left">
<img src="plots/mod_open_ss_effects-1.png" alt="plot of chunk mod_open_ss_effects" width="800px" />
<p class="caption">plot of chunk mod_open_ss_effects</p>
</div>

``` r
ggplot(e, aes(y=rep, x=int, label=scode) ) +
   geom_point(data=e, size=4, shape=21, fill="blue") +  
   geom_errorbar(data=e, mapping=aes(xmin=int.l, xmax=int.h)) +
   geom_errorbar(data=e, mapping=aes(ymin=rep.l, ymax=rep.h)) +
   geom_hline(yintercept = 0, size=1)  + geom_label() +
   theme_bw() + ggtitle('Repetition Effect as a function of global speed') +
   xlab ('Estimate of the basic spped of reading at sentence beginning [log(ms)]') +
   ylab ('Estimate of the effect of repetition [log(ms)]') +
   geom_smooth(method='gam', se=0)
```

<div class="figure" style="text-align: left">
<img src="plots/mod_open_ss_effects-2.png" alt="plot of chunk mod_open_ss_effects" width="800px" />
<p class="caption">plot of chunk mod_open_ss_effects</p>
</div>

``` r
ggplot(e, aes(y=lff.rep, x=lff, label=scode) ) +
   geom_point(data=e, size=4, shape=21, fill="blue") +  
   geom_errorbar(data=e, mapping=aes(xmin=lff.l, xmax=lff.h)) +
   geom_errorbar(data=e, mapping=aes(ymin=lff.rep.l, ymax=lff.rep.h)) +
   geom_hline(yintercept = 0, size=1) + geom_vline(xintercept = 0, size=1) + 
   geom_label() +
   theme_bw() + ggtitle('Frequency Effect as a function Frequency BY Repetition interaction') +
   xlab ('Estimate of magnitude of frequency by repetition interaction [log(ms)]') +
   ylab ('Estimate of the effect of ferquency in reading [log(ms)]') +
   geom_smooth(method='gam', se=0)
```

<div class="figure" style="text-align: left">
<img src="plots/mod_open_ss_effects-3.png" alt="plot of chunk mod_open_ss_effects" width="800px" />
<p class="caption">plot of chunk mod_open_ss_effects</p>
</div>


The plots show that, as it can be expected faster subjects show in general smaller effects of frequency and repetition.  Moreover, subjects which show small effects of frequency hardly reduce them during repetition.

The fastest subject (EBQ) also shows an inverse frequency effect,  possibly based on  due to some strategic effect or just at chance. This suggests not to put to much temporal pressure on the task. A couple of subject show very low speed but the interplay with frequency and repetition seems in line with the group.

It is interesting to note that both in the plot of frequency effect as a function of global speed and in the frequency by repetition effect as a function of frequency a couple of subjects seem to lie out of the group.  These subjects are not exteremely slow but show little effects of frequency and this is not reduced by repetition. Clearly this could be due to noise but it can be a sign that these subjects are not able as others to profit of sentence repetition to reduce the costs of access at low-frequency words.

We plot the model predictions of these two subjects below.


``` r
#  models on two subjects 58I  e QOL

sb = subset(regopen2, scode =='QOL')
tmp = lmer(rt ~ lff*rep + (1|item), data=sb)
sm = summary(tmp)
plot_model(tmp, type='pred', terms = c('lff', 'rep'), title = 'Predicted RT of frequency and repetition for subject QOL')
```

<div class="figure" style="text-align: left">
<img src="plots/mod_open_ss_outliers-1.png" alt="plot of chunk mod_open_ss_outliers" width="800px" />
<p class="caption">plot of chunk mod_open_ss_outliers</p>
</div>

``` r
sb = subset(regopen2, scode =='58I')
tmp = lmer(rt ~ lff*rep + (1|item), data=sb)
sm = summary(tmp)
plot_model(tmp, type='pred', terms = c('lff', 'rep'), title = 'Predicted RT of frequency and repetition for subject 58I')
```

<div class="figure" style="text-align: left">
<img src="plots/mod_open_ss_outliers-2.png" alt="plot of chunk mod_open_ss_outliers" width="800px" />
<p class="caption">plot of chunk mod_open_ss_outliers</p>
</div>


##  Resume

As first we analyzed with mixed model the effects of number of chracter, frequency of the owr,  word order (or position) and repetition in interaction.  Clearly a model like this is likely to be overfitting the results,  moreover some of these variable are not independent in language (Zipf law) or in our stimuli, inducing correlations of the predictors.

We have seen that nchar has a rather complex behaviour, mainly attributed to the nature of the maze task which become more difficoult for short characters. Moreover frequency effects and word position seem to have different effects in function and content word.  On the basis of these early analyises wich were mainly explorative we decided to run single subject models on:

a) a subset of the open words with more than 3 characters and not considering wp above 9

b) limiting the analisys to the comparison of reading and 1st repetition, since 2nd repetition does not seem to show such large qualitative difference

c) ignoring nchar and owo (open word order) as predictors since the first does not seem to be very important in this subset of the data and the second seems to show different trends in the three subset of the material we have.

The model applied to single subjects show that effects seem rather strong the interpretation of subject that do not pattern the others suggests that the technique could provide a sensitive measure of the reduction of frequency sensitivity in repetition. TODO... what does this mean in terms of WM and situational support? 
