---
title: "Repetition Maze in aging - Violations"
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

We select only correct sentences from the 3rd block

We consider only correct trials and RT<3000ms.

The three levels of the variable repetition (rep) and encoded as "read" (1st presentation), "rep1" (2nd presentation) and "rep2" (3rd presentation).


Syntactic and Semantic violation will be studies separately to allow for simpler modeling and the pattern for the two types of violation will be compared qualitativey.

The critical word is always wp=2 (the third word of the sentence, wp starts from 0). 


Given the strong unbalancedness on word length between ListA with has many word after the violations and listB  which typically has only one word after the violation (error  of counterbalancing)  we will first model the effect in the whole sentence but then we will limit the more carefull analysis to the first 4 words of the sentences (wp =0,1,2,3) that are present in both list.

The error in counterbalancing the lists prevents us to study in depth long spillover effects and end of sentence effect (the reason why we had longer sentences in list A)  but, on another way, it offers us the possibility to check if local effects (on target word wp=2 and on the  following wp=3) are similar in teh two list independently from the fact that for half of the subjects (who have seen list B)  wp=3 is also the end of the sentence and for the other half it is not. If similar effects are found for listA and listB the generality of the results would be clearly stronger.


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

se =  subset(d, cond %in% c('SEC','SEV'))
sy =  subset(d, cond %in% c('SYC','SYV'))
```
# Semantic Violation 

Combined effect of violation, word position and repetition across the whole data. Explorative plots with wp as scale factor.


``` r
ggplot(se, aes(x=wp, y=rt, color=cond))+ geom_jitter(width=0.2, height=0, size=0.3) + facet_wrap(se$rep) + 
    ggtitle('RT as a function of word position') + 
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", fx = TRUE, k = 7))
```

<div class="figure" style="text-align: left">
<img src="plots/vio-se-desc-1.png" alt="plot of chunk vio-se-desc" width="800px" />
<p class="caption">plot of chunk vio-se-desc</p>
</div>

There is an effect at the target word and the effect seems even larger and involve also the previous word (the noun) in repetition 1,  in repetition 2 the effect seems to be largely reduced.

Statistical anlyses restricted to WP 1,2,3. WP in the following are considered no more as a continuous variables but levels of non -ordered factors.




``` r
sesh = subset(se, wp %in% c(1,2,3))
sesh$wp = factor(sesh$wp)


l = lmer(lrt~rep*cond*wp + (1|item) + (1|scode), data = sesh)
plot_model(l, type='pred', terms=c('wp', 'cond','rep'))
```

<div class="figure" style="text-align: left">
<img src="plots/vio-se-stat-1.png" alt="plot of chunk vio-se-stat" width="800px" />
<p class="caption">plot of chunk vio-se-stat</p>
</div>

``` r
summary(l)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: lrt ~ rep * cond * wp + (1 | item) + (1 | scode)
##    Data: sesh
## 
## REML criterion at convergence: 283.8
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.9097 -0.6615 -0.1473  0.4904  4.0736 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  item     (Intercept) 0.002799 0.05291 
##  scode    (Intercept) 0.036120 0.19005 
##  Residual             0.061938 0.24887 
## Number of obs: 1770, groups:  item, 20; scode, 20
## 
## Fixed effects:
##                       Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)          6.774e+00  5.209e-02  3.907e+01 130.053  < 2e-16 ***
## reprep1             -2.395e-01  3.529e-02  1.716e+03  -6.787 1.58e-11 ***
## reprep2             -2.129e-01  3.537e-02  1.716e+03  -6.017 2.16e-09 ***
## condSEV              5.981e-02  4.266e-02  1.072e+02   1.402   0.1637    
## wp2                  5.420e-02  3.538e-02  1.716e+03   1.532   0.1257    
## wp3                  7.220e-02  3.547e-02  1.716e+03   2.036   0.0419 *  
## reprep1:condSEV      9.779e-02  5.010e-02  1.716e+03   1.952   0.0511 .  
## reprep2:condSEV     -6.915e-03  5.023e-02  1.716e+03  -0.138   0.8905    
## reprep1:wp2         -6.170e-03  4.997e-02  1.716e+03  -0.123   0.9017    
## reprep2:wp2          4.720e-03  5.003e-02  1.716e+03   0.094   0.9248    
## reprep1:wp3         -1.840e-02  5.003e-02  1.716e+03  -0.368   0.7130    
## reprep2:wp3         -6.017e-02  5.003e-02  1.716e+03  -1.203   0.2293    
## condSEV:wp2          5.886e-02  5.044e-02  1.716e+03   1.167   0.2434    
## condSEV:wp3         -5.209e-02  5.050e-02  1.716e+03  -1.031   0.3025    
## reprep1:condSEV:wp2 -3.850e-02  7.109e-02  1.716e+03  -0.542   0.5882    
## reprep2:condSEV:wp2 -7.211e-02  7.109e-02  1.716e+03  -1.014   0.3106    
## reprep1:condSEV:wp3 -2.060e-02  7.105e-02  1.716e+03  -0.290   0.7719    
## reprep2:condSEV:wp3  1.009e-01  7.113e-02  1.716e+03   1.418   0.1562    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

No clear significant effects of violation or interactions with wp and rep emerge.  We also run an analysis restricted to wp2 (the target)



``` r
se2 = subset(se, wp == 2)


l = lmer(lrt~rep*cond + (1|item) + (1|scode), data = se2)
plot_model(l, type='pred', terms=c('rep','cond'))
```

<div class="figure" style="text-align: left">
<img src="plots/vio-se-target-1.png" alt="plot of chunk vio-se-target" width="800px" />
<p class="caption">plot of chunk vio-se-target</p>
</div>

``` r
summary(l)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: lrt ~ rep * cond + (1 | item) + (1 | scode)
##    Data: se2
## 
## REML criterion at convergence: 60.5
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.2820 -0.6635 -0.1024  0.5097  3.8936 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  item     (Intercept) 0.002216 0.04708 
##  scode    (Intercept) 0.035136 0.18744 
##  Residual             0.054939 0.23439 
## Number of obs: 589, groups:  item, 20; scode, 20
## 
## Fixed effects:
##                  Estimate Std. Error        df t value Pr(>|t|)    
## (Intercept)       6.82876    0.05035  33.41434 135.628  < 2e-16 ***
## reprep1          -0.24688    0.03332 546.95877  -7.409 4.87e-13 ***
## reprep2          -0.20999    0.03332 546.95836  -6.302 6.07e-10 ***
## condSEV           0.11749    0.03986  60.23263   2.948  0.00455 ** 
## reprep1:condSEV   0.06146    0.04752 547.28064   1.293  0.19647    
## reprep2:condSEV  -0.07534    0.04740 547.36374  -1.589  0.11253    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) reprp1 reprp2 cndSEV r1:SEV
## reprep1     -0.331                            
## reprep2     -0.331  0.500                     
## condSEV     -0.388  0.418  0.418              
## rprp1:cnSEV  0.232 -0.701 -0.351 -0.603       
## rprp2:cnSEV  0.233 -0.352 -0.703 -0.605  0.507
```

In this analysis the effect of violation emerges clearly but it does not significantly interact with repetition. The cost persists across repetitions even if it appears numerically larger in repetition1 and smaller in repetition2.

# Syntactic Violation 

Combined effect of violation, word position and repetition across the whole data. Explorative plots with wp as scale factor.


``` r
ggplot(sy, aes(x=wp, y=rt, color=cond))+ geom_jitter(width=0.2, height=0, size=0.3) + facet_wrap(sy$rep) + 
    ggtitle('RT as a function of word position') + 
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", fx = TRUE, k = 7))
```

<div class="figure" style="text-align: left">
<img src="plots/vio-sy-desc-1.png" alt="plot of chunk vio-sy-desc" width="800px" />
<p class="caption">plot of chunk vio-sy-desc</p>
</div>


For syntactic violation the pattern seems to differ a lot.  The local effect is stronger in reading,  it moves to the previous word in rep1 and goes back to the target in rep 3.




There is an effect at the target word and the effect seems even larger and involve also the previous word (the noun) in repetition 1,  in repetition 2 the effect seems to be largely reduced.




``` r
sysh = subset(sy, wp %in% c(1,2,3))
sysh$wp = factor(sysh$wp)


l = lmer(lrt~rep*cond*wp + (1|item) + (1|scode), data = sysh)
plot_model(l, type='pred', terms=c('wp', 'cond','rep'))
```

<div class="figure" style="text-align: left">
<img src="plots/vio-sy-stat-1.png" alt="plot of chunk vio-sy-stat" width="800px" />
<p class="caption">plot of chunk vio-sy-stat</p>
</div>

``` r
summary(l)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: lrt ~ rep * cond * wp + (1 | item) + (1 | scode)
##    Data: sysh
## 
## REML criterion at convergence: 179.8
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.5581 -0.6327 -0.1697  0.4262  5.4456 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  item     (Intercept) 0.003824 0.06184 
##  scode    (Intercept) 0.026177 0.16179 
##  Residual             0.058391 0.24164 
## Number of obs: 1733, groups:  item, 20; scode, 20
## 
## Fixed effects:
##                       Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)            6.74179    0.04772   48.13632 141.280  < 2e-16 ***
## reprep1               -0.24210    0.03417 1678.96133  -7.084 2.05e-12 ***
## reprep2               -0.23053    0.03417 1678.96133  -6.746 2.09e-11 ***
## condSYV               -0.05683    0.04412   79.16841  -1.288 0.201436    
## wp2                    0.01799    0.03435 1679.10158   0.524 0.600580    
## wp3                   -0.03802    0.03543 1683.66351  -1.073 0.283432    
## reprep1:condSYV        0.22151    0.04871 1679.02994   4.548 5.81e-06 ***
## reprep2:condSYV        0.16256    0.04865 1679.07942   3.342 0.000851 ***
## reprep1:wp2            0.03993    0.04846 1679.03188   0.824 0.409985    
## reprep2:wp2            0.02132    0.04846 1679.03188   0.440 0.660004    
## reprep1:wp3            0.12716    0.04981 1679.06932   2.553 0.010764 *  
## reprep2:wp3            0.05699    0.04981 1679.08890   1.144 0.252752    
## condSYV:wp2            0.34827    0.04926 1679.33588   7.070 2.26e-12 ***
## condSYV:wp3            0.19727    0.04996 1681.87865   3.949 8.19e-05 ***
## reprep1:condSYV:wp2   -0.39660    0.06927 1679.11965  -5.726 1.22e-08 ***
## reprep2:condSYV:wp2   -0.30691    0.06923 1679.14779  -4.433 9.88e-06 ***
## reprep1:condSYV:wp3   -0.25317    0.07009 1679.31377  -3.612 0.000313 ***
## reprep2:condSYV:wp3   -0.20639    0.07009 1679.29405  -2.944 0.003280 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Strong effects of violation during reading emerge on wp2 (target) and as spillover (wp3). Also the interactions  with repetition above described on the explorative plots emerge clearly at a statistical level. 


We also run the analysis on target word only.


``` r
sy2 = subset(sy, wp == 2)

l = lmer(lrt~rep*cond + (1|item) + (1|scode), data = sy2)
plot_model(l, type='pred', terms=c('rep','cond'))
```

<div class="figure" style="text-align: left">
<img src="plots/vio-sy-target-1.png" alt="plot of chunk vio-sy-target" width="800px" />
<p class="caption">plot of chunk vio-sy-target</p>
</div>

``` r
summary(l)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: lrt ~ rep * cond + (1 | item) + (1 | scode)
##    Data: sy2
## 
## REML criterion at convergence: 155.4
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.6691 -0.6185 -0.1521  0.4828  5.0287 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  item     (Intercept) 0.01007  0.1003  
##  scode    (Intercept) 0.03178  0.1783  
##  Residual             0.06341  0.2518  
## Number of obs: 584, groups:  item, 20; scode, 20
## 
## Fixed effects:
##                  Estimate Std. Error        df t value Pr(>|t|)    
## (Intercept)       6.76154    0.05700  43.09932 118.619  < 2e-16 ***
## reprep1          -0.20433    0.03581 541.56114  -5.706 1.91e-08 ***
## reprep2          -0.21137    0.03581 541.56114  -5.903 6.32e-09 ***
## condSYV           0.28956    0.05809  32.33208   4.985 2.03e-05 ***
## reprep1:condSYV  -0.16962    0.05137 541.76455  -3.302  0.00102 ** 
## reprep2:condSYV  -0.14140    0.05137 541.83558  -2.752  0.00611 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) reprp1 reprp2 cndSYV r1:SYV
## reprep1     -0.318                            
## reprep2     -0.318  0.505                     
## condSYV     -0.500  0.312  0.312              
## rprp1:cnSYV  0.221 -0.697 -0.352 -0.455       
## rprp2:cnSYV  0.221 -0.352 -0.697 -0.455  0.514
```

In this analysis the effect of violation emerges clearly but it does not significantly interact with repetition. The cost persists across repetitions.

# Single subject estimates

The more relvant interactions of repetitions which may be analyzed at single subject level is:

- for semantic violations the effects seems larger in repetion than in read,  to evaluate this we will cosider both wp1 and w2 collapsing data across the two levels of wp

- for syntactic violation is the interaction between pre-target and target which changes fron read to repetition1

For these reason we will limit the analysis to the first repetition only and the target and pretarget words. For semantic condition we will not consider reading but only repetition1.

Before running models at the single subject level we run these analyses in the group.

The interesting fact that in rep2 the pattern of syntactic violation returns similar to the read condition is very interesting but hard to quantify at this stage of the research.



``` r
sy2w = subset(sy, wp %in% c(1,2) & rep!='rep2')
se2w = subset(se, wp %in% c(1,2) & rep=='rep1')

sem = lmer(lrt~cond + (1|item) + (1|scode), data = se2w)
sym = lmer(lrt~rep*cond*wp + (1|item) + (1|scode), data = sy2w)

summary(sem)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: lrt ~ cond + (1 | item) + (1 | scode)
##    Data: se2w
## 
## REML criterion at convergence: 158.5
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.0696 -0.6880 -0.1349  0.5051  3.2825 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  item     (Intercept) 0.004382 0.0662  
##  scode    (Intercept) 0.028194 0.1679  
##  Residual             0.074387 0.2727  
## Number of obs: 395, groups:  item, 20; scode, 20
## 
## Fixed effects:
##             Estimate Std. Error       df t value Pr(>|t|)    
## (Intercept)  6.55861    0.04717 28.02664 139.043  < 2e-16 ***
## condSEV      0.16593    0.04045 17.06708   4.102 0.000738 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##         (Intr)
## condSEV -0.427
```

``` r
plot_model(sem, type='pred', terms=c('cond'))
```

<div class="figure" style="text-align: left">
<img src="plots/vio-2words-1.png" alt="plot of chunk vio-2words" width="800px" />
<p class="caption">plot of chunk vio-2words</p>
</div>

``` r
summary(sym)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: lrt ~ rep * cond * wp + (1 | item) + (1 | scode)
##    Data: sy2w
## 
## REML criterion at convergence: 93.8
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.1690 -0.6180 -0.1581  0.4092  5.4331 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  item     (Intercept) 0.003626 0.06021 
##  scode    (Intercept) 0.030653 0.17508 
##  Residual             0.056839 0.23841 
## Number of obs: 780, groups:  item, 20; scode, 20
## 
## Fixed effects:
##                     Estimate Std. Error        df t value Pr(>|t|)    
## (Intercept)          6.72263    0.06893 139.55289  97.528  < 2e-16 ***
## reprep1             -0.28104    0.07547 735.62594  -3.724 0.000211 ***
## condSYV             -0.39755    0.08086 415.45426  -4.916 1.27e-06 ***
## wp                   0.01898    0.03390 735.83396   0.560 0.575686    
## reprep1:condSYV      0.61325    0.10771 735.75732   5.694 1.80e-08 ***
## reprep1:wp           0.03894    0.04781 735.70448   0.814 0.415625    
## condSYV:wp           0.34235    0.04862 736.29469   7.041 4.37e-12 ***
## reprep1:condSYV:wp  -0.39195    0.06835 735.87433  -5.734 1.43e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) reprp1 cndSYV wp     rp1:SYV rprp1: cnSYV:
## reprep1     -0.549                                           
## condSYV     -0.577  0.468                                    
## wp          -0.735  0.671  0.626                             
## rprp1:cnSYV  0.384 -0.701 -0.667 -0.470                      
## reprep1:wp   0.521 -0.949 -0.444 -0.709  0.665               
## condSYV:wp   0.512 -0.468 -0.894 -0.697  0.671   0.494       
## rprp1:cSYV: -0.364  0.663  0.635  0.496 -0.948  -0.699 -0.711
```

``` r
plot_model(sym, type='pred', terms=c('rep','cond','wp'))
```

<div class="figure" style="text-align: left">
<img src="plots/vio-2words-2.png" alt="plot of chunk vio-2words" width="800px" />
<p class="caption">plot of chunk vio-2words</p>
</div>


# Syntactic interaction


Since the overall model is singular for many subjects we esitimate separately at single subject level the effect of violation during reading at wp2 (target) during reading and at wp1 (pre-target)  during 1st repetition, using simple linear models.


``` r
syt_2 = subset(sy, wp==2 & rep=='read')
syp_1 = subset(sy, wp==1 & rep=='rep1')


out = data.frame()
for (i in levels(syt_2$scode)) { 
	sb = subset(syt_2, scode ==i)
	tmp = lm(lrt~cond, data=sb)
	sm = summary(tmp)
	h =  data.frame(sm$coefficients)
	h$term = c('int','vio')
	h$scode = i
	h$listType = sb$listType[1]
        h$age = sb$age[1]
	out = rbind(out,h)

}

out$upper = out$Estimate + out$Std..Error
out$lower = out$Estimate - out$Std..Error

a = subset(out, term =='vio', select=c('scode','age', 'listType', 'Estimate', 'lower','upper'))
colnames(a)[4:6] =c('tarRead','tarRead.l', 'tarRead.h')


out = data.frame()
for (i in levels(syp_1$scode)) { 
	sb = subset(syp_1, scode ==i)
	tmp = lm(lrt~cond, data=sb)
	sm = summary(tmp)
	h =  data.frame(sm$coefficients)
	h$term = c('int','vio')
	h$scode = i
	h$listType = sb$listType[1]
        h$age = sb$age[1]
	out = rbind(out,h)
}

out$upper = out$Estimate + out$Std..Error
out$lower = out$Estimate - out$Std..Error

b = subset(out, term =='vio', select=c('scode','age', 'listType', 'Estimate', 'lower','upper'))
colnames(b)[4:6] =c('preRep','preRep.l', 'preRep.h')


e = merge(a,b)

ggplot(e, aes(x=tarRead, y=scode, xmin=tarRead.l, xmax=tarRead.h, color=listType) ) + geom_point( size=4, shape=16) +
   geom_errorbar()  +  theme_bw() + ggtitle('Estimate of violation effect at target (verb, wp=2) during reading') +
   geom_vline(xintercept = 0, size=1) +
   xlab ('violation effect [log(ms)]') +
   ylab ("Subject's codes")
```

<div class="figure" style="text-align: left">
<img src="plots/vio-ss-sy-1.png" alt="plot of chunk vio-ss-sy" width="800px" />
<p class="caption">plot of chunk vio-ss-sy</p>
</div>

``` r
ggplot(e, aes(x=preRep, y=scode, xmin=preRep.l, xmax=preRep.h,color=listType)) + geom_point( size=4, shape=16) +
   geom_errorbar()  +  theme_bw() + ggtitle('Estimate of violation effect at the pretarget (noun, wp=1) in the first repetition') +
   geom_vline(xintercept = 0, size=1) +
   xlab ('violation effect [log(ms)]') +
   ylab ("Subject's codes")
```

<div class="figure" style="text-align: left">
<img src="plots/vio-ss-sy-2.png" alt="plot of chunk vio-ss-sy" width="800px" />
<p class="caption">plot of chunk vio-ss-sy</p>
</div>

``` r
ggplot(e, aes(x=tarRead,  y=preRep, label=scode, color=listType) ) + geom_point(size=4, shape=16) +  
   geom_errorbar(e, mapping=aes(y=preRep, xmin=tarRead.l, xmax=tarRead.h)) +
   geom_errorbar(e, mapping=aes(x=tarRead, ymin=preRep.l, ymax=preRep.h)) +
   geom_hline(yintercept = 0, size=1) +
   geom_vline(xintercept = 0, size=1) + geom_label() + theme_bw() +
   xlab('Violation effect on the target during reading') +
   ylab('Violation effect on the pre-target during repetition 1') +
   ggtitle('Combined sungle subject estimates of main syntactic violation effects on reading and repeating' )
```

<div class="figure" style="text-align: left">
<img src="plots/vio-ss-sy-3.png" alt="plot of chunk vio-ss-sy" width="800px" />
<p class="caption">plot of chunk vio-ss-sy</p>
</div>

It is interesting that for most subject both effect emerge (most points in the 1st quadrant) but these are not strongly linear, seemling rather independent in magnitude.

Three subjecs do not show either of the effects. EBQ  is a subject very fast in performing the task (already seen in the word frequency analysis as an "outlier" subject). ALD shows a clear and significant effect on reading but no effect on the pretarget on repeating,  SGC the opposite effect.  We will plot raw data for a qualitative analysis of their behaviour.



ald = subset(sy, scode=='ALD')

``` r
ggplot(ald, aes(x=factor(wp), y=rt, color=cond))+ geom_boxplot() +
 geom_jitter(width=0.2, height=0, size=0.3) + facet_wrap(ald$rep) + 
 ggtitle('Syntactic violation: ALD subject')
```

<div class="figure" style="text-align: left">
<img src="plots/vio-ssraw-sy-1.png" alt="plot of chunk vio-ssraw-sy" width="800px" />
<p class="caption">plot of chunk vio-ssraw-sy</p>
</div>

``` r
sgc = subset(sy, scode=='SGC')
ggplot(sgc, aes(x=factor(wp), y=rt, color=cond))+ geom_boxplot() +
 geom_jitter(width=0.2, height=0, size=0.3) + facet_wrap(sgc$rep) +
 ggtitle('Syntactic violation: SGC subject')
```

<div class="figure" style="text-align: left">
<img src="plots/vio-ssraw-sy-2.png" alt="plot of chunk vio-ssraw-sy" width="800px" />
<p class="caption">plot of chunk vio-ssraw-sy</p>
</div>

``` r
ebq = subset(sy, scode=='EBQ')
ggplot(ebq, aes(x=factor(wp), y=rt, color=cond))+ geom_boxplot() +
 geom_jitter(width=0.2, height=0, size=0.3) + facet_wrap(ebq$rep) +
 ggtitle('Syntactic violation: EBQ subject')
```

<div class="figure" style="text-align: left">
<img src="plots/vio-ssraw-sy-3.png" alt="plot of chunk vio-ssraw-sy" width="800px" />
<p class="caption">plot of chunk vio-ssraw-sy</p>
</div>


SGC  shows the slow-downs in reading not on the target but on the spillover, probably bacause he is very  fast.

EBQ does not show signs of costs for violations.

ALD interesting shows the cost during reading but no cost on the pre-target during repeating. Even if the control condition seems rather slow (probably by random noise) the RT for all violation in the pre-target seem rather fast,  suggesting that the violation does not interfere much with the RT of the subject of the sentence (this is interesting and it is the pattern we expect for people that do not have a strong syntactic representation of the sentence in WM...)


# Semantic violation in repetition1 across subjects



sem = lmer(lrt~cond + (1|item) + (1|scode), data = se2w)




``` r
se2w = subset(se, wp %in% c(1,2) & rep=='rep1')


out = data.frame()
for (i in levels(se2w$scode)) { 
	sb = subset(se2w, scode ==i)
	tmp = lm(lrt~cond, data=sb)
	sm = summary(tmp)
	h =  data.frame(sm$coefficients)
	h$term = c('int','vio')
	h$scode = i
	h$listType = sb$listType[1]
        h$age = sb$age[1]
	out = rbind(out,h)
}

out$upper = out$Estimate + out$Std..Error
out$lower = out$Estimate - out$Std..Error

a = subset(out, term =='vio', select=c('scode','age', 'listType', 'Estimate', 'lower','upper'))
colnames(a)[4:6] =c('semRep','semRep.l', 'semRep.h')



ggplot(a, aes(x=semRep, y=scode, xmin=semRep.l, xmax=semRep.h, color=listType) ) + geom_point( size=4, shape=16) +
   geom_errorbar()  +  theme_bw() + ggtitle('Estimate of semantic violation effect in repetition 1') +
   geom_vline(xintercept = 0, size=1) +
   xlab ('violation effect [log(ms)]') +
   ylab ("Subject's codes")
```

<div class="figure" style="text-align: left">
<img src="plots/vio-ss-se-1.png" alt="plot of chunk vio-ss-se" width="800px" />
<p class="caption">plot of chunk vio-ss-se</p>
</div>


The effect seems to be there at least numerically for most subjects, rather independently from list.   We will plot raw data for a qualitative inspection of the four subjects who does show the effect in the opposite way.




``` r
tmp = subset(se, scode %in% c('RDU','QOL', 'PGG', 'OKZ'))

ggplot(tmp, aes(x=factor(wp), y=rt, color=cond))+ geom_boxplot() +
  geom_jitter(width=0.2, height=0, size=0.3) + facet_grid(tmp$rep~tmp$scode) +
  ggtitle(paste('Semantic violation: ',s,'subject'))
```

<div class="figure" style="text-align: left">
<img src="plots/vio-ss-raw-se-1.png" alt="plot of chunk vio-ss-raw-se" width="800px" />
<p class="caption">plot of chunk vio-ss-raw-se</p>
</div>

All three subject does not seem so sensitive to semantic violation either in reading or in repeating, with the exception of some possible spillover on wp4 for QOL in reading.

By checking how these subject perform with frequency effect in the regular sentences QOL behaved differently from other subjects also in that analysis (one of the one who showed larger frequency effect in repetition1),  this second evidence converges in suggesting a less strong memory effect for lexical semantic aspects. Note that QOL does not show clearly different behaviour in the syntactic violations. Similar observations for ALD and ABQ which seems odd in the syntactic measures and do not show abnormal values in the semantic measures.  I think these observations suggest that the different indexes extracted by the task could differentiate strengths and weakness in different aspects of WM  for sentences. 
