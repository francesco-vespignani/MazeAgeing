---
title: "Repetition Maze in aging"
output: html
author: F.V.
date: November 2025
---

Load libraries

``` r
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

We also remove the condition WS (word salad)  since for an error these items were present only on two of the four lists. The three levels of teh variable repetition (rep) and encoded as "read" (1st presentation), "rep1" (2nd presentation) and "rep2" (3rd presentation).


``` r
data =  read.table('data/dataMaioli.tsv', sep='\t', header= TRUE, as.is=TRUE)
fac = c('scode','list','item','rep','POS','corrkey','correct','open')
for (f in fac)  {
    data[[f]]=factor(data [[f]])
}
levels(data$rep) = c('read','rep1','rep2')

print('*risposte corrette per soggetto*')
```

```
## [1] "*risposte corrette per soggetto*"
```

``` r
kable(table(data$scode, data$correct))
```



|    | FALSE| TRUE|
|:---|-----:|----:|
|2NT |     3|  858|
|58I |     5|  856|
|6RK |    10|  632|
|ALD |     7|  854|
|BDG |     8|  853|
|BXF |     8|  634|
|CJT |     6|  855|
|EBQ |     5|  637|
|FGH |     6|  855|
|N8N |     8|  853|
|OKZ |     6|  636|
|PFG |    10|  851|
|QOL |     2|  859|
|RDU |     9|  633|
|SGC |    12|  849|
|SH2 |     3|  639|
|T4Y |     3|  639|
|XQR |    10|  632|
|YZ1 |     3|  858|
|ZMU |     8|  634|

``` r
print('*numero outlier per soggetto*')
```

```
## [1] "*numero outlier per soggetto*"
```

``` r
kable(table(data$scode[data$rt>3000]))
```



|Var1 | Freq|
|:----|----:|
|2NT  |    2|
|58I  |    4|
|6RK  |   32|
|ALD  |    2|
|BDG  |    4|
|BXF  |   12|
|CJT  |    0|
|EBQ  |    1|
|FGH  |    1|
|N8N  |    0|
|OKZ  |    1|
|PFG  |   13|
|QOL  |    3|
|RDU  |    0|
|SGC  |    6|
|SH2  |   14|
|T4Y  |    2|
|XQR  |   10|
|YZ1  |    1|
|ZMU  |    1|

``` r
d = subset(data, rt<=3000 & correct == TRUE & cond !='WS')

d$lrt =  log(d$rt)
d$lff = log(d$countForm)
d$llf = log(d$countLemma)
```
# Analisys on regular sentences 

We first consider all regular sentences,  pooling toghether AAT, SAND and the control condisions of the third block (SEC, SYC). These sentence have a variable length, syntactic structure and difficulty. Separate data sets for open and close words are also created.


``` r
reg =  subset(d, cond %in% c('AAT','SAND','SEC','SYC'))
ow = subset(reg, open==1, select=-cwo)
cw = subset(reg, open!=1, select=-owo)
```

## Descriptive plots

These plots are on raw data to describe the data set and see if marcrosocpic trends emerge across trials and subjects.

###  bare repetition effect (all other variable collapsed)


``` r
ggplot(reg, aes(x=rep, y=rt))+ geom_violin() + geom_jitter(width=0.1, height=0, size=0.3)
```

<div class="figure" style="text-align: left">
<img src="plots/rt_rep-1.png" alt="plot of chunk rt_rep" width="800px" />
<p class="caption">plot of chunk rt_rep</p>
</div>

###  word position 

For this analisis word position is treated as a countinuos interval variable (it can be also considered, probably more correctly, as an ordinal factor).

**RT vs word position**

``` r
ggplot(reg, aes(x=wp, y=rt))+ geom_jitter(width=0.2, height=0, size=0.3) + facet_wrap(reg$rep) + 
    ggtitle('RT as a function of word position') + 
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", fx = TRUE, k = 5))
```

<div class="figure" style="text-align: left">
<img src="plots/rt_rep_wp-1.png" alt="plot of chunk rt_rep_wp" width="800px" />
<p class="caption">plot of chunk rt_rep_wp</p>
</div>

**RT vs word position split between open and close class word**

``` r
ggplot(reg, aes(x=wp, y=rt, color=open))+ geom_jitter(width=0.2, height=0, size=0.1) + facet_wrap(reg$rep) + 
    ggtitle('RT as a function of word position (separate fit for open and close words)') +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", fx = TRUE, k = 5))
```

<div class="figure" style="text-align: left">
<img src="plots/rt_rep_wp2-1.png" alt="plot of chunk rt_rep_wp2" width="800px" />
<p class="caption">plot of chunk rt_rep_wp2</p>
</div>

###  open/close word order 

Differently from previous plots we look as predictior the oreder of that type of word (open and close class) within the sentences.

**Open words**

``` r
ggplot(ow, aes(x=owo, y=rt))+ geom_jitter(width=0.2, height=0, size=0.3) + facet_wrap(ow$rep) + 
    ggtitle('RT for open class words as a function their order in the sentence') + 
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", fx = TRUE, k = 5))
```

<div class="figure" style="text-align: left">
<img src="plots/rt_rep_wo_open-1.png" alt="plot of chunk rt_rep_wo_open" width="800px" />
<p class="caption">plot of chunk rt_rep_wo_open</p>
</div>

**Close words**

``` r
ggplot(cw, aes(x=cwo, y=rt))+ geom_jitter(width=0.2, height=0, size=0.3) + facet_wrap(cw$rep) + 
    ggtitle('RT for close class  words as a function their order in the sentence') + 
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", fx = TRUE, k = 5))
```

<div class="figure" style="text-align: left">
<img src="plots/rt_rep_wo_close-1.png" alt="plot of chunk rt_rep_wo_close" width="800px" />
<p class="caption">plot of chunk rt_rep_wo_close</p>
</div>


###  number of characters

**RT vs word length**

``` r
ggplot(reg, aes(x=nchar, y=rt))+ geom_jitter(width=0.2, height=0, size=0.3) + facet_wrap(reg$rep) + 
    ggtitle('RT as a function of number of characters') + 
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", fx = TRUE, k = 5))
```

<div class="figure" style="text-align: left">
<img src="plots/rt_rep_nchar-1.png" alt="plot of chunk rt_rep_nchar" width="800px" />
<p class="caption">plot of chunk rt_rep_nchar</p>
</div>

**RT vs word length split for open and close class words**

``` r
ggplot(reg, aes(x=nchar, y=rt, color=open))+ geom_jitter(width=0.2, height=0, size=0.3) + facet_wrap(reg$rep) + 
    ggtitle('RT as a function of number of characters') + 
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", fx = TRUE, k = 5))
```

<div class="figure" style="text-align: left">
<img src="plots/rt_rep_nchar2-1.png" alt="plot of chunk rt_rep_nchar2" width="800px" />
<p class="caption">plot of chunk rt_rep_nchar2</p>
</div>

###  Log of word form frequency


**RT vs log word form frequency**

``` r
ggplot(reg, aes(x=lff, y=rt))+ geom_jitter(width=0.2, height=0, size=0.3) + facet_wrap(reg$rep) + 
    ggtitle('RT as a function of log of Frequency') + 
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", fx = TRUE, k = 5))
```

<div class="figure" style="text-align: left">
<img src="plots/rt_rep_lff-1.png" alt="plot of chunk rt_rep_lff" width="800px" />
<p class="caption">plot of chunk rt_rep_lff</p>
</div>

**RT vs log word form frequency (open close words seprately)**

``` r
ggplot(reg, aes(x=lff, y=rt, color=open))+ geom_jitter(width=0.2, height=0, size=0.3) + facet_wrap(reg$rep) + 
    ggtitle('RT as a function of log of Frequency') + 
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", fx = TRUE, k = 5))
```

<div class="figure" style="text-align: left">
<img src="plots/rt_rep_lff2-1.png" alt="plot of chunk rt_rep_lff2" width="800px" />
<p class="caption">plot of chunk rt_rep_lff2</p>
</div>
