
<!-- README.md is generated from README.Rmd. Please edit that file -->
standardize 0.1.1
=================

The *standardize* package provides tools for controlling continuous variable scaling and factor contrasts. The goal of these standardizations is to keep the regression parameters on similar scales, and to ensure that the intercept (which is the predicted value of an observation when all other coefficients are multiplied by 0) represents the corrected mean (i.e. the predicted value for an observation which is average in every way, holding covariates at their mean values and averaging over group differences in factors). When the predictors are all on a similar scale, there are computational benefits for both frequentist and Bayesian approaches in mixed effects regressions, reasonable Bayesian priors are easier to specify, and regression output is easier to interpret. Take, for example, the **ptk** dataset included in the package (for which we will create a new ordered factor *preheight*):

``` r
library(standardize)

ptk$preheight <- "Mid"
ptk$preheight[ptk$prevowel == "a"] <- "Low"
ptk$preheight[ptk$prevowel %in% c("i", "u")] <- "High"
ptk$preheight <- factor(ptk$preheight, ordered = TRUE, levels = c("Low",
  "Mid", "High"))

summary(ptk)
#>       cdur            vdur             place            stress    prevowel
#>  Min.   : 61.0   Min.   :  0.00   Bilabial:228   Post-Tonic:250   a:217   
#>  1st Qu.:117.0   1st Qu.: 49.00   Dental  :275   Tonic     :252   e:169   
#>  Median :133.0   Median : 62.00   Velar   :248   Unstressed:249   i:146   
#>  Mean   :137.5   Mean   : 60.69                                   o:151   
#>  3rd Qu.:155.0   3rd Qu.: 77.00                                   u: 68   
#>  Max.   :281.0   Max.   :190.00                                           
#>                                                                           
#>  posvowel    wordpos       wordfreq        speechrate         sex     
#>  a:212    Initial:322   Min.   :     1   Min.   : 2.000   Female:384  
#>  e:144    Medial :429   1st Qu.:  1104   1st Qu.: 5.000   Male  :367  
#>  i: 86                  Median :  4899   Median : 6.000               
#>  o:255                  Mean   : 18840   Mean   : 5.812               
#>  u: 54                  3rd Qu.: 26610   3rd Qu.: 7.000               
#>                         Max.   :158168   Max.   :10.000               
#>                                                                       
#>     speaker    preheight 
#>  s02    : 45   Low :217  
#>  s03    : 45   Mid :320  
#>  s04    : 45   High:214  
#>  s07    : 45             
#>  s11    : 45             
#>  s16    : 44             
#>  (Other):482
```

Suppose we want to fit a linear mixed effects regression with total consonant duration *cdur* as the response, *place*, *stress*, *preheight*, the natural log of *wordfreq*, and *speaker*-relative *speechrate* as fixed effects, and random intercepts for *speaker*. The variables for this regression can be easily placed into a standardized space with the **standardize** function:

``` r
sdat <- standardize(cdur ~ place + stress + preheight + log(wordfreq) +
  scale_by(speechrate ~ speaker) + (1 | speaker), ptk)
  
sdat
#> 
#> Call:
#> standardize(formula = cdur ~ place + stress + preheight + log(wordfreq) + 
#>     scale_by(speechrate ~ speaker) + (1 | speaker), data = ptk)
#> 
#> Standardized Formula:
#> cdur ~ place + stress + preheight + log_wordfreq + scale_speechrate_by_speaker + 
#>     (1 | speaker)
#> 
#> Variables:
#>  Variable                       Standardized Name           Class   
#>  cdur                           cdur                        numeric 
#>  place                          place                       factor  
#>  stress                         stress                      factor  
#>  preheight                      preheight                   ordered 
#>  log(wordfreq)                  log_wordfreq                numeric 
#>  scale_by(speechrate ~ speaker) scale_speechrate_by_speaker scaledby
#>  speaker                        speaker                     group   
#> 
#> Response has mean 0 and standard deviation 1
#> 
#> Standardized Scale for the Predictors: 1
#> Continuous variables have mean 0 and standard deviation 'scale'
#>   (within-factor-level if scale_by was used)
#> Unordered factors have sum contrasts with deviation 'scale'
#> Ordered factors have orthogonal polynomial contrasts whose
#>   columns have standard deviation 'scale'
#> Grouping factors are coded as unordered factors with default contrasts

names(sdat)
#> [1] "call"      "scale"     "formula"   "family"    "data"      "pred"     
#> [7] "variables" "contrasts" "groups"

head(sdat$data)
#>          cdur  place     stress preheight log_wordfreq
#> 1  0.58032620  Velar Unstressed       Low  -1.64753617
#> 2  0.21456174  Velar Post-Tonic      High  -1.64753617
#> 3 -0.11795140 Dental Unstressed      High  -0.05570344
#> 4  0.01505386 Dental Post-Tonic      High  -1.90076049
#> 5  0.91283934  Velar      Tonic       Mid  -0.19203879
#> 6  1.51136300 Dental Post-Tonic      High  -0.22152148
#>   scale_speechrate_by_speaker speaker
#> 1                  -0.4984662     s01
#> 2                   1.3452937     s01
#> 3                  -0.4984662     s01
#> 4                   0.6454974     s01
#> 5                   2.2671736     s01
#> 6                  -2.3422261     s01

mean(sdat$data$cdur)
#> [1] -1.539605e-16
sd(sdat$data$cdur)
#> [1] 1

mean(sdat$data$log_wordfreq)
#> [1] 1.550764e-16
sd(sdat$data$log_wordfreq)
#> [1] 1
all.equal(scale(log(ptk$wordfreq))[, 1], sdat$data$log_wordfreq[, 1])
#> [1] TRUE

with(sdat$data, tapply(scale_speechrate_by_speaker, speaker, mean))
#>           s01           s02           s03           s04           s05 
#> -2.960268e-16  3.165268e-18 -3.119346e-16  1.004790e-16  2.183358e-16 
#>           s06           s07           s08           s09           s10 
#> -7.174906e-17 -1.953684e-16 -2.200950e-16  1.468327e-16 -1.266348e-16 
#>           s11           s12           s13           s14           s15 
#> -2.923792e-16  3.850162e-16  1.980309e-16  6.325185e-17  1.427623e-16 
#>           s16           s17           s18 
#> -2.220446e-16  1.582818e-16 -2.505491e-16
with(sdat$data, tapply(scale_speechrate_by_speaker, speaker, sd))
#> s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15 s16 s17 s18 
#>   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1

sdat$contrasts
#> $place
#>          Bilabial Dental
#> Bilabial        1      0
#> Dental          0      1
#> Velar          -1     -1
#> 
#> $stress
#>            Post-Tonic Tonic
#> Post-Tonic          1     0
#> Tonic               0     1
#> Unstressed         -1    -1
#> 
#> $preheight
#>                 .L         .Q
#> Low  -1.000000e+00  0.5773503
#> Mid  -2.168241e-17 -1.1547005
#> High  1.000000e+00  0.5773503

sdat$groups
#> $speaker
#>  [1] "s01" "s02" "s03" "s04" "s05" "s06" "s07" "s08" "s09" "s10" "s11"
#> [12] "s12" "s13" "s14" "s15" "s16" "s17" "s18"
```

The default settings for **standardize** have placed all the continuous variables on unit scale, set (named) sum contrasts for all unordered factors, and orthogonal polynomial contrasts with column standard deviations of 1 for all ordered factors. In the case of *speechrate*, the call to **scale\_by** ensured that rather than simply placing *speechrate* on unit scale, it was placed on unit scale for each *speaker*, so that the resulting variable represents speaker-relative speech rate. We can then simply use the *formula* and *data* elements of the object returned by **standardize** to fit the mixed effects regression in this standardized space:

``` r
library(lme4)
#> Loading required package: Matrix

mod <- lmer(sdat$formula, sdat$data)

summary(mod)
#> Linear mixed model fit by REML ['lmerMod']
#> Formula: 
#> cdur ~ place + stress + preheight + log_wordfreq + scale_speechrate_by_speaker +  
#>     (1 | speaker)
#>    Data: sdat$data
#> 
#> REML criterion at convergence: 2018
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -2.5479 -0.6919 -0.1014  0.5883  4.2981 
#> 
#> Random effects:
#>  Groups   Name        Variance Std.Dev.
#>  speaker  (Intercept) 0.09375  0.3062  
#>  Residual             0.79081  0.8893  
#> Number of obs: 751, groups:  speaker, 18
#> 
#> Fixed effects:
#>                              Estimate Std. Error t value
#> (Intercept)                  0.015759   0.079462   0.198
#> placeBilabial                0.008255   0.047766   0.173
#> placeDental                 -0.011703   0.045913  -0.255
#> stressPost-Tonic            -0.062456   0.048407  -1.290
#> stressTonic                  0.212176   0.047974   4.423
#> preheight.L                  0.017645   0.046657   0.378
#> preheight.Q                  0.059551   0.039241   1.518
#> log_wordfreq                -0.037784   0.033815  -1.117
#> scale_speechrate_by_speaker -0.306867   0.033216  -9.239
#> 
#> Correlation of Fixed Effects:
#>             (Intr) plcBlb plcDnt strP-T strssT prhg.L prhg.Q lg_wrd
#> placeBilabl  0.024                                                 
#> placeDental -0.039 -0.500                                          
#> strssPst-Tn  0.004  0.053 -0.051                                   
#> stressTonic -0.006  0.003 -0.003 -0.524                            
#> preheight.L  0.009  0.075 -0.106 -0.272  0.276                     
#> preheight.Q  0.083 -0.041 -0.127  0.029 -0.059  0.023              
#> log_wordfrq  0.005 -0.092  0.062 -0.131 -0.029  0.106  0.093       
#> scl_spchr__  0.007  0.015 -0.043  0.058  0.046  0.061  0.073  0.013
```

The scaling of the predictors can be controlled through the **scale** argument to **standardize**. For example:

``` r
sdat <- standardize(cdur ~ place + stress + preheight + log(wordfreq) +
  scale_by(speechrate ~ speaker) + (1 | speaker), ptk, scale = 0.5)
  
sdat
#> 
#> Call:
#> standardize(formula = cdur ~ place + stress + preheight + log(wordfreq) + 
#>     scale_by(speechrate ~ speaker) + (1 | speaker), data = ptk, 
#>     scale = 0.5)
#> 
#> Standardized Formula:
#> cdur ~ place + stress + preheight + log_wordfreq + scale_speechrate_by_speaker + 
#>     (1 | speaker)
#> 
#> Variables:
#>  Variable                       Standardized Name           Class   
#>  cdur                           cdur                        numeric 
#>  place                          place                       factor  
#>  stress                         stress                      factor  
#>  preheight                      preheight                   ordered 
#>  log(wordfreq)                  log_wordfreq                numeric 
#>  scale_by(speechrate ~ speaker) scale_speechrate_by_speaker scaledby
#>  speaker                        speaker                     group   
#> 
#> Response has mean 0 and standard deviation 1
#> 
#> Standardized Scale for the Predictors: 0.5
#> Continuous variables have mean 0 and standard deviation 'scale'
#>   (within-factor-level if scale_by was used)
#> Unordered factors have sum contrasts with deviation 'scale'
#> Ordered factors have orthogonal polynomial contrasts whose
#>   columns have standard deviation 'scale'
#> Grouping factors are coded as unordered factors with default contrasts

names(sdat)
#> [1] "call"      "scale"     "formula"   "family"    "data"      "pred"     
#> [7] "variables" "contrasts" "groups"

head(sdat$data)
#>          cdur  place     stress preheight log_wordfreq
#> 1  0.58032620  Velar Unstressed       Low  -0.82376808
#> 2  0.21456174  Velar Post-Tonic      High  -0.82376808
#> 3 -0.11795140 Dental Unstressed      High  -0.02785172
#> 4  0.01505386 Dental Post-Tonic      High  -0.95038025
#> 5  0.91283934  Velar      Tonic       Mid  -0.09601939
#> 6  1.51136300 Dental Post-Tonic      High  -0.11076074
#>   scale_speechrate_by_speaker speaker
#> 1                  -0.2492331     s01
#> 2                   0.6726468     s01
#> 3                  -0.2492331     s01
#> 4                   0.3227487     s01
#> 5                   1.1335868     s01
#> 6                  -1.1711131     s01

mean(sdat$data$cdur)
#> [1] -1.539605e-16
sd(sdat$data$cdur)
#> [1] 1

mean(sdat$data$log_wordfreq)
#> [1] 7.753821e-17
sd(sdat$data$log_wordfreq)
#> [1] 0.5
all.equal(0.5 * scale(log(ptk$wordfreq))[, 1], sdat$data$log_wordfreq[, 1])
#> [1] TRUE

with(sdat$data, tapply(scale_speechrate_by_speaker, speaker, mean))
#>           s01           s02           s03           s04           s05 
#> -1.480134e-16  1.582634e-18 -1.559673e-16  5.023952e-17  1.091679e-16 
#>           s06           s07           s08           s09           s10 
#> -3.587453e-17 -9.768421e-17 -1.100475e-16  7.341635e-17 -6.331741e-17 
#>           s11           s12           s13           s14           s15 
#> -1.461896e-16  1.925081e-16  9.901544e-17  3.162593e-17  7.138116e-17 
#>           s16           s17           s18 
#> -1.110223e-16  7.914090e-17 -1.252746e-16
with(sdat$data, tapply(scale_speechrate_by_speaker, speaker, sd))
#> s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15 s16 s17 s18 
#> 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5

sdat$contrasts
#> $place
#>          Bilabial Dental
#> Bilabial      0.5    0.0
#> Dental        0.0    0.5
#> Velar        -0.5   -0.5
#> 
#> $stress
#>            Post-Tonic Tonic
#> Post-Tonic        0.5   0.0
#> Tonic             0.0   0.5
#> Unstressed       -0.5  -0.5
#> 
#> $preheight
#>                .L         .Q
#> Low  -5.00000e-01  0.2886751
#> Mid  -1.08412e-17 -0.5773503
#> High  5.00000e-01  0.2886751

sdat$groups
#> $speaker
#>  [1] "s01" "s02" "s03" "s04" "s05" "s06" "s07" "s08" "s09" "s10" "s11"
#> [12] "s12" "s13" "s14" "s15" "s16" "s17" "s18"
```

The **stanardize** function works by making use of the function **scale** from base *R*, as well as the *standardize* functions **scale\_by**, **named\_contr\_sum**, and **scaled\_contr\_poly**. For more details, install the package and see the vignette "Using the standardize Package".

Installation
============

To install the *standardize* package, call:

``` r
install.packages("standardize")
```
