Precision calculcations for weighted kappa
========================================================

When you are trying to justify the sample size for a research project 
that involves measuring agreement via Cohn's kappa (or weighted kappa)
You do this not through a power calculation, but through a calculation
of precision. You want your sample size to be large enough so that the
resulting confidence interval for kappa is reasonably narrow

Perfect independence is the case where the two raters assign
values in a perfectly indpendent manner. This means that the
probability for any combination of scores is equal to the 
product of the marginal probabilities.

When you 


```r
library("psych")

margin1 <- c(40, 30, 20, 10)
if (sum(margin1)!=1 & sum(margin1)!=100) {
  print("Warning: data does not sum to 1.0 or 100%")
}
margin2 <- c(40, 20, 20, 20)
if (sum(margin2)!=1 & sum(margin2)!=100) {
  print("Warning: data does not sum to 1.0 or 100%")
}
margin1 <- margin1 / sum(margin1)
margin2 <- margin2 / sum(margin2)

perfect_independence <- margin1 %o% margin2
print(perfect_independence)
```

```
##      [,1] [,2] [,3] [,4]
## [1,] 0.16 0.08 0.08 0.08
## [2,] 0.12 0.06 0.06 0.06
## [3,] 0.08 0.04 0.04 0.04
## [4,] 0.04 0.02 0.02 0.02
```

```r
cohen.kappa(200*perfect_independence, alpha=.05)
```

```
## Call: cohen.kappa1(x = x, w = w, n.obs = n.obs, alpha = alpha)
## 
## Cohen Kappa and Weighted Kappa correlation coefficients and confidence boundaries 
##                   lower estimate upper
## unweighted kappa -0.081        0 0.081
## weighted kappa   -0.135        0 0.135
## 
##  Number of subjects = 200
```


```r
margin_min <- pmin(margin1,margin2)
leftover1 <- margin1 - margin_min
leftover2 <- margin2 - margin_min
excess <- sum(leftover1)
leftover1 <- leftover1 / sum(leftover1)
leftover2 <- leftover2 / sum(leftover2)
n <- length(margin1)
perfect_agreement<- matrix(0, nrow=n, ncol=n)
for (i in 1:n) {
  perfect_agreement[i, i] <- margin_min[i]
}
perfect_agreement <- perfect_agreement + (leftover1 %o% leftover2)*excess
print(perfect_agreement)
```

```
##      [,1] [,2] [,3] [,4]
## [1,]  0.4  0.0  0.0  0.0
## [2,]  0.0  0.2  0.0  0.1
## [3,]  0.0  0.0  0.2  0.0
## [4,]  0.0  0.0  0.0  0.1
```

```r
print(sum(perfect_agreement))
```

```
## [1] 1
```

```r
cohen.kappa(200*perfect_agreement, alpha=.05)
```

```
## Call: cohen.kappa1(x = x, w = w, n.obs = n.obs, alpha = alpha)
## 
## Cohen Kappa and Weighted Kappa correlation coefficients and confidence boundaries 
##                  lower estimate upper
## unweighted kappa  0.81     0.86  0.92
## weighted kappa    0.77     0.83  0.90
## 
##  Number of subjects = 200
```


```r
for (wt in (1:9)/10) {
  mixed_agreement <- (1-wt)*perfect_independence + wt*perfect_agreement
  print(wt)
  print(mixed_agreement)
  print(cohen.kappa(200*mixed_agreement))
}
```

```
## [1] 0.1
##       [,1]  [,2]  [,3]  [,4]
## [1,] 0.184 0.072 0.072 0.072
## [2,] 0.108 0.074 0.054 0.064
## [3,] 0.072 0.036 0.056 0.036
## [4,] 0.036 0.018 0.018 0.028
## Call: cohen.kappa1(x = x, w = w, n.obs = n.obs, alpha = alpha)
## 
## Cohen Kappa and Weighted Kappa correlation coefficients and confidence boundaries 
##                     lower estimate upper
## unweighted kappa -0.00064    0.086  0.17
## weighted kappa   -0.05359    0.083  0.22
## 
##  Number of subjects = 200 
## [1] 0.2
##       [,1]  [,2]  [,3]  [,4]
## [1,] 0.208 0.064 0.064 0.064
## [2,] 0.096 0.088 0.048 0.068
## [3,] 0.064 0.032 0.072 0.032
## [4,] 0.032 0.016 0.016 0.036
## Call: cohen.kappa1(x = x, w = w, n.obs = n.obs, alpha = alpha)
## 
## Cohen Kappa and Weighted Kappa correlation coefficients and confidence boundaries 
##                  lower estimate upper
## unweighted kappa 0.082     0.17  0.26
## weighted kappa   0.029     0.17  0.30
## 
##  Number of subjects = 200 
## [1] 0.3
##       [,1]  [,2]  [,3]  [,4]
## [1,] 0.232 0.056 0.056 0.056
## [2,] 0.084 0.102 0.042 0.072
## [3,] 0.056 0.028 0.088 0.028
## [4,] 0.028 0.014 0.014 0.044
## Call: cohen.kappa1(x = x, w = w, n.obs = n.obs, alpha = alpha)
## 
## Cohen Kappa and Weighted Kappa correlation coefficients and confidence boundaries 
##                  lower estimate upper
## unweighted kappa  0.17     0.26  0.35
## weighted kappa    0.11     0.25  0.39
## 
##  Number of subjects = 200 
## [1] 0.4
##       [,1]  [,2]  [,3]  [,4]
## [1,] 0.256 0.048 0.048 0.048
## [2,] 0.072 0.116 0.036 0.076
## [3,] 0.048 0.024 0.104 0.024
## [4,] 0.024 0.012 0.012 0.052
## Call: cohen.kappa1(x = x, w = w, n.obs = n.obs, alpha = alpha)
## 
## Cohen Kappa and Weighted Kappa correlation coefficients and confidence boundaries 
##                  lower estimate upper
## unweighted kappa  0.25     0.34  0.44
## weighted kappa    0.20     0.33  0.47
## 
##  Number of subjects = 200 
## [1] 0.5
##      [,1] [,2] [,3] [,4]
## [1,] 0.28 0.04 0.04 0.04
## [2,] 0.06 0.13 0.03 0.08
## [3,] 0.04 0.02 0.12 0.02
## [4,] 0.02 0.01 0.01 0.06
## Call: cohen.kappa1(x = x, w = w, n.obs = n.obs, alpha = alpha)
## 
## Cohen Kappa and Weighted Kappa correlation coefficients and confidence boundaries 
##                  lower estimate upper
## unweighted kappa  0.34     0.43  0.52
## weighted kappa    0.29     0.42  0.55
## 
##  Number of subjects = 200 
## [1] 0.6
##       [,1]  [,2]  [,3]  [,4]
## [1,] 0.304 0.032 0.032 0.032
## [2,] 0.048 0.144 0.024 0.084
## [3,] 0.032 0.016 0.136 0.016
## [4,] 0.016 0.008 0.008 0.068
## Call: cohen.kappa1(x = x, w = w, n.obs = n.obs, alpha = alpha)
## 
## Cohen Kappa and Weighted Kappa correlation coefficients and confidence boundaries 
##                  lower estimate upper
## unweighted kappa  0.43     0.52  0.61
## weighted kappa    0.38     0.50  0.62
## 
##  Number of subjects = 200 
## [1] 0.7
##       [,1]  [,2]  [,3]  [,4]
## [1,] 0.328 0.024 0.024 0.024
## [2,] 0.036 0.158 0.018 0.088
## [3,] 0.024 0.012 0.152 0.012
## [4,] 0.012 0.006 0.006 0.076
## Call: cohen.kappa1(x = x, w = w, n.obs = n.obs, alpha = alpha)
## 
## Cohen Kappa and Weighted Kappa correlation coefficients and confidence boundaries 
##                  lower estimate upper
## unweighted kappa  0.52     0.60  0.69
## weighted kappa    0.47     0.58  0.70
## 
##  Number of subjects = 200 
## [1] 0.8
##       [,1]  [,2]  [,3]  [,4]
## [1,] 0.352 0.016 0.016 0.016
## [2,] 0.024 0.172 0.012 0.092
## [3,] 0.016 0.008 0.168 0.008
## [4,] 0.008 0.004 0.004 0.084
## Call: cohen.kappa1(x = x, w = w, n.obs = n.obs, alpha = alpha)
## 
## Cohen Kappa and Weighted Kappa correlation coefficients and confidence boundaries 
##                  lower estimate upper
## unweighted kappa  0.61     0.69  0.77
## weighted kappa    0.56     0.67  0.77
## 
##  Number of subjects = 200 
## [1] 0.9
##       [,1]  [,2]  [,3]  [,4]
## [1,] 0.376 0.008 0.008 0.008
## [2,] 0.012 0.186 0.006 0.096
## [3,] 0.008 0.004 0.184 0.004
## [4,] 0.004 0.002 0.002 0.092
## Call: cohen.kappa1(x = x, w = w, n.obs = n.obs, alpha = alpha)
## 
## Cohen Kappa and Weighted Kappa correlation coefficients and confidence boundaries 
##                  lower estimate upper
## unweighted kappa  0.71     0.78  0.84
## weighted kappa    0.66     0.75  0.84
## 
##  Number of subjects = 200
```

