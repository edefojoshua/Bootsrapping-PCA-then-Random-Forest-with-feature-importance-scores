Bootsrapping, PCA, then Random Forest with feature importance scores
================
Joshua Edefo
2024-08-17

Libraries

``` r
# Libraries
library(readxl)
library(boot)
library(dplyr)
library(usethis)
```

    ## Warning: package 'usethis' was built under R version 4.3.2

\#Boostrapping

Importing data

``` r
data <- read_excel("C:/Users/joe62/OneDrive - Aberystwyth University/Apps/Desktop/GP Prescribing/Wales/how to use data to provide insights.xlsx")
head(data)
```

    ## # A tibble: 6 × 4
    ##   date                demand service proximi
    ##   <dttm>               <dbl>   <dbl>   <dbl>
    ## 1 2024-08-07 00:00:00     37    1          0
    ## 2 2024-08-08 00:00:00     38    1.12       0
    ## 3 2024-08-09 00:00:00     38    1.14       0
    ## 4 2024-08-10 00:00:00     40    1.15       0
    ## 5 2024-08-11 00:00:00     42    1.16       0
    ## 6 2024-08-12 00:00:00     43    1.17       1

``` r
str(data)
```

    ## tibble [7 × 4] (S3: tbl_df/tbl/data.frame)
    ##  $ date   : POSIXct[1:7], format: "2024-08-07" "2024-08-08" ...
    ##  $ demand : num [1:7] 37 38 38 40 42 43 45
    ##  $ service: num [1:7] 1 1.12 1.14 1.15 1.16 1.17 1.18
    ##  $ proximi: num [1:7] 0 0 0 0 0 1 1

Bootstrappin of data

``` r
# Bootsrapping Demand
demand <- data$demand
mean_function <- function(demand, indices) {
  demand <- demand[indices]
  return(mean(demand))}
R <- 2000
bootstrap_results <- boot(demand, statistic = mean_function, R = R)
bootstrap_results
```

    ## 
    ## ORDINARY NONPARAMETRIC BOOTSTRAP
    ## 
    ## 
    ## Call:
    ## boot(data = demand, statistic = mean_function, R = R)
    ## 
    ## 
    ## Bootstrap Statistics :
    ##     original      bias    std. error
    ## t1* 40.42857 -0.02207143    1.016596

``` r
print(bootstrap_results)
```

    ## 
    ## ORDINARY NONPARAMETRIC BOOTSTRAP
    ## 
    ## 
    ## Call:
    ## boot(data = demand, statistic = mean_function, R = R)
    ## 
    ## 
    ## Bootstrap Statistics :
    ##     original      bias    std. error
    ## t1* 40.42857 -0.02207143    1.016596

``` r
demandb <- bootstrap_results$t
head (demandb) 
```

    ##          [,1]
    ## [1,] 39.42857
    ## [2,] 42.57143
    ## [3,] 38.57143
    ## [4,] 42.42857
    ## [5,] 39.42857
    ## [6,] 40.14286

``` r
# Bootsrapping Service
service <- data$service
mean_function2 <- function(service, indices) {
  # indices is a vector of resampled indices
  service <- service[indices]
  return(mean(service))}
R <- 2000
bootstrap_results2 <- boot(service, statistic = mean_function, R = R)
bootstrap_results2
```

    ## 
    ## ORDINARY NONPARAMETRIC BOOTSTRAP
    ## 
    ## 
    ## Call:
    ## boot(data = service, statistic = mean_function, R = R)
    ## 
    ## 
    ## Bootstrap Statistics :
    ##     original        bias    std. error
    ## t1* 1.131429 -0.0002892857  0.02204252

``` r
print(bootstrap_results2)
```

    ## 
    ## ORDINARY NONPARAMETRIC BOOTSTRAP
    ## 
    ## 
    ## Call:
    ## boot(data = service, statistic = mean_function, R = R)
    ## 
    ## 
    ## Bootstrap Statistics :
    ##     original        bias    std. error
    ## t1* 1.131429 -0.0002892857  0.02204252

``` r
serviceb <- bootstrap_results2$t
head(serviceb)
```

    ##          [,1]
    ## [1,] 1.064286
    ## [2,] 1.158571
    ## [3,] 1.128571
    ## [4,] 1.128571
    ## [5,] 1.104286
    ## [6,] 1.085714

``` r
## Bootsrapping Proximity to the hospital
proximi <- data$proximi
mean_function3 <- function(proximi, indices) {
  # indices is a vector of resampled indices
  proximi <- proximi[indices]
  return(mean(proximi))}
R <- 2000
bootstrap_results3 <- boot(proximi, statistic = mean_function, R = R)
bootstrap_results3
```

    ## 
    ## ORDINARY NONPARAMETRIC BOOTSTRAP
    ## 
    ## 
    ## Call:
    ## boot(data = proximi, statistic = mean_function, R = R)
    ## 
    ## 
    ## Bootstrap Statistics :
    ##      original      bias    std. error
    ## t1* 0.2857143 0.007571429   0.1695068

``` r
print(bootstrap_results3)
```

    ## 
    ## ORDINARY NONPARAMETRIC BOOTSTRAP
    ## 
    ## 
    ## Call:
    ## boot(data = proximi, statistic = mean_function, R = R)
    ## 
    ## 
    ## Bootstrap Statistics :
    ##      original      bias    std. error
    ## t1* 0.2857143 0.007571429   0.1695068

``` r
proximib <- bootstrap_results2$t
head(proximib)
```

    ##          [,1]
    ## [1,] 1.064286
    ## [2,] 1.158571
    ## [3,] 1.128571
    ## [4,] 1.128571
    ## [5,] 1.104286
    ## [6,] 1.085714

Principal Component Analysis (PCA)

``` r
data_b <- data.frame(demandb, serviceb, proximib)
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 4.3.2

``` r
# Extracting the target Serviceb
data_b_w <-data_b[, -2]
#Standardised the scale
data_b_w_scaled <- scale(data_b_w)
head(data_b_w_scaled)
```

    ##         demandb   proximib
    ## [1,] -0.9619634 -3.0329374
    ## [2,]  2.1295850  1.2445105
    ## [3,] -1.8051129 -0.1164956
    ## [4,]  1.9890601 -0.1164956
    ## [5,] -0.9619634 -1.2182625
    ## [6,] -0.2593388 -2.0607901

``` r
# Perform PCA
pca_result <- prcomp(data_b_w_scaled, center = TRUE, scale. = TRUE)
# Print summary of PCA results
summary(pca_result)
```

    ## Importance of components:
    ##                          PC1   PC2
    ## Standard deviation     1.004 0.996
    ## Proportion of Variance 0.504 0.496
    ## Cumulative Proportion  0.504 1.000

``` r
# Extract PCA scores
pca_scores <- pca_result$x
head(pca_scores)
```

    ##            PC1        PC2
    ## [1,] -2.824821  1.4643998
    ## [2,]  2.385846  0.6258422
    ## [3,] -1.358782 -1.1940327
    ## [4,]  1.324103  1.4888527
    ## [5,] -1.541653  0.1812309
    ## [6,] -1.640579  1.2738185

``` r
# Get the first 2 principal components
pca_scores_first_2 <- pca_scores[, 1:2]

# Replace column names with custom names
colnames(pca_scores_first_2) <- c("demandb", "proxmib")
head (pca_scores_first_2)
```

    ##        demandb    proxmib
    ## [1,] -2.824821  1.4643998
    ## [2,]  2.385846  0.6258422
    ## [3,] -1.358782 -1.1940327
    ## [4,]  1.324103  1.4888527
    ## [5,] -1.541653  0.1812309
    ## [6,] -1.640579  1.2738185

``` r
# Combine PCA scores with the target variable
data <- data.frame(pca_scores_first_2, serviceb = data_b$serviceb)
head(data)
```

    ##     demandb    proxmib serviceb
    ## 1 -2.824821  1.4643998 1.064286
    ## 2  2.385846  0.6258422 1.158571
    ## 3 -1.358782 -1.1940327 1.128571
    ## 4  1.324103  1.4888527 1.128571
    ## 5 -1.541653  0.1812309 1.104286
    ## 6 -1.640579  1.2738185 1.085714

Random Forest

``` r
#Libraries for Random Forest
library(randomForest)
```

    ## Warning: package 'randomForest' was built under R version 4.3.2

    ## randomForest 4.7-1.1

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
library(lime)
```

    ## Warning: package 'lime' was built under R version 4.3.2

    ## 
    ## Attaching package: 'lime'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     explain

``` r
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
library(caTools)
```

    ## Warning: package 'caTools' was built under R version 4.3.2

``` r
library(ranger)
```

    ## Warning: package 'ranger' was built under R version 4.3.3

    ## 
    ## Attaching package: 'ranger'

    ## The following object is masked from 'package:randomForest':
    ## 
    ##     importance

``` r
split <- sample.split( data, SplitRatio= 0.7)
train<- subset(data, split == "TRUE")
test <- subset(data, split== "FALSE")

rf_model <- ranger(serviceb ~ ., data = train, importance = "impurity", oob.error = TRUE)
summary(rf_model)
```

    ##                           Length Class         Mode     
    ## predictions               1334   -none-        numeric  
    ## num.trees                    1   -none-        numeric  
    ## num.independent.variables    1   -none-        numeric  
    ## mtry                         1   -none-        numeric  
    ## min.node.size                1   -none-        numeric  
    ## variable.importance          2   -none-        numeric  
    ## prediction.error             1   -none-        numeric  
    ## forest                       7   ranger.forest list     
    ## splitrule                    1   -none-        character
    ## treetype                     1   -none-        character
    ## r.squared                    1   -none-        numeric  
    ## call                         5   -none-        call     
    ## importance.mode              1   -none-        character
    ## num.samples                  1   -none-        numeric  
    ## replace                      1   -none-        logical  
    ## dependent.variable.name      1   -none-        character

``` r
predictions <- predict(rf_model, data = test)$predictions
head(predictions)
```

    ## [1] 1.126475 1.086184 1.128518 1.157301 1.108950 1.122563

``` r
# Calculate RMSE
actual_values <- test$serviceb  # The actual target values in the test dataset
rmse <- sqrt(mean((predictions - actual_values)^2))
rmse
```

    ## [1] 0.001446

``` r
# rmse = 0.0016
# Compute R-squared
rss <- sum((actual_values - predictions)^2)  # Residual Sum of Squares
tss <- sum((actual_values - mean(actual_values))^2)  # Total Sum of Squares
r_squared <- 1 - (rss / tss)
r_squared
```

    ## [1] 0.9955411

``` r
# R square = 0.9943 (99.5%)
# Check the OOB error (Out-of-Bag error)
OOB_error <- rf_model$prediction.error
OOB_error
```

    ## [1] 3.072361e-06

Using Feature Importance socres to explain the contribution of each
variable

``` r
importance_scores <- importance(rf_model)
importance_scores
```

    ##   demandb   proxmib 
    ## 0.3331217 0.3224951

``` r
# similar feature importance score
```

session information

``` r
sessionInfo()
```

    ## R version 4.3.1 (2023-06-16 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 11 x64 (build 22631)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United Kingdom.utf8 
    ## [2] LC_CTYPE=English_United Kingdom.utf8   
    ## [3] LC_MONETARY=English_United Kingdom.utf8
    ## [4] LC_NUMERIC=C                           
    ## [5] LC_TIME=English_United Kingdom.utf8    
    ## 
    ## time zone: Europe/London
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] ranger_0.16.0        caTools_1.18.2       MASS_7.3-60         
    ##  [4] lime_0.5.3           randomForest_4.7-1.1 ggplot2_3.4.4       
    ##  [7] usethis_2.2.2        dplyr_1.1.3          boot_1.3-28.1       
    ## [10] readxl_1.4.3        
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] utf8_1.2.3        generics_0.1.3    bitops_1.0-7      shape_1.4.6      
    ##  [5] stringi_1.7.12    lattice_0.21-8    digest_0.6.33     magrittr_2.0.3   
    ##  [9] evaluate_0.21     grid_4.3.1        iterators_1.0.14  fastmap_1.1.1    
    ## [13] cellranger_1.1.0  foreach_1.5.2     Matrix_1.6-1.1    glmnet_4.1-8     
    ## [17] survival_3.7-0    purrr_1.0.2       fansi_1.0.4       scales_1.3.0     
    ## [21] codetools_0.2-19  cli_3.6.1         rlang_1.1.1       munsell_0.5.0    
    ## [25] splines_4.3.1     withr_2.5.0       yaml_2.3.7        tools_4.3.1      
    ## [29] colorspace_2.1-0  assertthat_0.2.1  vctrs_0.6.3       R6_2.5.1         
    ## [33] lifecycle_1.0.3   fs_1.6.3          pkgconfig_2.0.3   pillar_1.9.0     
    ## [37] gtable_0.3.4      glue_1.6.2        Rcpp_1.0.11       xfun_0.40        
    ## [41] tibble_3.2.1      tidyselect_1.2.0  rstudioapi_0.15.0 knitr_1.44       
    ## [45] htmltools_0.5.6   rmarkdown_2.25    gower_1.0.1       compiler_4.3.1
