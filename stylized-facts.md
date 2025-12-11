stylized facts
================
Jonathan Vasquez

## Setup

``` r
#install.packages("geepack", "marginaleffects") #install once if needed
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.1     ✔ stringr   1.5.2
    ## ✔ ggplot2   4.0.0     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(scales)
```

    ## 
    ## Attaching package: 'scales'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
library(geepack)
```

    ## Warning: package 'geepack' was built under R version 4.5.2

``` r
library(dplyr)
library(marginaleffects)
```

    ## Warning: package 'marginaleffects' was built under R version 4.5.2

``` r
library(ggplot2)
library(emmeans)
```

    ## Warning: package 'emmeans' was built under R version 4.5.2

    ## Welcome to emmeans.
    ## Caution: You lose important information if you filter this package's results.
    ## See '? untidy'

### Loading and Preparing Data

``` r
df <- read_csv("df_lag.csv")
```

    ## Rows: 6006 Columns: 83
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): iso_code, country, continent, UN_region, hdicode, development_stat...
    ## dbl (76): year, co2, population, share_co2, share_tmp_ghg, tmp_change_co2, t...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
df
```

    ## # A tibble: 6,006 × 83
    ##     year iso_code country       co2 population share_co2 share_tmp_ghg
    ##    <dbl> <chr>    <chr>       <dbl>      <dbl>     <dbl>         <dbl>
    ##  1  1991 AFG      Afghanistan  1.91   12238879     0.008         0.092
    ##  2  1992 AFG      Afghanistan  1.48   13278982     0.007         0.09 
    ##  3  1993 AFG      Afghanistan  1.49   14943174     0.007         0.089
    ##  4  1994 AFG      Afghanistan  1.45   16250799     0.006         0.087
    ##  5  1995 AFG      Afghanistan  1.42   17065836     0.006         0.085
    ##  6  1996 AFG      Afghanistan  1.37   17763265     0.006         0.084
    ##  7  1997 AFG      Afghanistan  1.30   18452100     0.005         0.083
    ##  8  1998 AFG      Afghanistan  1.28   19159996     0.005         0.082
    ##  9  1999 AFG      Afghanistan  1.09   19887791     0.004         0.082
    ## 10  2000 AFG      Afghanistan  1.05   20130334     0.004         0.081
    ## # ℹ 5,996 more rows
    ## # ℹ 76 more variables: tmp_change_co2 <dbl>, tmp_change_ghg <dbl>, gdp <dbl>,
    ## #   kyoto_ratification_year <dbl>, kyoto_us_ratification_year <dbl>,
    ## #   paris_ratification_year <dbl>, kyoto_dummy <dbl>, kyoto_us_dummy <dbl>,
    ## #   paris_dummy <dbl>, time <dbl>, ln_co2 <dbl>, ln_population <dbl>,
    ## #   ln_share_co2 <dbl>, ln_share_tmp_ghg <dbl>, ln_tmp_change_co2 <dbl>,
    ## #   ln_tmp_change_ghg <dbl>, ln_gdp <dbl>, zln_co2 <dbl>, …

``` r
#Setting index
df <- df |> 
  dplyr::arrange(country, year)

df$country <- factor(df$country)  # safest

# or create an explicit numeric id
df$country_id <- as.integer(factor(df$country))
```

``` r
# Releveling categorical variables
df$income <- relevel(factor(df$income), ref = "Low income")
df$hdicode <- relevel(factor(df$hdicode), ref = "Low")
```

``` r
## GEE with AR1 correlation structure
df <- df %>%
  arrange(country, year)

gee_ar1 <- geeglm(
 ln_co2 ~ 1 + ln_co2_lag1 + kyoto_dummy + paris_dummy + time_kyoto + time_paris + time + ln_population + income + kyoto_dummy * income + paris_dummy * income,
  data = df,
  id = country,
  waves = year,         
  family = gaussian,    
  corstr = "ar1",
  std.err = "san.se"     
)
summary(gee_ar1)
```

    ## 
    ## Call:
    ## geeglm(formula = ln_co2 ~ 1 + ln_co2_lag1 + kyoto_dummy + paris_dummy + 
    ##     time_kyoto + time_paris + time + ln_population + income + 
    ##     kyoto_dummy * income + paris_dummy * income, family = gaussian, 
    ##     data = df, id = country, waves = year, corstr = "ar1", std.err = "san.se")
    ## 
    ##  Coefficients:
    ##                                         Estimate    Std.err     Wald Pr(>|W|)
    ## (Intercept)                           -0.5621838  0.1894863    8.802 0.003008
    ## ln_co2_lag1                            0.9655221  0.0114964 7053.438  < 2e-16
    ## kyoto_dummy                            0.0562693  0.0138388   16.533 4.78e-05
    ## paris_dummy                            0.0975401  0.0235249   17.191 3.38e-05
    ## time_kyoto                            -0.0030712  0.0009653   10.123 0.001464
    ## time_paris                            -0.0030240  0.0007477   16.356 5.25e-05
    ## time                                   0.0027075  0.0009792    7.645 0.005692
    ## ln_population                          0.0351231  0.0116187    9.138 0.002503
    ## incomeHigh income                      0.1427227  0.0511098    7.798 0.005231
    ## incomeLower middle income              0.0772944  0.0274612    7.922 0.004883
    ## incomeUpper middle income              0.1042511  0.0375094    7.725 0.005447
    ## kyoto_dummy:incomeHigh income         -0.0403803  0.0106215   14.453 0.000144
    ## kyoto_dummy:incomeLower middle income -0.0194869  0.0132744    2.155 0.142103
    ## kyoto_dummy:incomeUpper middle income -0.0188883  0.0118774    2.529 0.111773
    ## paris_dummy:incomeHigh income         -0.0323049  0.0159288    4.113 0.042552
    ## paris_dummy:incomeLower middle income -0.0089190  0.0164300    0.295 0.587233
    ## paris_dummy:incomeUpper middle income -0.0232585  0.0145153    2.568 0.109080
    ##                                          
    ## (Intercept)                           ** 
    ## ln_co2_lag1                           ***
    ## kyoto_dummy                           ***
    ## paris_dummy                           ***
    ## time_kyoto                            ** 
    ## time_paris                            ***
    ## time                                  ** 
    ## ln_population                         ** 
    ## incomeHigh income                     ** 
    ## incomeLower middle income             ** 
    ## incomeUpper middle income             ** 
    ## kyoto_dummy:incomeHigh income         ***
    ## kyoto_dummy:incomeLower middle income    
    ## kyoto_dummy:incomeUpper middle income    
    ## paris_dummy:incomeHigh income         *  
    ## paris_dummy:incomeLower middle income    
    ## paris_dummy:incomeUpper middle income    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation structure = ar1 
    ## Estimated Scale Parameters:
    ## 
    ##             Estimate  Std.err
    ## (Intercept)  0.02338 0.007145
    ##   Link = identity 
    ## 
    ## Estimated Correlation Parameters:
    ##       Estimate Std.err
    ## alpha -0.03284 0.06491
    ## Number of clusters:   203  Maximum cluster size: 30

``` r
# Model Predictions for the Kyoto Protocol
plot_predictions(gee_ar1, by = c("kyoto_dummy", "income")) +
  theme_minimal() +
  labs(y = "Predicted y", x = "Kyoto participation")
```

![](stylized-facts_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
# Model Predictions for the Paris Agreement
plot_predictions(gee_ar1, by = c("paris_dummy", "income")) +
  theme_minimal() +
  labs(y = "Predicted y", x = "Kyoto participation")
```

![](stylized-facts_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
emm_p <- emmeans(gee_ar1, ~ paris_dummy | income)
p <- plot(emm_p)

pdf("emm_plot_paris.pdf", width = 11, height = 8)
print(p)
dev.off()
```

    ## png 
    ##   2

``` r
pairs(emm_p)
```

    ## income = Low income:
    ##  contrast                    estimate     SE   df t.ratio p.value
    ##  paris_dummy0 - paris_dummy1  -0.0975 0.0235 5989  -4.146  <.0001
    ## 
    ## income = High income:
    ##  contrast                    estimate     SE   df t.ratio p.value
    ##  paris_dummy0 - paris_dummy1  -0.0652 0.0188 5989  -3.466  0.0005
    ## 
    ## income = Lower middle income:
    ##  contrast                    estimate     SE   df t.ratio p.value
    ##  paris_dummy0 - paris_dummy1  -0.0886 0.0217 5989  -4.083  <.0001
    ## 
    ## income = Upper middle income:
    ##  contrast                    estimate     SE   df t.ratio p.value
    ##  paris_dummy0 - paris_dummy1  -0.0743 0.0205 5989  -3.620  0.0003
    ## 
    ## Results are averaged over the levels of: kyoto_dummy

``` r
emm_k <- emmeans(gee_ar1, ~ kyoto_dummy | income)
k <- plot(emm_k)

pdf("emm_plot_kyoto.pdf", width = 11, height = 8)
print(k)
dev.off()
```

    ## png 
    ##   2

``` r
pairs(emm_k)
```

    ## income = Low income:
    ##  contrast                    estimate     SE   df t.ratio p.value
    ##  kyoto_dummy0 - kyoto_dummy1  -0.0563 0.0138 5989  -4.066  <.0001
    ## 
    ## income = High income:
    ##  contrast                    estimate     SE   df t.ratio p.value
    ##  kyoto_dummy0 - kyoto_dummy1  -0.0159 0.0123 5989  -1.290  0.1971
    ## 
    ## income = Lower middle income:
    ##  contrast                    estimate     SE   df t.ratio p.value
    ##  kyoto_dummy0 - kyoto_dummy1  -0.0368 0.0150 5989  -2.449  0.0144
    ## 
    ## income = Upper middle income:
    ##  contrast                    estimate     SE   df t.ratio p.value
    ##  kyoto_dummy0 - kyoto_dummy1  -0.0374 0.0142 5989  -2.626  0.0087
    ## 
    ## Results are averaged over the levels of: paris_dummy
