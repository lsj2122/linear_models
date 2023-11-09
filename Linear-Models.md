Linear Models
================

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(p8105.datasets)
```

## Load and Clean the Airbnb Data

``` r
data("nyc_airbnb")

nyc_airbnb = 
  nyc_airbnb |> 
  mutate(stars = review_scores_location / 2) |> 
  select(
    price, stars, borough = neighbourhood_group,
    neighbourhood, room_type) |> 
  filter(borough != "Staten Island")
```

Let’s fit a model

``` r
fit = lm(price ~ stars + borough, data = nyc_airbnb)
```

Let’s look at the fit

``` r
fit
```

    ## 
    ## Call:
    ## lm(formula = price ~ stars + borough, data = nyc_airbnb)
    ## 
    ## Coefficients:
    ##      (Intercept)             stars   boroughBrooklyn  boroughManhattan  
    ##           -70.41             31.99             40.50             90.25  
    ##    boroughQueens  
    ##            13.21

``` r
summary(fit)
```

    ## 
    ## Call:
    ## lm(formula = price ~ stars + borough, data = nyc_airbnb)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -169.8  -64.0  -29.0   20.2 9870.0 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -70.414     14.021  -5.022 5.14e-07 ***
    ## stars              31.990      2.527  12.657  < 2e-16 ***
    ## boroughBrooklyn    40.500      8.559   4.732 2.23e-06 ***
    ## boroughManhattan   90.254      8.567  10.534  < 2e-16 ***
    ## boroughQueens      13.206      9.065   1.457    0.145    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 181.5 on 30525 degrees of freedom
    ##   (9962 observations deleted due to missingness)
    ## Multiple R-squared:  0.03423,    Adjusted R-squared:  0.03411 
    ## F-statistic: 270.5 on 4 and 30525 DF,  p-value: < 2.2e-16

``` r
summary(fit)$coef
```

    ##                   Estimate Std. Error   t value     Pr(>|t|)
    ## (Intercept)      -70.41446  14.020697 -5.022180 5.137589e-07
    ## stars             31.98989   2.527500 12.656733 1.269392e-36
    ## boroughBrooklyn   40.50030   8.558724  4.732049 2.232595e-06
    ## boroughManhattan  90.25393   8.567490 10.534465 6.638618e-26
    ## boroughQueens     13.20617   9.064879  1.456850 1.451682e-01

``` r
coef(fit)
```

    ##      (Intercept)            stars  boroughBrooklyn boroughManhattan 
    ##        -70.41446         31.98989         40.50030         90.25393 
    ##    boroughQueens 
    ##         13.20617

``` r
#fitted.values(fit)
```

Tidy up the output instead

``` r
fit |> 
  broom::glance()
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic   p.value    df   logLik    AIC    BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>     <dbl> <dbl>    <dbl>  <dbl>  <dbl>
    ## 1    0.0342        0.0341  182.      271. 6.73e-229     4 -202113. 4.04e5 4.04e5
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

Tidy up the coefficients

``` r
fit |> 
  broom::tidy() |> 
  mutate(term = str_replace(term, "^borough", "Borough: ")) |> 
  select(term, estimate, p.value) |> 
  knitr::kable(digits = 3)
```

| term               | estimate | p.value |
|:-------------------|---------:|--------:|
| (Intercept)        |  -70.414 |   0.000 |
| stars              |   31.990 |   0.000 |
| Borough: Brooklyn  |   40.500 |   0.000 |
| Borough: Manhattan |   90.254 |   0.000 |
| Borough: Queens    |   13.206 |   0.145 |

## Fit another model

``` r
fit = 
  nyc_airbnb |> 
  mutate(
    borough = fct_infreq(borough),
    room_type = fct_infreq(room_type)) |> 
  lm(price ~ stars + borough + room_type, data = _)

fit |> 
  broom::tidy()
```

    ## # A tibble: 7 × 5
    ##   term                  estimate std.error statistic  p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)              113.      11.8       9.54 1.56e-21
    ## 2 stars                     21.9      2.43      9.01 2.09e-19
    ## 3 boroughBrooklyn          -40.3      2.15    -18.8  4.62e-78
    ## 4 boroughQueens            -55.5      3.59    -15.4  1.32e-53
    ## 5 boroughBronx             -63.0      8.22     -7.67 1.76e-14
    ## 6 room_typePrivate room   -105.       2.05    -51.2  0       
    ## 7 room_typeShared room    -129.       6.15    -21.0  2.24e-97

## Quick look at diagnostics

``` r
nyc_airbnb |> 
  modelr::add_residuals(fit) |> 
  ggplot(aes(x = resid)) +
  geom_density() +
  xlim(-100, 500)
```

    ## Warning: Removed 11681 rows containing non-finite values (`stat_density()`).

![](Linear-Models_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
nyc_airbnb |> 
  modelr::add_predictions(fit)
```

    ## # A tibble: 40,492 × 6
    ##    price stars borough neighbourhood room_type        pred
    ##    <dbl> <dbl> <chr>   <chr>         <chr>           <dbl>
    ##  1    99   5   Bronx   City Island   Private room     54.0
    ##  2   200  NA   Bronx   City Island   Private room     NA  
    ##  3   300  NA   Bronx   City Island   Entire home/apt  NA  
    ##  4   125   5   Bronx   City Island   Entire home/apt 159. 
    ##  5    69   5   Bronx   City Island   Private room     54.0
    ##  6   125   5   Bronx   City Island   Entire home/apt 159. 
    ##  7    85   5   Bronx   City Island   Entire home/apt 159. 
    ##  8    39   4.5 Bronx   Allerton      Private room     43.1
    ##  9    95   5   Bronx   Allerton      Entire home/apt 159. 
    ## 10   125   4.5 Bronx   Allerton      Entire home/apt 148. 
    ## # ℹ 40,482 more rows

## Different variables for plot

``` r
nyc_airbnb |> 
  modelr::add_residuals(fit) |> 
  ggplot(aes(x = borough, y = resid)) + geom_violin()
```

    ## Warning: Removed 9962 rows containing non-finite values (`stat_ydensity()`).

![](Linear-Models_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
nyc_airbnb |> 
  modelr::add_residuals(fit) |> 
  ggplot(aes(x = stars, y = resid)) + geom_point()
```

    ## Warning: Removed 9962 rows containing missing values (`geom_point()`).

![](Linear-Models_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

## Hypothesis Testing for categorical predictor

fitting a “null” and “alternative” model

``` r
fit_null = lm(price ~ stars + borough, data = nyc_airbnb)
fit_alternative = lm(price ~ stars + borough + room_type, data = nyc_airbnb)


anova(fit_null, fit_alternative) |> 
  broom::tidy()
```

    ## # A tibble: 2 × 7
    ##   term                        df.residual    rss    df   sumsq statistic p.value
    ##   <chr>                             <dbl>  <dbl> <dbl>   <dbl>     <dbl>   <dbl>
    ## 1 price ~ stars + borough           30525 1.01e9    NA NA            NA       NA
    ## 2 price ~ stars + borough + …       30523 9.21e8     2  8.42e7     1394.       0

## Borough-level differences

``` r
fit = 
  nyc_airbnb |> 
  lm(price ~ stars * borough + room_type * borough, data = _) |>
  broom::tidy() |> 
  knitr::kable(digits = 3)
```

## Nesting within Boroughs

``` r
airbnb_lm = function(df) {
  lm(price ~ stars + room_type, data = df)
}

nyc_airbnb |> 
  nest(df = -borough) |> 
  mutate(
    models = map(df, airbnb_lm),
    results = map(models, broom::tidy)) |> 
  select(borough, results) |> 
  unnest(results) |> 
  select(borough, term, estimate) |> 
  pivot_wider(
    names_from = term, 
    values_from = estimate
    ) |> 
  knitr::kable(digits = 2)
```

| borough   | (Intercept) | stars | room_typePrivate room | room_typeShared room |
|:----------|------------:|------:|----------------------:|---------------------:|
| Bronx     |       90.07 |  4.45 |                -52.91 |               -70.55 |
| Queens    |       91.58 |  9.65 |                -69.26 |               -94.97 |
| Brooklyn  |       69.63 | 20.97 |                -92.22 |              -105.84 |
| Manhattan |       95.69 | 27.11 |               -124.19 |              -153.64 |

## Anonymous Function

``` r
nyc_airbnb |> 
  nest(df = -borough) |> 
  mutate(
    models = map(df, \(df) lm(price ~ stars + room_type, data = df)),
    results = map(models, broom::tidy)) |> 
  select(borough, results) |> 
  unnest(results) |> 
  select(borough, term, estimate) |> 
  pivot_wider(
    names_from = term, 
    values_from = estimate
    ) |> 
  knitr::kable(digits = 2)
```

| borough   | (Intercept) | stars | room_typePrivate room | room_typeShared room |
|:----------|------------:|------:|----------------------:|---------------------:|
| Bronx     |       90.07 |  4.45 |                -52.91 |               -70.55 |
| Queens    |       91.58 |  9.65 |                -69.26 |               -94.97 |
| Brooklyn  |       69.63 | 20.97 |                -92.22 |              -105.84 |
| Manhattan |       95.69 | 27.11 |               -124.19 |              -153.64 |

## Binary Outcomes

Looking at Homicides in Baltimore

``` r
baltimore_df = 
  read_csv("data/homicide-data.csv") |> 
  filter(city == "Baltimore") |> 
  mutate(
    resolved = as.numeric(disposition == "Closed by arrest"),
    victim_age = as.numeric(victim_age)) |> 
  select(resolved, victim_age, victim_race, victim_sex)
```

    ## Rows: 52179 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (9): uid, victim_last, victim_first, victim_race, victim_age, victim_sex...
    ## dbl (3): reported_date, lat, lon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Fitting a logistic regression

``` r
fit_logistic = 
  baltimore_df |> 
  glm(
    resolved ~ victim_age + victim_race + victim_sex, 
    data = _, 
    family = binomial()) 
```

Look at model results

``` r
fit_logistic |> 
  broom::tidy() |> 
  mutate(OR = exp(estimate)) |>
  select(term, log_OR = estimate, OR, p.value) |> 
  knitr::kable(digits = 3)
```

| term                | log_OR |    OR | p.value |
|:--------------------|-------:|------:|--------:|
| (Intercept)         |  1.486 | 4.421 |   0.024 |
| victim_age          | -0.007 | 0.993 |   0.027 |
| victim_raceBlack    | -1.138 | 0.320 |   0.076 |
| victim_raceHispanic | -0.562 | 0.570 |   0.418 |
| victim_raceOther    | -1.064 | 0.345 |   0.323 |
| victim_raceWhite    | -0.296 | 0.744 |   0.653 |
| victim_sexMale      | -0.880 | 0.415 |   0.000 |
