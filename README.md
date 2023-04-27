Breast Cancer Wisconsin (Diagnostic) Data Set
================

[Kaggle
Link](https://www.kaggle.com/datasets/uciml/breast-cancer-wisconsin-data)

# **IN PROGRESS**

------------------------------------------------------------------------

### Contents

- [Setup](#setup)
- [Data Import](#data-import)
- [Checking for Missing Data](#checking-for-missing-data)
- [Exploratory Data Analysis](#exploratory-data-analysis)
- [Modeling](#modeling)

### Setup

``` r
tictoc::tic()

library(tidyverse)
library(tidymodels)
library(tvthemes)
library(janitor)
library(patchwork)
library(ggcorrplot)

theme_custom = theme_avatar() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 9, vjust = 2.5, face = "italic"),
        panel.grid.major = element_line(linewidth = 0.5, colour = "#D6D0C4"),
        panel.grid.minor = element_line(linewidth = 0.5, colour = "#D6D0C4"))

theme_set(theme_custom)
custom_fills = scale_fill_manual(values = c("springgreen4", "indianred3"))
```

### Data Import

<details>
<summary>
View Code
</summary>

``` r
df = clean_names(read_csv("breast_cancer_data.csv", col_types = cols()))
paste0("Data Dimensions: ", nrow(df), " Rows, ", ncol(df), " Columns")
```

</details>

    ## [1] "Data Dimensions: 569 Rows, 32 Columns"

### Checking for Missing Data

<details>
<summary>
View Code
</summary>

``` r
colSums(is.na(df))
```

</details>

    ##                      id               diagnosis             radius_mean 
    ##                       0                       0                       0 
    ##            texture_mean          perimeter_mean               area_mean 
    ##                       0                       0                       0 
    ##         smoothness_mean        compactness_mean          concavity_mean 
    ##                       0                       0                       0 
    ##     concave_points_mean           symmetry_mean  fractal_dimension_mean 
    ##                       0                       0                       0 
    ##               radius_se              texture_se            perimeter_se 
    ##                       0                       0                       0 
    ##                 area_se           smoothness_se          compactness_se 
    ##                       0                       0                       0 
    ##            concavity_se       concave_points_se             symmetry_se 
    ##                       0                       0                       0 
    ##    fractal_dimension_se            radius_worst           texture_worst 
    ##                       0                       0                       0 
    ##         perimeter_worst              area_worst        smoothness_worst 
    ##                       0                       0                       0 
    ##       compactness_worst         concavity_worst    concave_points_worst 
    ##                       0                       0                       0 
    ##          symmetry_worst fractal_dimension_worst 
    ##                       0                       0

None :)

### Exploratory Data Analysis

------------------------------------------------------------------------

### Overview of *diagnosis*

<details>
<summary>
View Code
</summary>

``` r
# renaming `diagnosis` labels
df2 = df |>
  mutate(diagnosis = ifelse(diagnosis == "M", "Malignant", "Benign"))

df2 |>
  count(diagnosis) |>
  ggplot(aes(diagnosis, n)) +
  geom_col(aes(fill = diagnosis), show.legend = F) +
  geom_text(aes(label = n), size = 3.5, vjust = -0.5) +
  scale_fill_manual(values = c("springgreen4", "indianred3")) +
  labs(x = "Diagnosis", y = "Count", title = "Diagnosis Counts") +
  theme(axis.text.y = element_blank())
```

</details>

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### Overview of *radius_mean*

<details>
<summary>
View Code
</summary>

``` r
sub_5num = function(x) {
  sub = fivenum(x)
  paste0("Min: ", sub[1], " | Q1: ", sub[2], " | Median: ", sub[3], " | Q3: ", sub[4], " | Max: ", sub[5])
}

df2 |>
  ggplot(aes(radius_mean)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Radius Mean", y = "Density",
       title = "Distribution of Radius Mean by Diagnosis", fill = NULL,
       subtitle = sub_5num(df2$radius_mean))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Overview of *texture_mean*

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  ggplot(aes(texture_mean)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Texture Mean", y = "Density",
       title = "Distribution of Texture Mean by Diagnosis", fill = "Diagnosis",
       subtitle = sub_5num(df2$texture_mean))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

### Overview of *perimeter_mean*

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  ggplot(aes(perimeter_mean)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Perimeter Mean", y = "Density", fill = "Diagnosis",
       title = "Distribution of Perimeter Mean by Diagnosis",
       subtitle = sub_5num(df2$perimeter_mean))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

### Overview of *area_mean*

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  ggplot(aes(area_mean)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Area Mean", y = "Density", fill = "Diagnosis",
       title = "Distribution of Area Mean by Diagnosis",
       subtitle = sub_5num(df2$area_mean))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

### Overview of *smoothness_mean*

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  ggplot(aes(smoothness_mean)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Smoothness Mean", y = "Density", fill = "Diagnosis",
       title = "Distribution of Smoothness Mean by Diagnosis",
       subtitle = sub_5num(df2$smoothness_mean))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

### Overview of *compactness_mean*

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  ggplot(aes(compactness_mean)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Compactness Mean", y = "Density", fill = "Diagnosis",
       title = "Distribution of Compactness Mean by Diagnosis",
       subtitle = sub_5num(df2$compactness_mean))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

### Overview of *concavity_mean*

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  ggplot(aes(concavity_mean)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Concavity Mean", y = "Density", fill = "Diagnosis",
       title = "Distribution of Concavity Mean by Diagnosis",
       subtitle = sub_5num(df2$concavity_mean))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

### Overview of *concave_points_mean*

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  ggplot(aes(concave_points_mean)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Concave Points Mean", y = "Density", fill = "Diagnosis",
       title = "Distribution of Concave Points Mean by Diagnosis",
       subtitle = sub_5num(df2$concave_points_mean))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

### Overview of *symmetry_mean*

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  ggplot(aes(symmetry_mean)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Symmetry Mean", y = "Density", fill = "Diagnosis",
       title = "Distribution of Symmetry Mean by Diagnosis",
       subtitle = sub_5num(df2$symmetry_mean))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

### Overview of *fractal_dimension_mean*

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  ggplot(aes(fractal_dimension_mean)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Fractal Dimension Mean", y = "Density", fill = "Diagnosis",
       title = "Distribution of Fractal Dimension Mean by Diagnosis",
       subtitle = sub_5num(df2$fractal_dimension_mean))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

### Overview of *radius_se*

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  ggplot(aes(radius_se)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Radius Standard Error", y = "Density", fill = "Diagnosis",
       title = "Distribution of Radius Standard Error by Diagnosis",
       subtitle = sub_5num(df2$radius_se))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

### Overview of *texture_se*

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  ggplot(aes(texture_se)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Texture Standard Error", y = "Density", fill = "Diagnosis",
       title = "Distribution of Texture Standard Error by Diagnosis",
       subtitle = sub_5num(df2$texture_se))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

### Overview of *perimeter_se*

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  ggplot(aes(perimeter_se)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Perimeter Standard Error", y = "Density", fill = "Diagnosis",
       title = "Distribution of Perimeter Standard Error by Diagnosis",
       subtitle = sub_5num(df2$perimeter_se))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

### Overview of *area_se*

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  ggplot(aes(area_se)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Area Standard Error", y = "Density", fill = "Diagnosis",
       title = "Distribution of Area Standard Error by Diagnosis",
       subtitle = sub_5num(df2$area_se))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

### Overview of *smoothness_se*

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  ggplot(aes(smoothness_se)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Smoothness Standard Error", y = "Density", fill = "Diagnosis",
       title = "Distribution of Smoothness Standard Error by Diagnosis",
       subtitle = sub_5num(df2$smoothness_se))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

### Overview of *compactness_se*

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  ggplot(aes(compactness_se)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Compactness Standard Error", y = "Density", fill = "Diagnosis",
       title = "Distribution of Compactness Standard Error by Diagnosis",
       subtitle = sub_5num(df2$compactness_se))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

### Overview of *concavity_se*

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  ggplot(aes(concavity_se)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Concavity Standard Error", y = "Density", fill = "Diagnosis",
       title = "Distribution of Concavity Standard Error by Diagnosis",
       subtitle = sub_5num(df2$concavity_se))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

### Overview of *concave_points_se*

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  ggplot(aes(concave_points_se)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Concave Points Standard Error", y = "Density", fill = "Diagnosis",
       title = "Distribution of Concave Points Standard Error by Diagnosis",
       subtitle = sub_5num(df2$concave_points_se))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

### Overview of *symmetry_se*

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  ggplot(aes(symmetry_se)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Symmetry Standard Error", y = "Density", fill = "Diagnosis",
       title = "Distribution of Symmetry Standard Error by Diagnosis",
       subtitle = sub_5num(df2$symmetry_se))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

### Overview of *fractal_dimension_se*

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  ggplot(aes(fractal_dimension_se)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Fractal Dimension Standard Error", y = "Density", fill = "Diagnosis",
       title = "Distribution of Fractal Dimension Standard Error by Diagnosis",
       subtitle = sub_5num(df2$fractal_dimension_se))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

### Overview of *radius_worst*

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  ggplot(aes(radius_worst)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Radius Worst", y = "Density", fill = "Diagnosis",
       title = "Distribution of Radius Worst by Diagnosis",
       subtitle = sub_5num(df2$radius_worst))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

### Overview of *texture_worst*

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  ggplot(aes(texture_worst)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Texture Worst", y = "Density", fill = "Diagnosis",
       title = "Distribution of Texture Worst by Diagnosis",
       subtitle = sub_5num(df2$texture_worst))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

### Overview of *perimeter_worst*

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  ggplot(aes(perimeter_worst)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Perimeter Worst", y = "Density", fill = "Diagnosis",
       title = "Distribution of Perimeter Worst by Diagnosis",
       subtitle = sub_5num(df2$perimeter_worst))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

### Overview of *area_worst*

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  ggplot(aes(area_worst)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Area Worst", y = "Density", fill = "Diagnosis",
       title = "Distribution of Area Worst by Diagnosis",
       subtitle = sub_5num(df2$area_worst))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

### Overview of *smoothness_worst*

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  ggplot(aes(smoothness_worst)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Smoothness Worst", y = "Density", fill = "Diagnosis",
       title = "Distribution of Smoothness Worst by Diagnosis",
       subtitle = sub_5num(df2$smoothness_worst))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

### Overview of *compactness_worst*

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  ggplot(aes(compactness_worst)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Compactness Worst", y = "Density", fill = "Diagnosis",
       title = "Distribution of Compactness Worst by Diagnosis",
       subtitle = sub_5num(df2$compactness_worst))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

### Overview of *concavity_worst*

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  ggplot(aes(concavity_worst)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Concavity Worst", y = "Density", fill = "Diagnosis",
       title = "Distribution of Concavity Worst by Diagnosis",
       subtitle = sub_5num(df2$concavity_worst))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

### Overview of *concave_points_worst*

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  ggplot(aes(concave_points_worst)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Concave Points Worst", y = "Density", fill = "Diagnosis",
       title = "Distribution of Concave Points Worst by Diagnosis",
       subtitle = sub_5num(df2$concave_points_worst))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

### Overview of *symmetry_worst*

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  ggplot(aes(symmetry_worst)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Symmetry Worst", y = "Density", fill = "Diagnosis",
       title = "Distribution of Symmetry Worst by Diagnosis",
       subtitle = sub_5num(df2$symmetry_worst))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

### Overview of *fractal_dimension_worst*

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  ggplot(aes(fractal_dimension_worst)) +
  geom_density(aes(fill = diagnosis), alpha = 0.5, col = "transparent") +
  custom_fills +
  labs(x = "Fractal Dimension Worst", y = "Density", fill = "Diagnosis",
       title = "Distribution of Fractal Dimension Worst by Diagnosis",
       subtitle = sub_5num(df2$fractal_dimension_worst))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

### Correlation Plot for All *mean* Variables

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  select(contains("mean")) |>
  cor() |>
  ggcorrplot(lab = T, type = "lower", lab_size = 3,
             colors = c("indianred3", "white", "springgreen4"))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

### Correlation Plot for All *se* Variables

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  select(contains("se")) |>
  cor() |>
  ggcorrplot(lab = T, type = "lower", lab_size = 3,
             colors = c("indianred3", "white", "springgreen4"))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

### Correlation Plot for All *worst* Variables

<details>
<summary>
View Code
</summary>

``` r
df2 |>
  select(contains("worst")) |>
  cor() |>
  ggcorrplot(lab = T, type = "lower", lab_size = 3,
             colors = c("indianred3", "white", "springgreen4"))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

This will conclude the exploratory portion of the analysis, and I will
now move onto the modeling portion.

### Modeling

------------------------------------------------------------------------

### Data Splitting

<details>
<summary>
View Code
</summary>

``` r
data_split = initial_split(df2, prop = 0.8, strata = diagnosis)
train_data = training(data_split)
test_data = testing(data_split)
paste0("Training Data: ", nrow(train_data), " Observations; Testing Data: ", nrow(test_data), " Observations")
```

</details>

    ## [1] "Training Data: 454 Observations; Testing Data: 115 Observations"

### Examining Distribution of *diagnosis* Across Data Splits

<details>
<summary>
View Code
</summary>

``` r
train_data |>
  count(diagnosis) |>
  mutate(set = "Training Data",
         pct = round(n / sum(n), 3),
         pct_lab = paste0(pct * 100, "%")) |>
  bind_rows(test_data |>
              count(diagnosis) |>
              mutate(set = "Testing Data",
                     pct = round(n / sum(n), 3),
                     pct_lab = paste0(pct * 100, "%"))) |>
  mutate(set = factor(set, levels = c("Training Data", "Testing Data"))) |>
  ggplot(aes(set, pct)) +
  geom_col(aes(fill = diagnosis), position = "dodge") +
  geom_text(aes(label = pct_lab), position = position_dodge2(width = 1), vjust = -0.5, size = 3.5) +
  coord_cartesian(ylim = c(0, 0.65)) +
  scale_fill_manual(values = c("springgreen4", "indianred3")) +
  labs(x = "Dataset", y = "Percent of Observations", fill = NULL,
       title = "Proportional Diagnoses Across Training and Testing Data") +
  theme(legend.position = "right")
```

</details>

![](README_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

### Data Preprocessing

<details>
<summary>
View Code
</summary>

``` r
pre_rec = recipe(diagnosis ~ ., data = df2) |>
  update_role(id, new_role = "ID") |>
  # normalizes predictors
  step_normalize(all_predictors()) |>
  # removes predictors with zero variance
  step_zv(all_predictors()) |>
  # removes highly-correlated predictors, we can control the threshold at which they are removed
  step_corr(all_predictors(), threshold = 0.8, method = "spearman")

pre_rec
```

</details>

    ## 

    ## ── Recipe ──────────────────────────────────────────────────────────────────────

    ## 

    ## ── Inputs

    ## Number of variables by role

    ## outcome:    1
    ## predictor: 30
    ## ID:         1

    ## 

    ## ── Operations

    ## • Centering and scaling for: all_predictors()

    ## • Zero variance filter on: all_predictors()

    ## • Correlation filter on: all_predictors()

### Previewing Preprocessed Data

<details>
<summary>
View Code
</summary>

``` r
pre_rec |>
  prep() |>
  juice() |>
  sample_n(10)
```

</details>

    ## # A tibble: 10 × 18
    ##           id texture_…¹ area_…² smooth…³ symme…⁴ fract…⁵ radiu…⁶ textu…⁷ smoot…⁸
    ##        <dbl>      <dbl>   <dbl>    <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1   8911834     -0.956  -0.192 -0.0853    1.09  -0.604  -0.567  -0.539   -0.935
    ##  2   8810158      0.756  -0.357  0.273     0.140  1.46   -0.765  -0.534   -0.596
    ##  3  84667401      0.772  -0.218  1.19      0.939  1.99   -0.696  -0.0867  -0.204
    ##  4    884689     -1.01   -0.706  0.351     0.260 -0.158  -0.537  -0.324   -0.126
    ##  5   9113514     -0.277  -1.05  -0.942    -0.480  0.187  -0.229   0.172    1.31 
    ##  6   8812816     -1.43   -0.244  0.00709  -1.65   0.0910 -0.703  -1.42    -0.969
    ##  7 911320502     -0.249  -0.334 -1.54     -1.30  -1.04   -0.732  -0.964   -0.357
    ##  8    895299     -0.316  -0.594 -1.39     -1.57  -0.297  -0.619  -0.557   -0.770
    ##  9    921092      1.44   -1.35  -1.09      0.213  1.42   -0.0991  0.444    1.87 
    ## 10   8810436     -1.48    0.201 -1.03     -1.65  -1.07   -0.692  -1.55    -0.916
    ## # … with 9 more variables: compactness_se <dbl>, concave_points_se <dbl>,
    ## #   symmetry_se <dbl>, fractal_dimension_se <dbl>, smoothness_worst <dbl>,
    ## #   compactness_worst <dbl>, symmetry_worst <dbl>,
    ## #   fractal_dimension_worst <dbl>, diagnosis <fct>, and abbreviated variable
    ## #   names ¹​texture_mean, ²​area_mean, ³​smoothness_mean, ⁴​symmetry_mean,
    ## #   ⁵​fractal_dimension_mean, ⁶​radius_se, ⁷​texture_se, ⁸​smoothness_se

### Building Cross Validation Folds

<details>
<summary>
View Code
</summary>

``` r
cv_folds = vfold_cv(train_data, v = 5, strata = diagnosis)
cv_folds
```

</details>

    ## #  5-fold cross-validation using stratification 
    ## # A tibble: 5 × 2
    ##   splits           id   
    ##   <list>           <chr>
    ## 1 <split [363/91]> Fold1
    ## 2 <split [363/91]> Fold2
    ## 3 <split [363/91]> Fold3
    ## 4 <split [363/91]> Fold4
    ## 5 <split [364/90]> Fold5

### Building Logistic Regression Specification and Workflow

<details>
<summary>
View Code
</summary>

``` r
log_spec = logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification")

log_wflow = workflow() |>
  add_recipe(pre_rec) |>
  add_model(log_spec)

log_wflow
```

</details>

    ## ══ Workflow ════════════════════════════════════════════════════════════════════
    ## Preprocessor: Recipe
    ## Model: logistic_reg()
    ## 
    ## ── Preprocessor ────────────────────────────────────────────────────────────────
    ## 3 Recipe Steps
    ## 
    ## • step_normalize()
    ## • step_zv()
    ## • step_corr()
    ## 
    ## ── Model ───────────────────────────────────────────────────────────────────────
    ## Logistic Regression Model Specification (classification)
    ## 
    ## Computational engine: glm

### Getting Logistic Regression Results

<details>
<summary>
View Code
</summary>

``` r
log_res = log_wflow |>
  fit_resamples(resamples = cv_folds,
                metrics = metric_set(accuracy, f_meas, roc_auc),
                control = control_resamples(save_pred = T))

log_res |>
  collect_metrics() |>
  select(.metric, mean, std_err)
```

</details>

    ## # A tibble: 3 × 3
    ##   .metric   mean std_err
    ##   <chr>    <dbl>   <dbl>
    ## 1 accuracy 0.956 0.00597
    ## 2 f_meas   0.965 0.00488
    ## 3 roc_auc  0.990 0.00372

### Building Random Forest Specification and Workflow

<details>
<summary>
View Code
</summary>

``` r
rf_spec = rand_forest() |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")

rf_wflow = workflow() |>
  add_recipe(pre_rec) |>
  add_model(rf_spec)

rf_wflow
```

</details>

    ## ══ Workflow ════════════════════════════════════════════════════════════════════
    ## Preprocessor: Recipe
    ## Model: rand_forest()
    ## 
    ## ── Preprocessor ────────────────────────────────────────────────────────────────
    ## 3 Recipe Steps
    ## 
    ## • step_normalize()
    ## • step_zv()
    ## • step_corr()
    ## 
    ## ── Model ───────────────────────────────────────────────────────────────────────
    ## Random Forest Model Specification (classification)
    ## 
    ## Engine-Specific Arguments:
    ##   importance = impurity
    ## 
    ## Computational engine: ranger

### Getting Random Forest Results

<details>
<summary>
View Code
</summary>

``` r
rf_res = rf_wflow |>
  fit_resamples(resamples = cv_folds,
                metrics = metric_set(accuracy, f_meas, roc_auc),
                control = control_resamples(save_pred = T))

rf_res |>
  collect_metrics() |>
  select(.metric, mean, std_err)
```

</details>

    ## # A tibble: 3 × 3
    ##   .metric   mean std_err
    ##   <chr>    <dbl>   <dbl>
    ## 1 accuracy 0.938 0.0145 
    ## 2 f_meas   0.952 0.0109 
    ## 3 roc_auc  0.988 0.00454

### Building XGBoost Specification and Workflow

<details>
<summary>
View Code
</summary>

``` r
xgb_spec = boost_tree() |>
  set_engine("xgboost") |>
  set_mode("classification")

xgb_wflow = workflow() |>
  add_recipe(pre_rec) |>
  add_model(xgb_spec)

xgb_wflow
```

</details>

    ## ══ Workflow ════════════════════════════════════════════════════════════════════
    ## Preprocessor: Recipe
    ## Model: boost_tree()
    ## 
    ## ── Preprocessor ────────────────────────────────────────────────────────────────
    ## 3 Recipe Steps
    ## 
    ## • step_normalize()
    ## • step_zv()
    ## • step_corr()
    ## 
    ## ── Model ───────────────────────────────────────────────────────────────────────
    ## Boosted Tree Model Specification (classification)
    ## 
    ## Computational engine: xgboost

### Getting XGBoost Results

<details>
<summary>
View Code
</summary>

``` r
xgb_res = xgb_wflow |>
  fit_resamples(resamples = cv_folds,
                metrics = metric_set(accuracy, f_meas, roc_auc),
                control = control_resamples(save_pred = T))

xgb_res |>
  collect_metrics() |>
  select(.metric, mean, std_err)
```

</details>

    ## # A tibble: 3 × 3
    ##   .metric   mean std_err
    ##   <chr>    <dbl>   <dbl>
    ## 1 accuracy 0.947 0.0145 
    ## 2 f_meas   0.958 0.0111 
    ## 3 roc_auc  0.987 0.00586

### Comparing Model Results

<details>
<summary>
View Code
</summary>

``` r
three_mod_res = log_res |>
  collect_metrics() |>
  select(.metric, mean, std_err) |>
  mutate(model = "Logistic Regression") |>
  bind_rows(rf_res |>
              collect_metrics() |>
              select(.metric, mean, std_err) |>
              mutate(model = "Random Forest")) |>
  bind_rows(xgb_res |>
              collect_metrics() |>
              select(.metric, mean, std_err) |>
              mutate(model = "XGBoost"))

lr_res = three_mod_res |>
  filter(model == "Logistic Regression") |>
  mutate(mean = paste0(round(mean, 4) * 100, "%"))

lr_acc = lr_res$mean[1]
lr_f1 = lr_res$mean[2]
lr_roc = lr_res$mean[3]

three_mod_res |>
  ggplot(aes(.metric, mean)) +
  geom_col(aes(fill = model), position = "dodge") +
  geom_text(aes(label = round(mean, 4)), size = 3.5, position = position_dodge2(width = 0.9), vjust = -0.5) +
  coord_cartesian(ylim = c(0.9, 1)) +
  labs(x = "Evaluation Metric", y = "Value", fill = NULL,
       title = "Evaluation Metrics Across Models")
```

</details>

![](README_files/figure-gfm/unnamed-chunk-50-1.png)<!-- -->

After this initial modeling step, we have achieved an accuracy of 95.6%,
an F1 score of 96.49%, and an AUC value of 98.96% with a logistic
regression model. We have achieved similar results with our random
forest and XGBoost models as well, and will now tune the hyperparameters
for those models to see if we can improve upon our current results.

### Random Forest Tuning

<details>
<summary>
View Code
</summary>

``` r
tune_spec = rand_forest(trees = 1000, mtry = tune(), min_n = tune()) |>
  set_mode("classification") |>
  set_engine("ranger")

tune_wf = workflow() |>
  add_recipe(pre_rec) |>
  add_model(tune_spec)

doParallel::registerDoParallel()
tune_res = tune_grid(tune_wf, resamples = cv_folds, grid = 25)
```

</details>

    ## i Creating pre-processing data to finalize unknown parameter: mtry

<details>
<summary>
View Code
</summary>

``` r
tune_res |>
  show_best("accuracy")
```

</details>

    ## # A tibble: 5 × 8
    ##    mtry min_n .metric  .estimator  mean     n std_err .config              
    ##   <int> <int> <chr>    <chr>      <dbl> <int>   <dbl> <chr>                
    ## 1     2    11 accuracy binary     0.945     5  0.0125 Preprocessor1_Model20
    ## 2     2    18 accuracy binary     0.941     5  0.0141 Preprocessor1_Model04
    ## 3     9     5 accuracy binary     0.941     5  0.0169 Preprocessor1_Model06
    ## 4    14     8 accuracy binary     0.938     5  0.0169 Preprocessor1_Model16
    ## 5     5    16 accuracy binary     0.938     5  0.0189 Preprocessor1_Model18

### XGBoost Tuning

<details>
<summary>
View Code
</summary>

``` r
xgb_spec = boost_tree(trees = 1000, tree_depth = tune(), min_n = tune(),
                      loss_reduction = tune(), sample_size = tune(),
                      mtry = tune(), learn_rate = tune()) |>
  set_engine("xgboost") |>
  set_mode("classification")

xgb_grid = grid_latin_hypercube(tree_depth(), min_n(), loss_reduction(),
                                sample_size = sample_prop(), finalize(mtry(), train_data),
                                learn_rate(), size = 30)

xgb_wf = workflow() |>
  add_recipe(pre_rec) |>
  add_model(xgb_spec)

doParallel::registerDoParallel()
xgb_res = tune_grid(xgb_wf, resamples = cv_folds, grid = xgb_grid, control = control_grid(save_pred = T))

xgb_res |>
  show_best("roc_auc")
```

</details>

    ## # A tibble: 5 × 12
    ##    mtry min_n tree_depth learn_rate loss_r…¹ sampl…² .metric .esti…³  mean     n
    ##   <int> <int>      <int>      <dbl>    <dbl>   <dbl> <chr>   <chr>   <dbl> <int>
    ## 1    32    12         14 0.00542     2.17e-9   0.810 roc_auc binary  0.983     5
    ## 2     7     2          6 0.00000995  1.61e-6   0.829 roc_auc binary  0.978     5
    ## 3     2    14         11 0.00122     5.90e-3   0.636 roc_auc binary  0.975     5
    ## 4    14    22         12 0.0176      4.34e-4   0.891 roc_auc binary  0.973     5
    ## 5     2     6         10 0.00000320  2.72e-7   0.313 roc_auc binary  0.972     5
    ## # … with 2 more variables: std_err <dbl>, .config <chr>, and abbreviated
    ## #   variable names ¹​loss_reduction, ²​sample_size, ³​.estimator

### Post-Tuning Model Comparison

<details>
<summary>
View Code
</summary>

``` r
lr_acc = collect_metrics(log_res) |>
  filter(.metric == "accuracy") |>
  pull(mean) |>
  round(4)

lr_auc = collect_metrics(log_res) |>
  filter(.metric == "roc_auc") |>
  pull(mean) |>
  round(4)

rf_acc = show_best(tune_res, metric = "accuracy", n = 1) |>
  pull(mean) |>
  round(4)

rf_auc = show_best(tune_res, metric = "roc_auc", n = 1) |>
  pull(mean) |>
  round(4)

xgb_acc = show_best(xgb_res, metric = "accuracy", n = 1) |>
  pull(mean) |>
  round(4)

xgb_auc = show_best(xgb_res, metric = "roc_auc", n = 1) |>
  pull(mean) |>
  round(4)

data.frame(model = c("Logistic Regression", "Random Forest", "XGBoost"),
           accuracy = c(lr_acc, rf_acc, xgb_acc),
           roc_auc = c(lr_auc, rf_auc, xgb_auc)) |>
  pivot_longer(!model, names_to = "metric", values_to = "value") |>
  ggplot(aes(metric, value)) +
  geom_col(aes(fill = model), position = "dodge") +
  geom_text(aes(label = value), position = position_dodge2(width = 0.9), vjust = -0.5, size = 3.5) +
  labs(title = "Post-Tuning Model Comparison", fill = NULL, x = "Evaluation Metric", y = "Value")
```

</details>

![](README_files/figure-gfm/unnamed-chunk-53-1.png)<!-- -->

### Script Runtime

``` r
tictoc::toc()
```

    ## 140.57 sec elapsed
