hw03-gapminder
================

Including packages:
-------------------

``` r
library(gapminder)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.0.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.6
    ## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

Get the maximum and minimum of GDP per capita for all continents:
-----------------------------------------------------------------

``` r
gdpByCont <- gapminder %>% 
  group_by(continent) %>% 
  summarize(gdpPercap_min = min(gdpPercap), gdpPercap_max = max(gdpPercap))
gdpByCont
```

    ## # A tibble: 5 x 3
    ##   continent gdpPercap_min gdpPercap_max
    ##   <fct>             <dbl>         <dbl>
    ## 1 Africa             241.        21951.
    ## 2 Americas          1202.        42952.
    ## 3 Asia               331        113523.
    ## 4 Europe             974.        49357.
    ## 5 Oceania          10040.        34435.

And then make a box plot to show the data above:

``` r
gdpByCont %>% 
  # gather the max and min data in one "value"
  gather(variable,value,-continent) %>% 
  ggplot(aes(x=continent, y=value, fill = variable)) + 
  # make the max and min value adopt "dodge" positions in one bar plot
  geom_col(position = position_dodge())
```

![](hw03-gapminder_files/figure-markdown_github/unnamed-chunk-3-1.png) \#\# Compute a trimmed mean of life expectancy for different years. Or a weighted mean, weighting by population. Just try something other than the plain vanilla mean.

``` r
wtMean <- gapminder %>% 
  group_by(continent,year) %>% 
  summarise(lifeExp=weighted.mean(lifeExp,pop))
wtMean
```

    ## # A tibble: 60 x 3
    ## # Groups:   continent [?]
    ##    continent  year lifeExp
    ##    <fct>     <int>   <dbl>
    ##  1 Africa     1952    38.8
    ##  2 Africa     1957    40.9
    ##  3 Africa     1962    43.1
    ##  4 Africa     1967    45.2
    ##  5 Africa     1972    47.2
    ##  6 Africa     1977    49.2
    ##  7 Africa     1982    51.0
    ##  8 Africa     1987    52.8
    ##  9 Africa     1992    53.4
    ## 10 Africa     1997    53.3
    ## # ... with 50 more rows

``` r
ggplot(wtMean, aes(year,lifeExp)) +
  geom_smooth(aes(colour=continent))
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](hw03-gapminder_files/figure-markdown_github/unnamed-chunk-5-1.png)

How is life expectancy changing over time on different continents?
------------------------------------------------------------------

``` r
gapminder %>% 
  group_by(continent,year) %>% 
  summarise(lifeExp=mean(lifeExp))
```

    ## # A tibble: 60 x 3
    ## # Groups:   continent [?]
    ##    continent  year lifeExp
    ##    <fct>     <int>   <dbl>
    ##  1 Africa     1952    39.1
    ##  2 Africa     1957    41.3
    ##  3 Africa     1962    43.3
    ##  4 Africa     1967    45.3
    ##  5 Africa     1972    47.5
    ##  6 Africa     1977    49.6
    ##  7 Africa     1982    51.6
    ##  8 Africa     1987    53.3
    ##  9 Africa     1992    53.6
    ## 10 Africa     1997    53.6
    ## # ... with 50 more rows

``` r
ggplot(gapminder, aes(year,lifeExp)) +
  geom_point(aes(colour=continent),alpha=0.1) +
  geom_smooth(aes(colour=continent))
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](hw03-gapminder_files/figure-markdown_github/unnamed-chunk-7-1.png)
