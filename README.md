
<!-- README.md is generated from README.Rmd. Please edit that file -->

# genuary

More information on [genuary](https://genuary2021.github.io/).

## Prompt 05 - code golf

``` r
library(tidyverse)
df <- expand_grid(x=1:50,y=1:50)
df$type <- sample(c("l","r"),nrow(df),replace=TRUE)
mutate(df,xend=ifelse(type=="r",x+1,x),yend=y+1,x=ifelse(type=="l",x+1,x)) %>% 
  ggplot(aes(x,y,xend=xend,yend=yend))+geom_segment()+theme_void()+
  ggtitle("10 PRINT CHR$(205.5+RND(1)); : GOTO 10")
```

<img src="img/05_golf.png" width="500px">

## Prompt 08 - curves

[Lissajou curves](https://en.wikipedia.org/wiki/Lissajous_curve)

<img src="img/08_curves.gif">

Final frame

<img src="img/08_curves.png" width="500px">

## Prompt 10 - tree

trees created with [Collatz
sequences](https://en.wikipedia.org/wiki/Collatz_conjecture)

<img src="img/10_trees.png" width="500px">

animated

<img src="img/10_tree.gif">

## Prompt 12 - api

<img src="img/12_api.gif">

## Prompt 13 - no repeat

Ulam Spiral with prime factor decomposition and unique colours for prime
numbers (1-1023)

<img src="img/13_no-repeat.png" width="500px">

animated

<img src="img/13_no-repeat.gif">

## Prompt 14 - subdivision

some random Mondrian art

<img src="img/14_subdivision.png" width="500px">
