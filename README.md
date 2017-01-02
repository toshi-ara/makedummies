DESCRIPTION
===========

A function to make dummy variables for R.

Usage
-----

``` {.r}
makedummies(dat, basal_level = FALSE, col = NULL, numerical = NULL, as.is = NULL)
```

-   dat: data.frame
-   basal_level
    -   TRUE: include a dummy variable for base group
    -   FALSE (default): exclude a dummy variable for base group
-   col: Columns vector (all columns are used if NULL is given)
-   numerical: Columns vector converting from factor/ordered to numeric
    (ignore if column is numeric)
-   as.is: Columns vector not converting (all columns are used if NULL
    is given)
-   sep: obsoluted

Examples
========

Simple usages
-------------

### factor

``` {.r}
dat <- data.frame(x = factor(rep(c("a", "b", "c"), each = 3)))
dat$x
makedummies(dat)
```

``` {.example}
[1] a a a b b b c c c
Levels: a b c

  x_b x_c
1   0   0
2   0   0
3   0   0
4   1   0
5   1   0
6   1   0
7   0   1
8   0   1
9   0   1
```

``` {.r}
makedummies(dat, basal_level = TRUE)
```

``` {.example}
  x_a x_b x_c
1   1   0   0
2   1   0   0
3   1   0   0
4   0   1   0
5   0   1   0
6   0   1   0
7   0   0   1
8   0   0   1
9   0   0   1
```

### ordered

``` {.r}
dat <- data.frame(x = factor(rep(c("a", "b", "c"), each = 3)))
dat$x <- ordered(dat$x, levels = c("a" ,"c" ,"b"))
dat$x
makedummies(dat)
```

``` {.example}
[1] a a a b b b c c c
Levels: a < c < b

  x_c x_b
1   0   0
2   0   0
3   0   0
4   0   1
5   0   1
6   0   1
7   1   0
8   1   0
9   1   0
```

### numeric

``` {.r}
dat <- data.frame(x = rep(1:3, each = 3))
makedummies(dat)
```

``` {.example}
  x
1 1
2 1
3 1
4 2
5 2
6 2
7 3
8 3
9 3
```

### factor and numeric

``` {.r}
dat <- data.frame(
    x = factor(rep(c("a", "b", "c"), each = 3)),
    y = rep(1:3, each = 3)
)
makedummies(dat)
```

``` {.example}
  x_b x_c y
1   0   0 1
2   0   0 1
3   0   0 1
4   1   0 2
5   1   0 2
6   1   0 2
7   0   1 3
8   0   1 3
9   0   1 3
```

### factors

``` {.r}
dat <- data.frame(
    x = factor(rep(c("a", "b", "c"), each = 3)),
    y = factor(rep(1:3, each = 3))
)
makedummies(dat)
```

``` {.example}
  x_b x_c y_2 y_3
1   0   0   0   0
2   0   0   0   0
3   0   0   0   0
4   1   0   1   0
5   1   0   1   0
6   1   0   1   0
7   0   1   0   1
8   0   1   0   1
9   0   1   0   1
```

Options
-------

### "col" option

``` {.r}
dat <- data.frame(
    x = factor(rep(c("a", "b", "c"), each = 3)),
    y = factor(rep(1:3, each = 3))
v)
makedummies(dat, col = "x")
```

``` {.example}
  x_b x_c
1   0   0
2   0   0
3   0   0
4   1   0
5   1   0
6   1   0
7   0   1
8   0   1
9   0   1
```

### "numerical" option

``` {.r}
dat <- data.frame(
    x = factor(rep(c("a", "b", "c"), each = 3)),
    y = factor(rep(1:3, each = 3))
)
makedummies(dat, numerical = "x")
```

``` {.example}
  x y_2 y_3
1 1   0   0
2 1   0   0
3 1   0   0
4 2   1   0
5 2   1   0
6 2   1   0
7 3   0   1
8 3   0   1
9 3   0   1
```

``` {.r}
dat <- data.frame(
    x = factor(rep(c("a", "b", "c"), each = 3)),
    y = rep(4:6, each = 3)
)
dat$x <- ordered(dat$x, levels = c("a" ,"c" ,"b"))
dat
dat$x
makedummies(dat, numerical = c("x", "y"))
```

``` {.example}
  x y
1 a 4
2 a 4
3 a 4
4 b 5
5 b 5
6 b 5
7 c 6
8 c 6
9 c 6

[1] a a a b b b c c c
Levels: a < c < b

  x y
1 1 4
2 1 4
3 1 4
4 3 5
5 3 5
6 3 5
7 2 6
8 2 6
9 2 6
```

### "as.is" option

``` {.r}
dat <- data.frame(
    x = factor(rep(c("a", "b", "c"), each = 3)),
    y = factor(rep(1:3, each = 3))
)
makedummies(dat, as.is = "x")
```

``` {.example}
  x y_2 y_3
1 a   0   0
2 a   0   0
3 a   0   0
4 b   1   0
5 b   1   0
6 b   1   0
7 c   0   1
8 c   0   1
9 c   0   1
```

``` {.r}
makedummies(dat, as.is = c("x", "y"))
```

``` {.example}
  x y
1 a 1
2 a 1
3 a 1
4 b 2
5 b 2
6 b 2
7 c 3
8 c 3
9 c 3
```
