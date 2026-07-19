# Introduction to timbr

``` r

library(timbr)
library(dplyr)
```

Hierarchical data usually lives in a flat data frame with one column per
level: sales by region, store, and product; energy demand by region,
sector, and fuel; cost breakdowns by assembly and part. dplyr handles
one level of aggregation naturally, but each
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
returns a new, flat data frame. After aggregating, you no longer know
how the levels relate to each other, and computations that need both
parents and children – rollups with losses, top-down allocations,
scenario propagation – turn into a dance of repeated joins.

timbr keeps the whole hierarchy in a single object called a **forest**.
A forest is built from a data frame, prints like one, and supports
familiar dplyr verbs – but its rows are nodes of trees, so aggregating
adds parent nodes instead of throwing the structure away.

This article walks through the basics with a small energy demand
dataset:

``` r

energy <- tribble(
  ~region , ~sector     , ~fuel         , ~demand ,
  "north" , "household" , "electricity" ,      20 ,
  "north" , "household" , "gas"         ,      25 ,
  "north" , "industry"  , "electricity" ,      30 ,
  "north" , "industry"  , "gas"         ,      45 ,
  "south" , "household" , "electricity" ,      40 ,
  "south" , "household" , "gas"         ,      15 ,
  "south" , "industry"  , "electricity" ,      60 ,
  "south" , "industry"  , "gas"         ,      30
)
```

## From data frame to forest

[`forest_by()`](https://uchidamizuki.github.io/timbr/reference/forest_by.md)
works like
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html):
you list the columns that define the hierarchy, from the top level down.
The last column becomes the nodes of the forest, and the remaining
columns become groups:

``` r

fr <- energy |>
  forest_by(region, sector, fuel)
fr
#> # A forest: 8 nodes and 1 feature
#> # Groups:   region, sector [4]
#> # Trees:    
#> #   fuel [8]
#>   region sector    .                  demand
#>   <chr>  <chr>     <node>              <dbl>
#> 1 north  household <fuel> electricity     20
#> 2 north  household <fuel> gas             25
#> 3 north  industry  <fuel> electricity     30
#> 4 north  industry  <fuel> gas             45
#> 5 south  household <fuel> electricity     40
#> 6 south  household <fuel> gas             15
#> 7 south  industry  <fuel> electricity     60
#> 8 south  industry  <fuel> gas             30
```

The `.` column shows each node’s name and value (here, `fuel` nodes).
The other columns – called **features** – hold the data attached to each
node, like `demand` here.

## Hierarchical aggregation

[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html) on
a forest looks just like
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html) on
a grouped data frame. The difference is that instead of discarding the
`fuel` level, it adds parent nodes on top of it:

``` r

fr_sector <- fr |>
  summarise(demand = sum(demand))
fr_sector
#> # A forest: 12 nodes and 1 feature
#> # Groups:   region [2]
#> # Trees:    
#> #   sector [4]
#> #   └─fuel [8]
#>   region .                  demand
#>   <chr>  <node>              <dbl>
#> 1 north  <sector> household     45
#> 2 north  <sector> industry      75
#> 3 south  <sector> household     55
#> 4 south  <sector> industry      90
```

The header now shows the tree structure: `sector` nodes with `fuel`
children. Summarise again to add a `region` level:

``` r

fr_region <- fr_sector |>
  summarise(demand = sum(demand))
fr_region
#> # A forest: 14 nodes and 1 feature
#> # Trees:    
#> #   region [2]
#> #   └─sector [4]
#> #     └─fuel [8]
#>   .              demand
#>   <node>          <dbl>
#> 1 <region> north    120
#> 2 <region> south    145
```

The forest prints its root nodes, but every level is still there –
nothing has been lost along the way.

## Moving around the hierarchy

[`children()`](https://uchidamizuki.github.io/timbr/reference/children.md)
steps down one level, turning the current roots into groups:

``` r

children(fr_region)
#> # A forest: 12 nodes and 1 feature
#> # Groups:   region [2]
#> # Trees:    
#> #   sector [4]
#> #   └─fuel [8]
#>   region .                  demand
#>   <chr>  <node>              <dbl>
#> 1 north  <sector> household     45
#> 2 north  <sector> industry      75
#> 3 south  <sector> household     55
#> 4 south  <sector> industry      90
```

[`climb()`](https://uchidamizuki.github.io/timbr/reference/climb.md)
jumps to a named level anywhere in the forest:

``` r

fr_region |>
  climb(fuel)
#> # A forest: 8 nodes and 1 feature
#> # Trees:    
#> #   fuel [8]
#>   .                  demand
#>   <node>              <dbl>
#> 1 <fuel> electricity     20
#> 2 <fuel> gas             25
#> 3 <fuel> electricity     30
#> 4 <fuel> gas             45
#> 5 <fuel> electricity     40
#> 6 <fuel> gas             15
#> 7 <fuel> electricity     60
#> 8 <fuel> gas             30
```

And
[`leaves()`](https://uchidamizuki.github.io/timbr/reference/leaves.md)
returns the terminal nodes:

``` r

leaves(fr_region)
#> # A forest: 8 nodes and 1 feature
#> # Trees:    
#> #   fuel [8]
#>   .                  demand
#>   <node>              <dbl>
#> 1 <fuel> electricity     20
#> 2 <fuel> gas             25
#> 3 <fuel> electricity     30
#> 4 <fuel> gas             45
#> 5 <fuel> electricity     40
#> 6 <fuel> gas             15
#> 7 <fuel> electricity     60
#> 8 <fuel> gas             30
```

## Combining forests

Real hierarchies are often ragged: different branches have different
levels. Suppose transport demand is broken down by mode instead of
sector and fuel. [`rbind()`](https://rdrr.io/r/base/cbind.html) combines
forests, and a further
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
joins them under a common root:

``` r

transport <- tribble(
  ~region , ~mode , ~demand ,
  "north" , "car" ,      35 ,
  "north" , "bus" ,      10 ,
  "south" , "car" ,      45 ,
  "south" , "bus" ,      15
)

fr_transport <- transport |>
  forest_by(region, mode)

fr_total <- rbind(fr_sector, fr_transport) |>
  summarise(demand = sum(demand))
fr_total
#> # A forest: 18 nodes and 1 feature
#> # Trees:    
#> #   region [2]
#> #   ├─sector [4]
#> #   │ └─fuel [8]
#> #   └─mode [4]
#>   .              demand
#>   <node>          <dbl>
#> 1 <region> north    165
#> 2 <region> south    205
```

Each `region` node now has two differently shaped subtrees, and
hierarchical operations keep working across both.

## Custom hierarchical computations with traverse()

[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
covers computations where each parent is a simple aggregate of its
children.
[`traverse()`](https://uchidamizuki.github.io/timbr/reference/traverse.md)
handles everything else: it applies a function at every parent-children
pair, walking the forest from the leaves up. For example, suppose demand
at each aggregation level includes 5% distribution losses on top of its
children’s total:

``` r

fr_total |>
  traverse(function(parent, children) {
    parent$demand <- sum(children$demand) * 1.05
    parent
  })
#> # A forest: 18 nodes and 1 feature
#> # Trees:    
#> #   region [2]
#> #   ├─sector [4]
#> #   │ └─fuel [8]
#> #   └─mode [4]
#>   .              demand
#>   <node>          <dbl>
#> 1 <region> north   180.
#> 2 <region> south   223.
```

Because
[`traverse()`](https://uchidamizuki.github.io/timbr/reference/traverse.md)
walks the tree level by level, the losses compound naturally: regional
totals include losses on losses. With `.climb = TRUE` the walk runs in
the opposite direction, from roots to leaves, and the function receives
`(children, parent)` – useful for top-down allocations such as
distributing a target or an overhead cost across children.

## Back to data frames

[`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
converts the root nodes of a forest back into an ordinary tibble:

``` r

fr_sector |>
  as_tibble()
#> # A tibble: 4 × 3
#> # Groups:   region [2]
#>   region sector    demand
#>   <chr>  <chr>      <dbl>
#> 1 north  household     45
#> 2 north  industry      75
#> 3 south  household     55
#> 4 south  industry      90
```

Combined with
[`climb()`](https://uchidamizuki.github.io/timbr/reference/climb.md) or
[`leaves()`](https://uchidamizuki.github.io/timbr/reference/leaves.md),
this lets you pull out any level of the hierarchy once the computation
is done.

## Learn more

- The [function
  reference](https://uchidamizuki.github.io/timbr/reference/) covers the
  remaining verbs, including
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
  [`select()`](https://dplyr.tidyverse.org/reference/select.html),
  [`relocate()`](https://dplyr.tidyverse.org/reference/relocate.html),
  [`rows_update()`](https://dplyr.tidyverse.org/reference/rows.html),
  and [`rows_patch()`](https://dplyr.tidyverse.org/reference/rows.html)
  for forests.
- Forests convert to tidygraph objects via `as_tbl_graph()` for
  graph-oriented workflows.
