# rankheatplot

Rank-heat plots for network meta-analyses with several outcomes, in R and as
a Shiny app. The package recreates the plot from the original rank-heat plot
web tool (Veroniki et al., 2016), which is no longer online.

Each ring is an outcome (the first outcome is the outermost ring), each
sector is a treatment, and each cell is coloured by the treatment's ranking
statistic (SUCRA, P-score, ...) on a red (0%) - yellow (50%) - green (100%)
scale. Cells without data are left white.

## Install

```r
# install.packages("remotes")
remotes::install_github("fparaiso19/rankheatplot_")
```

## Use

Data go in the same layout as the original tool: treatments in the first
column, one column per outcome, empty cells for missing data.

```csv
treatment,Efficacy,Remission,Quality of life,Serious adverse events,Discontinuation
Drug A,88,80,73,35,42
Drug B,71,66,79,48,55
Drug E,40,,38,80,
Placebo,9,12,22,91,75
```

```r
library(rankheatplot)

d <- read_rankheat("my_sucras.csv")          # .csv, .tsv or .xlsx
p <- rankheatplot(d)
p

# Journal-ready TIFF (LZW compressed). Fonts rescale with the figure size.
save_rankheatplot(p, "rankheat.tiff", width = 7, height = 7, dpi = 600)
save_rankheatplot(p, "rankheat.pdf")
```

Values between 0 and 1 (e.g. P-scores) are converted to percentages
automatically. Long data (`treatment`, `outcome`, `value` columns) and
matrices with treatment row names work too.

Straight from `netmeta`:

```r
rh <- rankheat_from_netmeta(
  list(Efficacy = nm_eff, Acceptability = nm_acc),
  small.values = c("undesirable", "desirable")
)
rankheatplot(rh)
```

### Options

| Argument | What it does |
|---|---|
| `treatment_order` | `"data"`, `"mean"` (best first) or `"alphabetical"`, or a vector of names |
| `outcome_order` | vector of outcome names, outermost first |
| `palette` | `"veroniki"` (original), `"ryg"`, `"RdYlGn"`, `"grey"`, `"viridis"` or your own colours |
| `show_values`, `digits`, `na_label` | numbers in the cells; `na_label = "*"` marks missing cells as in the paper's grey version |
| `outcome_title`, `legend_title`, `legend_position` | headings and legend |
| `treatment_size`, `outcome_size`, `value_size`, `family` | text |
| `inner_radius`, `sector_gap`, `border_colour` | geometry |

The result is a regular `ggplot` object.

## Shiny app

```r
rankheatplot::run_rankheat_app()
```

Upload a CSV/Excel file or paste the table, adjust order, colours and text,
and download TIFF, PNG, PDF, SVG or EPS at any size and resolution.

## Reference

Veroniki AA, Straus SE, Fyraridis A, Tricco AC. The rank-heat plot is a novel
way to present the results from a network meta-analysis including multiple
outcomes. *J Clin Epidemiol*. 2016;76:193-199.
doi:10.1016/j.jclinepi.2016.02.016
