
<!-- README.md is generated from README.Rmd. Please edit that file -->

# severance <img src="man/figures/logo.png" align="right" height="300" />

The severance package contains color palettes for R inspired by the show
[Severance](https://tv.apple.com/us/show/severance/umc.cmc.1srk2goyh2q2zdxcx605w8vtx).

Images were sourced from the [Apple TV+ Press
website](https://www.apple.com/tv-pr/originals/severance/episodes-images/).
The structure of the package was based on the [MetBrewer
package](https://github.com/BlakeRMills/MetBrewer) by Blake R. Mills and
the [wesanderson package](https://github.com/karthik/wesanderson) by
Karthik Ram.

-   [Installation](#installation)
-   [Example](#example)
-   [Palettes](#palettes)
-   [Notes](#notes)

## Installation

You can install the development version of severance from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ivelasq/seveRance")
```

## Example

Below is an example using [TidyTuesday
data](https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-02-23/readme.md)
from the Bureau of Labor Statistics. The plot is based on one created by
Amanda Luby from the [Swarthmore College Data Visualization
Group](https://aluby.domains.swarthmore.edu/sdv/posts/2021-03-05-bls-earnings-data/).

<img src="man/figures/README-plot-ref-1.png" width="100%" />

The code to reproduce the plot is below:

``` r
library(severance)
library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyr)

earn <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv'
  )

earn_clean <- earn %>%
  mutate(median_yearly = median_weekly_earn * 52) %>%
  filter(race != "All Races",
         sex != "Both Sexes") %>%
  group_by(race, sex, year, quarter) %>%
  summarize(median_salary = median(median_yearly)) %>%
  pivot_wider(
    id_cols = c("race", "year", "quarter"),
    names_from = sex,
    values_from = median_salary
  ) %>%
  group_by(race) %>%
  mutate(
    time = paste(year, quarter, sep = "-Q"),
    men_dev = Men - (Men + Women) / 2,
    women_dev = Women - (Men + Women) / 2,
    race = factor(race, levels = c(
      "Asian", "White", "Black or African American"
    ))
  )

p <-
  ggplot(earn_clean,
         aes(
           x = time,
           ymin = women_dev,
           ymax = men_dev,
           fill = race,
           group = race
         )) +
  geom_ribbon() +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

p1 <- p + scale_fill_manual(values = severance_palette("Jazz02"))
p2 <- p + scale_fill_manual(values = severance_palette("TheYouYouAre"))
p3 <- p + scale_fill_manual(values = severance_palette("Hell"))

title <- ggdraw() +
  draw_label(
    "The gender wage gap persists across Asian, Black, and White Americans",
    fontface = "bold",
    x = 0,
    hjust = 0
  ) +
  draw_label(
    "Original visualization by Amanda Luby, Swarthmore College",
    x = 0,
    vjust = 2.2,
    hjust = 0
  ) +
  theme(plot.margin = margin(0, 0, 0, 7))

prow <- plot_grid(
  p1,
  p2,
  p3,
  align = 'h',
  labels = c("Jazz02", "TheYouYouAre", "Hell"),
  hjust = 0,
  nrow = 1
)

caption <- ggdraw() +
  draw_label("Source: BLS Earnings Data",
             hjust = -0.3,
             vjust = -0.5)

plot_grid(title,
          prow,
          caption,
          ncol = 1,
          # rel_heights values control vertical title margins
          rel_heights = c(0.3, 1))
```

## Palettes

### Colorblind-friendly

------------------------------------------------------------------------

#### Dinner

<img src="man/figures/dinner_cbf.jpg" alt="A scene from Severance where Helly and Dylan are standing in a room">

#### Hell

<img src="man/figures/hell_cbf.jpg" alt="A scene from Severance where Helly is sitting on the floor">

#### Jazz02

<img src="man/figures/jazz02_cbf.jpg" alt="A scene from Severance where the four main characters are staring at something">

#### TheYouYouAre

<img src="man/figures/theyouyouare_cbf.jpg" alt="A scene from Severance where Helly sits in a chair">

### Not colorblind-friendly

------------------------------------------------------------------------

#### HideAndSeek

<img src="man/figures/hideandseek_ncbf.jpg" alt="A scene from Severance where a bunch of characters look at Milchick">

#### Jazz01

<img src="man/figures/jazz01_ncbf.jpg" alt="A scene from Severance where Helly and Milchick dance">

## Notes

Thank you to the package developers of [MetBrewer
package](https://github.com/BlakeRMills/MetBrewer), Blake R. Mills, and
the [wesanderson package](https://github.com/karthik/wesanderson),
Karthik Ram, for sharing their code.

Thank you to Amanda Luby from the [Swarthmore College Data Visualization
Group](https://aluby.domains.swarthmore.edu/sdv/posts/2021-03-05-bls-earnings-data/)
for sharing her data visualization.

Colorblind-friendliness was checked using the [Adobe Color Accessibility
Wheel](https://color.adobe.com/create/color-accessibility).
