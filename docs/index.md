# ggcirclize Reference

**ggcirclize** is an R package that extends the grammar of graphics
([`ggplot2`](https://ggplot2.tidyverse.org)) to **circular visualizations**. It
lets you draw circular scatter points, lines, bars, tiles, chord diagrams and a
full set of *genomic track* layers using familiar `ggplot2` syntax — no need to
learn a second, low-level plotting engine.

```r
# install.packages("devtools")
devtools::install_github("junjunlab/ggcirclize")

library(ggcirclize)

ggcirclize(data = mtcars, mapping = aes(x = mpg, y = disp)) +
  geom_arcpoint()
```

```{toctree}
:maxdepth: 1
:caption: Contents

chapters/low-level-arcgrobs
chapters/geom-arc-layers
chapters/geom-track-layers
chapters/geom-genomic-track-layers
chapters/geom-chorddiagram-layers
chapters/add-significant-marks
chapters/nested-zooming
chapters/adding-gene-labels
chapters/create-own-genome-track
```

## Highlights

- **Low-level arc grobs** — `arcSectorGrob`, `arcAxisGrob`, `arcTextGrob`, and
  friends, the building blocks every layer is drawn with.
- **Arc & track layers** — `geom_arcpoint`, `geom_arcline`, `geom_arcbar`,
  `geom_arctile`, `geom_trackpoint`, `geom_trackbar`, … mirror their linear
  `ggplot2` counterparts on a circle.
- **Genomic track layers** — `geom_trackgenomicpoint`, `geom_trackgenomicline`,
  `geom_trackgenomictile`, `geom_trackgenomiclink`, `geom_trackgenomiclabel`
  and more, driven by `chr / gstart / gend` aesthetics.
- **Chord diagrams** — `geom_chordDiagram` and friends.
- **Nested zooming** — `geom_genomicNestedZoom` for spotlighting a genomic
  region inside the same circle.

## Getting help

* Source & issues: <https://github.com/junjunlab/ggcirclize>
* Reference manual: this site
