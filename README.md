# ggcirclize <img src="data/tracks.svg" align="right" height="200" />

<!-- badges: start -->

[![Version](https://img.shields.io/badge/version-0.0.3-2a6db5.svg)](https://github.com/junjunlab/ggcirclize)
[![Documentation](https://img.shields.io/badge/docs-Read%20the%20Docs-2a6db5.svg)](https://ggcirclize.readthedocs.io/)
[![License: MIT](https://img.shields.io/badge/license-MIT-green.svg)](https://opensource.org/licenses/MIT)
[![R](https://img.shields.io/badge/R-%E2%89%A5%204.1-1f65b0.svg)](https://cran.r-project.org/)
[![ggplot2](https://img.shields.io/badge/ggplot2-%E2%89%A5%203.4-1f65b0.svg)](https://ggplot2.tidyverse.org)
[![GitHub stars](https://img.shields.io/github/stars/junjunlab/ggcirclize?style=social)](https://github.com/junjunlab/ggcirclize/stargazers)

<!-- badges: end -->

**ggcirclize** extends the grammar of graphics to **circular visualisation**. Every
layer you already know from `ggplot2` — points, lines, bars, tiles, ribbons,
violins, boxplots — has a circular counterpart, and on top of that it ships a
complete set of **genomic track** layers (`chr / gstart / gend`), chord diagrams
and nested zooming. You keep writing `ggplot2` code; the circle is just another
coordinate system.

If you have ever needed a circos-style figure and did not want to learn a second,
low-level plotting engine, this package is for you.

---

## Highlights

| | |
|---|---|
| **Grammar, not a new API** | `ggcirclize(data, aes(...)) + geom_arcpoint()` — the same mapping, stats and `+` composition you use in `ggplot2`. |
| **Two track families** | `geom_arc*()` draws on the full circle, `geom_track*()` draws inside a radial band you position with `r0` / `r1`. |
| **Genomic layers** | `geom_trackgenomic*()` maps chromosomes to sectors with `chrom_data`, so `chr`, `gstart`, `gend` are all you need. |
| **Chord diagrams** | `geom_chordDiagram()` turns a from/to/value data frame into a link diagram. |
| **Nested zooming** | `geom_genomicNestedZoom()` spotlights a genomic region inside the same circle instead of a second panel. |
| **Circular facets** | `facet_sector()` splits the circle into independent sectors, each with its own scales. |
| **Low-level grobs** | `arcSectorGrob()`, `arcAxisGrob()`, `arcTextGrob()` … are exported, so you can build your own layers. |

---

## Installation

You can install the development version of ggcirclize like so:

```r
# install.packages("devtools")
devtools::install_github("junjunlab/ggcirclize")

# or
remotes::install_github("junjunlab/ggcirclize")
```

`ggcirclize` needs R ≥ 4.1 and `ggplot2` ≥ 3.4. The heavy lifting on the geometry
side is done by [`circlize`](https://github.com/jokergoo/circlize) and
[`ComplexHeatmap`](https://github.com/jokergoo/ComplexHeatmap), which are installed
automatically.

---

## Quick start

```r
library(ggcirclize)
library(ggplot2)

# 1. A plain circular scatter: x maps to the angle, y to the radius
ggcirclize(data = mtcars, mapping = aes(x = mpg, y = disp, end = 360)) +
  geom_arcpoint()
```

```r
# 2. Several tracks on one circle
set.seed(1)
df <- data.frame(
  x = rep(1:24, 2),
  y = c(runif(24, 2, 6), runif(24, 3, 8)),
  gp = rep(c("A", "B"), each = 24)
)

ggcirclize(df, aes(x = x, y = y, gp = gp, end = 360)) +
  geom_arcline() +
  geom_arcpoint(size = 1.5)
```

```r
# 3. Genomic data: chromosomes become sectors
data("hg19_chrom_info", package = "ggcirclize")
chrom_hg19 <- hg19_chrom_info$chromsize
bed <- circlize::generateRandomBed(nr = 200)

ggcirclize(bed, aes(end = 360, chr = chr, gstart = start, gend = end, value = value1)) +
  geom_trackgenomicpoint(chrom_data = chrom_hg19)
```

---

## Layer overview

Nearly every `ggplot2` layer has a circular counterpart here. **Arc layers** draw
on the circle itself; **track layers** draw the same thing inside a radial band
that you place with `r0` / `r1`.

| `ggplot2` | Arc layer (full circle) | Track layer (radial band) |
|:----------|:------------------------|:--------------------------|
| `geom_point()` | `geom_arcpoint()` | `geom_trackpoint()` |
| `geom_line()` | `geom_arcline()` | `geom_trackline()` |
| `geom_path()` | `geom_arcpath()` | — |
| `geom_segment()` | `geom_arcsegment()` | — |
| `geom_bar()` (count) | `geom_arcbar()` | `geom_trackbar()` |
| `geom_col()` (identity) | `geom_arccol()` | `geom_trackcol()` |
| `geom_histogram()` | `geom_archistogram()` | `geom_trackhist()` |
| `geom_rect()` | `geom_arcrect()` | `geom_trackrect()` |
| `geom_tile()` | `geom_arctile()` | `geom_tracktile()` / `geom_tracktile2()` |
| `geom_area()` | `geom_arcarea()` | `geom_trackarea()` |
| `geom_ribbon()` | `geom_arcribbon()` | `geom_trackribbon()` |
| `geom_violin()` | `geom_arcviolin()` | `geom_trackviolin()` |
| `geom_boxplot()` | `geom_arcboxplot()` | `geom_trackboxplot()` |
| `geom_crossbar()` | `geom_arcrossbar()` | — |
| `geom_density()` | `geom_arcdensity()` | `geom_trackdensity()` |
| `geom_linerange()` | `geom_arclinerange()` | — |
| `geom_polygon()` | `geom_arcpolygon()` | — |
| `geom_text()` | `geom_arctext()` | — |
| `geom_rect()` (sector) | `geom_arcsector()` | `geom_tracksector()` |
| `geom_curve()` (arrow) | `geom_arcarrow()` | — |

**Genomic track layers** — take `chr` / `gstart` / `gend` and turn chromosomes into
sectors through `chrom_data`:

| Layer | What it draws |
|:------|:--------------|
| `geom_trackgenomicpoint()` | points at genomic positions |
| `geom_trackgenomicline()` | a line joining genomic positions |
| `geom_trackgenomicarea()` | filled area under the values |
| `geom_trackgenomicrect()` | rectangles spanning `gstart`–`gend` |
| `geom_trackgenomictile()` | a tile per genomic interval |
| `geom_trackgenomictile2()` | tile variant with per-chromosome fill |
| `geom_trackgenomicdensity()` | density along the chromosome |
| `geom_trackgenomicrainfall()` | rainfall-style depth plot |
| `geom_trackgenomiclink()` | links between two genomic regions |
| `geom_trackgenomiclabel()` | gene labels with leader lines |
| `geom_trackgenomiclabel2()` | labels placed by smart alignment |

**Also available**

| Function | Purpose |
|:---------|:--------|
| `geom_chordDiagram()` | chord / link diagram from a `from`/`to`/`value` table |
| `geom_genomicNestedZoom()` | nested zoom of a genomic region inside the same circle |
| `facet_sector()` | split the circle into independent sectors with their own scales |
| `geom_cpoint()` | plain scatter points, for mixing with the arc layers |

**Low-level grobs** — exported, so you can compose your own layers:

| Grob | Draws |
|:-----|:------|
| `arcSectorGrob()` | a sector (the arc equivalent of a rectangle) |
| `arcRectGrob()` | a rectangle in polar space |
| `arcPointGrob()` | points |
| `arcLinesGrob()` | a polyline |
| `arcSegmentsGrob()` | straight segments between two angles |
| `arcPolygonGrob()` | a closed polygon |
| `arcArrowGrob()` | an arrow between two angles |
| `arcTextGrob()` | text with circlize-compatible facing |
| `arcAxisGrob()` | a circular axis with ticks and labels |
| `arcLinksGrob()` | links between two sets of angles |
| `arcLinks2Grob()` | links with a mid-point offset |

---

## Citation

> Jun Zhang (2023). *Creating Circular Graph with the Grammar of GGplot2.*  https://github.com/junjunlab/ggcirclize

---

## News

### 0.0.3

- 2026/10/29  (Documentation moved to **Sphinx + Read the Docs**, replacing the old bookdown manual.)
- 2026/10/29  (Fixing the package being **completely unusable under ggplot2 4.x**: `...` forwarding, lost `mapping` class, and `expand` semantics.)
- 2026/10/29  (Restoring **StatArcbar** / **StatArcboxplot** / **StatArcdensity** that were dropped in 0.0.2, so `geom_arcbar()`, `geom_trackbar()`, `geom_trackboxplot()` and `geom_trackdensity()` no longer error out on call.)
- 2026/10/29  (**geom_trackgenomiclabel** / **geom_trackgenomiclabel2** label placement and link segments reworked to match the manual examples; outer chr labels now all follow the arc.)
- 2026/10/29  (Text and axes aligned with circlize: unified `arc_theta()` angle mapping, full `facing` semantics, and a rewritten **arcAxisGrob**.)
- 2026/10/29  (Added `tests/smoke.R` regression suite covering 60+ layers, grobs and utility functions.)

### 0.0.2

- 2023/12/08  (Adding **geom_genomicNestedZoom** layer to visualize zoomed plot regions.)
- 2023/12/06  (Adding **geom_tracktile2** and **geom_trackgenomictile2** layers.)
- 2023/12/02  (Adding **geom_chordDiagram** layer.)
- 2023/11/29  (Adding **geom_trackgenomiclabel2** layer.)

### 0.0.1

- 2023/11/26  (First version pushed.)

---

## Documentation

The reference manual is built with Sphinx and hosted on Read the Docs:

- English site: [https://ggcirclize.readthedocs.io/](https://ggcirclize.readthedocs.io/)

The old bookdown manual ([https://junjunlab.github.io/ggcirclize-manual/](https://junjunlab.github.io/ggcirclize-manual/)) is kept
for the 0.0.2 release and will no longer be updated.

---

## Related blogs

- [ggcirclize 绘制基因组数据初探](https://mp.weixin.qq.com/s?__biz=MzkyMTI1MTYxNA==&mid=2247510637&idx=1&sn=e9d02f505f0b69fc8735980a3aa59ca7&chksm=c184901cf6f3190a5de6abaec2c4a353f68b9dff9a29bd453f7b71bb69ff209c4f79c7484f48&token=1656012957&lang=zh_CN#rd)
- [Nested Zooming: 嵌套放大](https://mp.weixin.qq.com/s?__biz=MzkyMTI1MTYxNA==&mid=2247510595&idx=1&sn=364f5064994bf08891d06cea0d6c8d13&chksm=c1849032f6f31924e1cf67ed6a793a7c7e1cfc8f4c86930d8d923ba04085fe60d0fb099dd586&token=1656012957&lang=zh_CN#rd)
- [环形分面布局探索](https://mp.weixin.qq.com/s?__biz=MzkyMTI1MTYxNA==&mid=2247510570&idx=1&sn=9bb8e881fc4aecd5eed8a778d0389453&chksm=c184905bf6f3194d7109d94badfee405ebd5dfc50d692b65072629e093ec26eb7c469bf01b13&token=1656012957&lang=zh_CN#rd)
- [arcpolygon 画掰弯的图形](https://mp.weixin.qq.com/s?__biz=MzkyMTI1MTYxNA==&mid=2247510542&idx=1&sn=d879dd0d564350f138de7f9432aa39df&chksm=c184907ff6f31969434cb41e70e970925af43bac29133793312e426f42dcf26ed7aeb292b4df&token=1656012957&lang=zh_CN#rd)
- [从 grid 到 ggplot 的跃迁](https://mp.weixin.qq.com/s?__biz=MzkyMTI1MTYxNA==&mid=2247510471&idx=1&sn=575f2b70b59ae561983f913a484bb86d&chksm=c18493b6f6f31aa0c2973537e8f0c3e528ef229dd65c9882c085f253586601ae606165d39be1&token=1656012957&lang=zh_CN#rd)
- [环形坐标轴研究](https://mp.weixin.qq.com/s?__biz=MzkyMTI1MTYxNA==&mid=2247510412&idx=1&sn=48a962270d05ee40af30f9295192d4f3&chksm=c18493fdf6f31aebba6a59f3d811915b0e51bc02d031d61b84c69eea14af4d1c5a4f11dec8a0&token=1656012957&lang=zh_CN#rd)
- [圈圈补充](https://mp.weixin.qq.com/s?__biz=MzkyMTI1MTYxNA==&mid=2247510297&idx=1&sn=9232131249c4cb2cc76f835f924bd47e&chksm=c1849368f6f31a7eb57b2c55cdf8a8c11a932147352414845361cba1e94afac91ce541b7ef43&token=1656012957&lang=zh_CN#rd)
- [画一些圈圈](https://mp.weixin.qq.com/s?__biz=MzkyMTI1MTYxNA==&mid=2247510275&idx=1&sn=d4c77bf68fe00e941dc138ec91bdc46d&chksm=c1849372f6f31a6406981ab57abae8c46d04a255b4288a93509dae26c42e4dd82a519edea531&token=1656012957&lang=zh_CN#rd)

---

## Acknowledgement

`ggcirclize` would not exist without [`circlize`](https://github.com/jokergoo/circlize)
and [`ComplexHeatmap`](https://github.com/jokergoo/ComplexHeatmap) by Zuguang Gu —
they provide the arc geometry, the axis machinery and the genomic coordinate
handling that this package wraps in a `ggplot2` interface. Thanks also to the
`ggplot2` team for a grammar that turned out to bend quite well.

---

## Getting help

- Browse the [reference manual](https://ggcirclize.readthedocs.io/) — every layer has a runnable example.
- Found a bug or have a question? Open an [issue](https://github.com/junjunlab/ggcirclize/issues).
- Pull requests are welcome; `tests/smoke.R` runs the rendering regression suite.

---

## License

MIT © Jun Zhang. See [LICENSE.md](LICENSE.md) for details.
