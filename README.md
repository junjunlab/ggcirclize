# ggcirclize <img src="data/tracks.svg" align="right" height="200" />
 
<!-- badges: start -->

**ggcirclize** is inspired by some circular plot R packages and is designed to create circular graph with the grammar of ggplot2.

<!-- badges: end -->

---

## Installation

You can install the development version of ggcirclize like so:

``` r
# install.packages("devtools")
devtools::install_github("junjunlab/ggcirclize")

# or
remotes::install_github("junjunlab/ggcirclize")
```

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
