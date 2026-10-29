# ggcirclize 0.0.3

## 文字与坐标轴全面对齐 circlize

这一版不是打补丁，而是把“角度映射”和“文字排布”两套基础设施重写，让所有图层的
朝向、刻度密度、标签格式都遵循 `circlize` 的规则。

### 1. 统一数据到角度的映射（修正镜像错位）

早期版本里 **点 / 线 / 多边形 / 文字** 用 `from = rev(scale)`，而 **矩形 / 线段 /
坐标轴** 用 `from = scale`，同一个 x 值在两类图层里会落到镜像的位置上。现在全包只有
一个换算函数 `arc_theta()`：

```
clock.wise = FALSE   x 小 -> 扇区起点，x 大 -> 扇区终点
clock.wise = TRUE    整体镜像（2*pi - angle）
```

`arcPointGrob()`、`arcLinesGrob()`、`arcPolygonGrob()`、`arcTextGrob()` 已改为使用它。

### 2. `facing` 语义完全对齐 `circos.text()`

`arcTextGrob()` / 新增的 `arc_resolve_facing()` 实现了 circlize 的整套朝向：

| facing | 旋转 | 含义 |
|---|---|---|
| `inside` | theta - 90 | 沿弧读，朝外 |
| `outside` | theta + 90 | 沿弧读，朝内 |
| `clockwise` | theta | 沿半径读，朝内 |
| `reverse.clockwise` | theta - 180 | 沿半径读，朝外 |
| `downward` | 0 | 恒水平 |
| `bending.inside` / `bending.outside` | 逐字 | 文字沿弧弯曲 |

`nice.facing = TRUE` 按 circlize 的规则在文字会倒置时翻转 180 度并同时镜像 `adj`
（之前实现成“永远是 hjust = 0.5”，等于没生效）。`bending` 系列现在按标签**实际宽度**
决定占据的弧长，并按上下半圈自动切换读取方向，不再出现反字或倒字。

### 3. `arcAxisGrob()` 重写

* 新增 circlize 风格的参数：`labels.cex`、`labels.facing`、`labels.niceFacing`、
  `labels.col`、`labels.font`、`labels.digits`、`labels.format`、
  `labels.pos.adjust`、`major.by.degree`、`minor.ticks`、`direction`、`lwd`、`col`。
* **刻度密度按角度决定**：`major.by.degree = 20`（circlize 默认为 10，本包放宽一档以适配更小的扇区），先算出扇区
  角度跨度能放几个主刻度，再把数据间隔收敛到 1/2/5×10^k。窄扇区自动变稀，不再
  无论扇区多宽都硬塞 5 个刻度。
* 标签格式化走 `labels.digits` + 可选的 `labels.format` 函数（例如
  `function(x) paste0(x/1e6, "Mb")`），不再直接把数字丢给 `textGrob`。
* 首尾标签仍然按 `labels.pos.adjust` 往扇区内挪，但挪动量会被“与相邻标签之间的
  空隙”限住，不会再挪过头撞上邻居。
* 轴对象内部不再改写 `start`/`end` 再反推刻度位置（旧实现在非等距 breaks 时会漂移）。

### 4. 径向轴：默认水平标签 + 不再画次刻度

* 径向轴的 `labels.facing` 默认改为 **`"downward"`**（文字恒水平）。circlize 用的是
  `"inside"`，即切向排布；当扇区跨度接近 180 度时，扇区端部的切向几乎是竖直的，
  数字会竖排并互相挤压。需要 circlize 原生观感可以显式传
  `yAxis.params = list(labels.facing = "inside")`。
* `minor.ticks.n` 默认改为 `NULL`：角向轴取 5，径向轴取 0（`circlize::circos.yaxis()`
  本身就不画次刻度）。另外当径向带宽不足以容纳次刻度时（间距 < 2% 画布）会自动关闭，
  避免短轴上出现一排"梳齿"。
* 径向轴的首尾标签也会按 `labels.pos.adjust` 内收，且内收量受"与相邻标签的空隙"
  限制，不会再溢出扇区边界。

### 5. 扇区范围、次刻度与径向轴朝向（修正）

* **柱状图/矩形不再被压进扇区一角**：`GeomArcbar` 里轴范围原本会被上游传入的
  **全局** `xscale` 覆盖，而几何体是按扇区自身范围 `xlim` 定位的，两者不一致时
  柱子和刻度一起被压缩。现在强制 `xscale` 采用扇区自身的 `xlim`。
  对 `aes(x = class, sector = class)` 这种「扇区与 x 是同一个离散变量」的写法，
  柱子现在会铺满各自的扇区。
* **恢复径向轴的次要刻度**：上一版把 `minor.ticks.n` 默认设为 0（照搬
  `circos.yaxis()`），实际观感太素。现在两个轴都默认 5 个次刻度；只有次刻度间距
  小于画布 1%（真正挤不下）时才自动关闭。
* **径向轴标签朝向自适应**：切向倾角在 ±60 度以内时沿轴排布（贴合扇区），超过
  60 度（扇区端部切向接近竖直）时自动转为水平，避免竖排数字互相挤压。
* **单刻度不画标签**：只有 1 个刻度时轴标签不携带信息，且常与分面标签重复，
  现在直接省略。

* **窄扇区自动减少径向刻度**：新增 `arc_breaks_n_for_span()`，扇区角度跨度 <15°、
  <30°、<60° 时径向刻度数量依次降到 2、3、4 个，避免五个标签挤在两个扇区之间。
  用户显式传 `breaks` 时不受影响。

### 6. 单位换算不再硬编码

文字宽度、弧长、色带宽度之间的比较原来都用 `/2`（假设视口恒为 `[-1,1]`）。
新增 `arc_npc_per_radius()`，按当前视口实测换算，`bending`、`auto.fit`、标签抽稀
都改用它，自定义 viewport 下也不会错位。


本版以修复为主：0.0.2 在当前 ggplot2（>= 4.0）下几乎无法出图，本版修复了全部
已知崩溃点，并在此基础上做了依赖瘦身与代码清理。`R CMD check` 从 3 个 WARNING
变为 **0 ERROR / 0 WARNING / 0 NOTE**。

## 致命修复：包在 ggplot2 4.x 下完全不可用

* `ggcirclize()` 把 `mg.t`/`mg.r`/`mg.b`/`mg.l` 作为参数传给了 `ggplot()`，
  ggplot2 4.0 起会对未使用的 `...` 报错
  （`Arguments in ... must be used`）。现在只把 `...` 交给 `ggplot()`，边距仅用于
  `plot.margin`。
* `ggcirclize()` 用 `modifyList()` 合并 `aes()`，结果退化成普通 list，ggplot2 4.0
  报 `mapping must be created by aes()`。新增 `merge_mapping()`，在合并后恢复
  mapping 类；用户传入的映射优先级保持不变。
* `coord_cartesian(expand = 0)` 改为 `expand = FALSE`。

## 致命修复：0.0.2 误删的 stat 导致多个图层报 "Can't find stat called …"

0.0.1 中的 `StatArcbar` / `StatArcboxplot` / `StatArcdensity` 在 0.0.2 中被删除，
但 `geom_arcbar()`、`geom_trackbar()`、`geom_trackboxplot()`、`geom_trackdensity()`
仍然按名字引用它们，调用即报错。本版恢复这三个统计变换（`R/stat_arcbar.R`、
`R/stat_arcboxplot.R`、`R/stat_arcdensity.R`）并重新导出。

## 图层修复

* `geom_arcbar()`：默认 `stat = "bar"`（不存在）改为 `"arcbar"`，
  与 `geom_bar()` 的计数语义一致。
* `geom_trackviolin()`：默认 `stat = "arcydensity"`（不存在，应为 `"ydensity"` 的笔误）
  改为 `"ydensity"`；补上从未声明的 `polar.every` 形参（原来传递时直接
  `object 'polar.every' not found`）。
* `geom_arcpath()` / `geom_arcviolin()` / `geom_trackviolin()`：
  `dapply()` 依赖的 `single_value` 是未注册的 S3 泛型，分包环境下必然报
  `no applicable method`，改为普通函数。
* `geom_arcrossbar()`：
  * `has_notch` 用 `is.na()` 判断整列，多行数据报 `'length = 3' in coercion to 'logical(1)'`；
    改为 `all(!is.na(...))`，越界提示改为 `any(...)`。
  * 顶点坐标用 `c()` 按列拼接、`group` 却按行生成，多组箱线图顶点会互相错位；
    改为 `as.vector(rbind(...))` + `each = n` 严格按行生成。
  * 空数据直接返回 `zeroGrob()`。
* `geom_arcboxplot()` / `geom_arcpoint()`：没有离群点时 `GeomArcpoint$draw_panel()`
  收到 0 行数据，仍用空向量构造 `gpar()`，报
  `'gpar' element 'fontsize' must not be length 0`。现在空层返回 `zeroGrob()`。
* `geom_trackboxplot()`：引用了从未创建的 `new_width`，轴范围取到 `Inf` 后崩溃；
  改为由 `xmax - xmin` 推导箱宽。
* `geom_arctext()`：数值型 `label` 触发 `nchar()` 的 `non-character argument`；
  多行标签的颜色重复次数也不对（`rep()` 缺 `times`）。均已修正。
* `geom_arctext()`：`size` 直接当 `fontsize` 用（缺 `.pt` 换算），`size = 4` 被渲染成
  4pt（几乎不可见），应为 `4 * .pt ≈ 11.4pt`。现改为 `fontsize = data$size * .pt`，
  与 `geom_text()` 的换算一致。
* `geom_tracktile2()` / `geom_trackgenomictile2()` / `geom_trackgenomiclabel2()`：
  同样缺 `.pt` 换算的 mark 点标记字号（字符型 `pch` 的字号）补上 `* .pt`，
  与 `geom_arcpoint()` 的做法一致。
* `stat_arcbar()`：照搬 `StatCount` 的 `setup_params` 错误地拒绝 `x`、`y` 同时映射
  （报 "must only have an x or y aesthetic"），导致 `geom_trackbar()`/`geom_arcbar()`
  无法画「离散 x + 数值 y」的柱状图。现支持两种模式：仅 `x`（或仅 `y`）时计数
  （`geom_bar` 语义），`x + y` 同时映射时按 `x` 分组、对 `y` 求和作为柱高
  （`geom_col` 语义）。
* 12 处 `message(...)` 后继续执行（接着对 `NULL` 做 `transform()`）改为
  `cli::cli_abort()`，给出可读的错误信息。
* `geom_trackgenomiclabel()` / `geom_trackgenomiclabel2()`：标签原本以**切向**
  朝向绘制（`inward`），文字宽度沿弧方向铺开，而 `label2` 的 `smartAlign2()` 是按
  文字**高度**计算角向占位的——两者对不上，标签必然堆叠成一团。现在标签改为
  **径向**朝向（`facing = "clockwise"` + `nice.facing`），占位与文字高度一致，
  标签沿半径排列、互不重叠，与文档效果一致。
* 连接线的方向写反了。`link_pos = "top"` 本应把标签放在环内、连线也朝环内，
  `"bottom"` 则都在环外；原实现用 `rev(seq(...))` 让 `r.y[1]` 落到了远端，
  而 `r.y[1]` 是用来画贴着染色体的 `up_line` 的——于是三段连线（竖线 + 弧段 +
  竖线）首尾对不上，看起来是几段断开的短线。现在 `r.y` 从轨道向标签方向单调伸展，
  `r.y[1]` 始终贴着染色体，连线连续不断。
* `geom_trackgenomiclabel()`：标签文字以自身中心对齐，会向两侧延伸压到连接线。
  `arcTextGrob()` 新增 `adj` 参数，`label` 用它把文字锚定在连线末端并**向外**展开
  （`top` 用 `adj = c(1, 0.5)`，`bottom` 用 `c(0, 0.5)`）。
* `arcTextGrob()`：`auto.fit` 在窄扇区触发时，会用 `adj = c(0.5, 0.5)` 覆盖调用方
  传入的 `adj` 并把朝向重设为居中径向，导致顶部区域的标签被拉平、重新压线。现在
  只对仍是沿弧朝向的标签做转向，已是径向（`clockwise`）的标签保留调用方的 `adj`。
* `geom_trackgenomiclabel()` / `geom_trackgenomiclabel2()`：外圈染色体（strip）标签
  方向不一致——`auto.fit` 会把放不下的标签转成径向，于是长染色体沿弧读、短染色体
  横着读。现在 strip 标签固定沿弧、统一字号，不再按扇区单独缩放，同一张图里
  所有染色体标签的外观完全一致。
* `geom_trackgenomiclabel2()`：标签原本居中（`adj = 0.5`），文字一半朝外压到连接线上。
  现在与 `label` 一致：锚在连线末端并单向朝远离轨道的一侧展开
  （`top` 用 `c(1, 0.5)`，`bottom` 用 `c(0, 0.5)`）。
* 标签与连线之间的间隙由 `0.02` 加大到 `0.05`：圆左右两侧的半径是水平的，标签和
  连线共线，原来的间隙视觉上会糊在一起。
* `geom_trackgenomiclabel()` / `geom_trackgenomiclabel2()`：标签排布改为**全图统一
  规则**，与 `circlize::circos.genomicLabels()` 一致——
  * 每个标签按自己的文字高度在半径上占一个角向槽位（`arc_label_gap()`）；
  * 按真实基因组位置排序后做一次最小位移的推送（`arc_smart_align()`），
    只在会和邻居重叠时才移动，顺序不变；
  * 所有标签共用同一个间距，不再"等距塞进扇区"（旧实现会把窄扇区的标签压成一团、
    宽扇区的标签拉得很开）。
  这样任何一张图里的标签间距、朝向、连接线形状都彼此相同，没有哪个标签被特殊对待。
* `arc_text_geometry()`（径向标签）：`hjust` 现在按"文字阅读方向与向外半径的夹角"
  逐点决定，使每个标签都锚在外端、朝圆心方向书写。此前用固定 `adj`，圆左侧和
  下半圈的标签会反过来朝外写，正好压住自己的连接线。
* 连接线的径向布局回归 0.0.2 的做法：连线整段放在**轨道外侧**（`r.y` 由轨道向外
  排开），标签放在轨道**内侧** 0.025 处。之前把连线塞进轨道内侧、让它横穿标签区，
  几十条连线叠在一起看就是一团乱线。现在连线有自己的一条窄带，标签在另一侧，
  互不干扰。`link_r` 默认值同步回到 `0.1`。
* 连接线的中间段改为**真正的圆弧**。`arcSegmentsGrob()` 在 `r0 != r1` 时同时插值
  半径和角度，画出来是一条斜线；几十个标签连线的斜线互相穿插，整圈看起来就是
  一团乱线。现在中间段固定在一个半径上，`geom_trackgenomiclabel2()` 里手写的
  `polylineGrob()` 版本同样改掉，连线变成干净的"⌐"形。
* `arc_text_geometry()`：`nice_facing` 对 `clockwise` / `reverse.clockwise` 用
  `deg >= 90 & deg < 270` 判断是否翻转，半开区间把正下方 270° 排除在外，圆底部的
  径向标签因此不翻转、整个头朝下。改为把角度折进 `[-90, 90]`
  （`base - 180 * ceiling((base - 90) / 180)`），任意角度都不会倒置。
* `geom_trackgenomiclabel2()`：测量标签高度时 `pushViewport()` 后从未
  `popViewport()`，每个扇区泄漏一个 viewport，现已补上配对。
* `geom_trackarea()` / `geom_arcribbon()`：离散型 `x`（如 `"a"/"b"/"c"`）时
  面积图渲染为空。根因是 ggplot2 会按离散 `x` 的每个水平拆成独立的 `group`，
  ribbon/area 退化成一个点的多边形，什么都画不出来。现在在 `GeomArcribbon` 的
  `draw_panel()` 里，当 `x` 为离散 scale 时按 `PANEL/sector/fill/colour/…` 重建
  `group`（`arc_regroup_ribbon()`），并让坐标轴在离散水平上打刻度、用水平名做标签
  （`arc_discrete_axis_params()`），与 `geom_arcpoint()` 的既有做法一致。
  `geom_arcarea()` 因依赖 `stat = "align"`（要求数值 `x`），离散 `x` 下仍返回空图，
  属设计使然。

## `geom_genomicNestedZoom()` 修复

* `draw_panel()` 直接使用了从未定义的 `x0`/`y0`（圆心坐标），一旦进入缩放面板绘制
  就报 `object 'x0' not found`。已补上圆心定义。
* 缩放区域所在染色体不在绘图数据中时，`sec.sum` 为空导致
  `only 0's may be mixed with negative subscripts`；现在跳过该区域。
* `nested_fill` 传单个颜色（如 `"lightcoral"`）时报
  `rep() invalid 'times' argument`：`rep(nested_fill, table(zoom_data$chr))` 里
  `table()` 带 class 属性、且 `nested_fill` 长度为 1 时 `rep` 的 `times` 解析失败。
  改为 `as.integer(table(...))` 取计数、`rep_len(nested_fill, length(cnt))` 先把颜色
  对齐到染色体数，再展开到每一行，同时支持单色和多色。

## 低层 grob 修复

* `arcAxisGrob()`：两处 `minor.y` 误用 `x0` 而非 `y0`，径向次刻度位置错误。
* `arcAxisGrob()`：`ifelse(is.null(x), a, x)` 反模式改为显式判断；
  `Reduce("rbind", list)` 改为 `do.call("rbind", list)`，空列表不再返回 `NULL`。

## 工具函数修复

* `download_chromInfo()`：`destfile = "./"` 指向目录，下载必然失败；现在写入
  `<dir>/<genome>.chrom.sizes` 与 `<dir>/<genome>.cytoBandIdeo.txt.gz`，
  新增 `dir`、`overwrite` 参数并返回文件路径。
* `get_chrom_data()`：未知 `genome` 时只打印消息、函数返回 `NULL`，下游继续崩；
  改为 `switch()` + 明确报错。
* `get_symmetric_val()`：原实现往矩阵里写占位字符串 `"heiheihei"` 再筛掉，会顺带
  把矩阵变成字符型；改为按三角索引直接取 `from`/`to`/`value`，保留行列名。
* `bedMatTolong()`：缺少数值列时抛出费解的 `replacement has 0 rows`；
  现在显式校验并报错，`seq_len()` 取代 `1:x`。
* `facet_sector()`：位置索引 `data[,2]`、用 `unique()` 估角度、单扇区时
  `1:(n-1)` 越界、`names()` 赋空；已重写并补充参数校验，角度几何与原来一致。
* `addsuffix()`：`<= 10^3` 导致 1000 显示为 `1000bp`；补充 `NA` 处理与数值保留。
* `addMarkers()`：`stop()` 无消息、`mark.label` 缺失时报错不清晰，已补。
* `newpage()`：新增 `width`/`height`/`circle`/`circle.gp` 参数（默认行为不变）。

## 布局修复：标签重叠与图形溢出

绘制 48 个图层的示例时发现两类系统性的版式问题，本版一并处理：

* **刻度标签重叠**：`arcAxisGrob()` 原来把 `grid.pretty()` 的每个分度都画出来，
  弧短或带宽窄时标签必然叠在一起（例如 24 条染色体的基因组图外圈会出现
  `0bp50Mb100Mb` 连成一片）。现在会根据可用弧长/带宽与实测文字尺寸自动抽稀，
  并且始终保留首尾两个标签。新增内部函数 `thin_label_index()`。
* **扇区标签压住坐标轴**：`strip.label` 与 x 轴标签画在同一半径上。新增
  `strip_label_r()`，当同一侧同时绘制坐标轴时，把分面标签再外推/内推一个
  `axis.space`，24 个 track 图层的 `label.r` 统一改为调用它。
* **拐角标签**：x 轴与 y 轴同时存在时，径向最外侧的标签会与 x 轴标签挤在扇区
  拐角。`arcAxisGrob()` 新增 `corner.trim`，`arcSectorGrob()` 在两条轴都绘制时
  自动开启。
* **色块溢出扇区**：`geom_tracktile()` / `geom_tracktile2()` 的绘制范围取自上游
  参数，而该参数往往只覆盖色块的**中心点**，导致两端的色块被线性外推到扇区之外。
  `GeomArctile` 现在改用将要绘制的矩形边界 `xmin/xmax/ymin/ymax` 作为范围
  （新增 `resize_scale_param()`，用户显式给出的范围只要装得下图形仍会保留），
  并新增 `tile.margin`（默认 0.03）留出内边距。实测两端色块由越界 -1.24°
  变为内缩 6.57°。

## 参照 circlize 的实现修正标签排布

轴标签与分面标签的处理方式现在与 `circlize` 保持一致：

* **首尾标签内移**：采用 `circlize::circos.axis()` 中 `labels.pos.adjust` 的做法——
  用实际文字宽度算出首/尾标签越过扇区边界的距离，沿弧挪进扇区内部，而不是把标签
  直接丢掉。新增参数 `arcAxisGrob(pos.adjust = TRUE)`；同时限制挪动量不超过与相邻
  标签之间的空隙，避免"挪着挪着撞上邻居"。
* **朝向命名对齐**：`circlize::circos.text()` 里 `srt` 的约定是
  `inside → θ-90`、`clockwise → θ`、`outside → θ+90`，并且 `niceFacing` 会在
  `θ∈[90,270)` 时翻转 180 度。`arcTextGrob(auto.fit = TRUE)` 的"切向 → 径向"
  切换用的就是这套映射（切向等价 `inside`，径向等价 `clockwise` + niceFacing 翻转），
  不再是自造的旋转角。
* **标签稀疏化**：circlize 靠 `labels.cex` / 手写 `major.at` 控制密度，本包在此之上
  增加了按实测文字宽度自动抽稀（`thin_label_index()`），并保留首尾；
  现在能否放下是直接由「可用弧长 ÷ 单个标签宽度」算出，而不是拍一个经验系数。

## 分面标签（strip label）排布

* **退化的刻度导致标签全部堆在扇区起点**：分面标签只传一个 `x`/`y` 值时，
  `range()` 退化成单点，`rescale()` 把所有标签都映射到扇区起点，因此
  `strip.label.pos = "bottom"` 时 7 个扇区的名字会挤在圆心附近。现在检测到
  退化尺度会对称展开，标签落到各自扇区的**中间**。
* **放不下时自动改排布**：新增 `arcTextGrob(auto.fit = TRUE)`。标签宽度超过所在
  弧长（×0.85）时自动转成**径向**排布，径向仍放不下再按比例缩小字号
  （最多缩到 55%）。扇区窄、名字长时不再互相压盖。

## 依赖与工程优化

* 移除 `DescTools`：内置 `bezier_points()` 计算三次贝塞尔曲线，与原
  `DrawBezier()` 数值一致（最大偏差 2.2e-16）。
* 移除 `tibble`、`lifecycle` 依赖；新增 `Suggests: quantreg`（加权箱线图才需要）。
* `NAMESPACE` 不再 `import(stats)`，消除 `replacing previous import
  'stats::filter' by 'dplyr::filter'` 警告。
* `utilities-ggplot2.R` 从 847 行精简到 336 行，删除从 ggplot2 复制来的死代码；
  尤其删除了会覆盖 base 的 `tolower()`/`toupper()`（包内任何 `tolower()` 调用都会
  直接 abort）。
* `check_number_decimal()` / `check_bool()` / `resolve_text_unit()` 不再依赖 ggplot2
  内部符号（`.pt`、`ffi_standalone_*` 原生例程），改为自带实现。
* `data_frame0()` 改用 `vctrs::data_frame()`，支持 `.size` 参数。
* `Depends: R (>= 4.1.0)`（`arcAxisGrob.R` 使用了原生管道 `|>`）。
* `data/tracks.svg` 移至 `inst/extdata/`（`data/` 只允许 R 数据文件）。
* 新增 `man/ggcirclize-ggproto.Rd`、`man/newpage.Rd`，为 `stat_*()` 补齐 `geom`/
  `coef`/`width`/`orientation` 参数文档。
* 新增 `tests/smoke.R`：把 63 个图层/grob/工具函数的渲染用例固化为回归测试，
  覆盖此前只在绘制阶段才暴露的错误（含 `geom_genomicNestedZoom()`、
  `geom_trackgenomiclink()`、`geom_trackhist()`）。

## 兼容性说明

* 图形外观除上述位置错误（`arcAxisGrob` 次刻度、多组箱线图顶点顺序）外保持不变。
* `geom_trackviolin(stat = "arcydensity")` 若曾显式传参，请改为 `stat = "ydensity"`。

## 文档：bookdown 手册迁移到 Sphinx + Read the Docs

* 原 bookdown 手册（<https://junjunlab.github.io/ggcirclize-manual/>）改写为
  **Sphinx + MyST Markdown + sphinx-book-theme**，源码放在包内 `docs/`，
  由 Read the Docs 自动构建：<https://ggcirclize.readthedocs.io/>。
* 章节结构保持不变（安装、快速开始、坐标系与分面、各图层族、基因组图层等），
  并补齐 0.0.3 新增参数（`link_pos`、`nested_fill`、`labels.facing` 等）的说明。
* 新增 `.readthedocs.yaml`（`sphinx.configuration: docs/conf.py`）与
  `docs/requirements.txt`；`.Rbuildignore` 排除 `^docs$` 与 `^\.readthedocs\.yaml$`，
  保证安装包体积不变。
* 启动信息 `R/zzz.R` 与 `DESCRIPTION` 的文档地址同步指向 Read the Docs。
