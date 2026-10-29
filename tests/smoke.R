# Regression smoke test for ggcirclize
#
# Every exported layer and grob is rendered once to a null device. This catches
# the kind of failures that only show up while a grob tree is being drawn
# (empty gpar elements, missing stats, wrong argument names, ...), which a
# plain `R CMD check` cannot detect.
#
# Nothing is asserted beyond "it draws without an error": the goal is coverage,
# not visual comparison.

suppressPackageStartupMessages({
  library(ggcirclize)
  library(ggplot2)
  library(grid)
  library(circlize)
})

failures <- character()
current <- ""

check <- function(name, expr) {
  current <<- name
  ok <- tryCatch({
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    force(expr)
    TRUE
  }, error = function(e) {
    failures <<- c(failures, paste0(name, ": ", conditionMessage(e)))
    FALSE
  })
  invisible(ok)
}

check_plot <- function(name, p) {
  check(name, print(p))
}

set.seed(1)

df <- data.frame(x = rep(c("a", "b", "c"), each = 4),
                 y = runif(12, 1, 10),
                 gp = rep(c("A", "B"), 6))

df2 <- data.frame(x = seq(5, 115, length.out = 12),
                  xmin = seq(0, 110, length.out = 12),
                  xmax = seq(10, 120, length.out = 12),
                  ymin = rep(0, 12),
                  ymax = runif(12, 1, 5),
                  y = runif(12, 1, 5),
                  gp = rep(c("A", "B"), 6))

bed <- circlize::generateRandomBed(nr = 80, nc = 1)
bed <- bed[bed$chr %in% paste0("chr", 1:6), ]
bed_t <- bed
bed_t$x <- 1
bed_t$y <- 1

data("hg19_chrom_info", package = "ggcirclize")
chrom_hg19 <- hg19_chrom_info$chromsize

# ---------------------------------------------------------------- low level
check("grob/arcRectGrob", {
  newpage()
  grid.draw(arcRectGrob(xmin = 0, xmax = 100, ymin = 0, ymax = 5))
})
check("grob/arcSectorGrob", {
  newpage()
  grid.draw(arcSectorGrob(start = 0, end = 270, r0 = 0.3, r1 = 0.8))
})
check("grob/arcArrowGrob", {
  newpage()
  grid.draw(arcArrowGrob(xmin = 10, xmax = 120, y = 3))
})
check("grob/arcPointGrob", {
  newpage()
  grid.draw(arcPointGrob(x = c(10, 50), y = c(1, 2)))
})
check("grob/arcTextGrob", {
  newpage()
  grid.draw(arcTextGrob(x = c(10, 50), y = c(1, 2), label = c("a", "b")))
})
check("grob/arcSegmentsGrob", {
  newpage()
  grid.draw(arcSegmentsGrob(xmin = c(10, 50), xmax = c(40, 90),
                            ymin = 0, ymax = 2))
})
check("grob/arcLinesGrob", {
  newpage()
  grid.draw(arcLinesGrob(x = c(10, 50), y = c(1, 2)))
})
check("grob/arcPolygonGrob", {
  newpage()
  grid.draw(arcPolygonGrob(x = c(10, 50, 90), y = c(1, 3, 1)))
})
check("grob/arcAxisGrob-x", {
  newpage()
  grid.draw(arcAxisGrob(start = 0, end = 180, axis.type = "x", pos = "top"))
})
check("grob/arcAxisGrob-y", {
  newpage()
  grid.draw(arcAxisGrob(start = 0, end = 180, axis.type = "y", pos = "left"))
})
check("grob/arcLinksGrob", {
  newpage()
  grid.draw(arcLinksGrob(start = c(10, 190), end = c(100, 260)))
})

# --------------------------------------------------------------------- geoms
check_plot("geom_arcpoint", ggcirclize(mtcars, aes(x = mpg, y = disp)) + geom_arcpoint())
check_plot("geom_arcline", ggcirclize(mtcars, aes(x = mpg, y = disp)) + geom_arcline())
check_plot("geom_arcbar", ggcirclize(mpg, aes(x = class)) + geom_arcbar())
check_plot("geom_arccol", ggcirclize(df, aes(x = x, y = y)) + geom_arccol())
check_plot("geom_arctile", ggcirclize(df, aes(x = x, y = y)) + geom_arctile())
check_plot("geom_arcrect",
           ggcirclize(df2, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) + geom_arcrect())
check_plot("geom_arcsector", ggcirclize(df, aes(x = x, y = y)) + geom_arcsector())
check_plot("geom_arctext", ggcirclize(df, aes(x = x, y = y, label = y)) + geom_arctext())
check_plot("geom_arcpolygon", ggcirclize(df, aes(x = x, y = y)) + geom_arcpolygon())
check_plot("geom_arcboxplot", ggcirclize(df, aes(x = x, y = y)) + geom_arcboxplot())
check_plot("geom_arcviolin", ggcirclize(df, aes(x = x, y = y)) + geom_arcviolin())
check_plot("geom_arcdensity", ggcirclize(mtcars, aes(x = mpg)) + geom_arcdensity())
check_plot("geom_archistogram", ggcirclize(mtcars, aes(x = mpg)) + geom_archistogram())
check_plot("geom_arcribbon",
           ggcirclize(df, aes(x = x, ymin = y - 1, ymax = y + 1)) + geom_arcribbon())
check_plot("geom_arcarea", ggcirclize(df, aes(x = x, y = y)) + geom_arcarea())
check_plot("geom_arcrossbar",
           ggcirclize(df2, aes(x = x, y = y, ymin = ymin, ymax = ymax)) + geom_arcrossbar())
check_plot("geom_arclinerange",
           ggcirclize(df, aes(x = x, ymin = y - 1, ymax = y + 1)) + geom_arclinerange())
check_plot("geom_arcsegment",
           ggcirclize(df2, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) + geom_arcsegment())
check_plot("geom_arcarrow",
           ggcirclize(df2, aes(xmin = xmin, xmax = xmax, y = y)) + geom_arcarrow())
check_plot("geom_arcpath", ggcirclize(df, aes(x = x, y = y)) + geom_arcpath())
check_plot("geom_cpoint", ggcirclize(df, aes(x = x, y = y)) + geom_cpoint())

check_plot("geom_trackbar",
           ggcirclize(mpg, aes(x = class, fill = drv, sector = drv)) + geom_trackbar())
check_plot("geom_trackbar-xy",
           ggcirclize(df, aes(x = x, y = y, fill = gp, sector = gp)) + geom_trackbar())
check_plot("geom_trackline",
           ggcirclize(df, aes(x = x, y = y, group = gp, sector = gp)) + geom_trackline())
check_plot("geom_trackpoint",
           ggcirclize(df, aes(x = x, y = y, sector = gp)) + geom_trackpoint())
check_plot("geom_trackrect",
           ggcirclize(df2, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                               sector = gp)) + geom_trackrect())
check_plot("geom_tracksector",
           ggcirclize(df, aes(x = x, y = y, sector = gp)) + geom_tracksector())
check_plot("geom_tracktile",
           ggcirclize(df, aes(x = x, y = y, sector = gp)) + geom_tracktile())
check_plot("geom_tracktile2",
           ggcirclize(df, aes(x = x, y = y, sector = gp)) + geom_tracktile2())
check_plot("geom_trackarea",
           ggcirclize(df, aes(x = x, y = y, sector = gp)) + geom_trackarea())
check_plot("geom_trackribbon",
           ggcirclize(df, aes(x = x, ymin = y - 1, ymax = y + 1, sector = gp)) + geom_trackribbon())
check_plot("geom_trackcol",
           ggcirclize(df, aes(x = x, y = y, fill = gp, sector = gp)) + geom_trackcol())
check_plot("geom_trackdensity",
           ggcirclize(df, aes(x = y, fill = gp, sector = gp)) + geom_trackdensity())
check_plot("geom_trackviolin",
           ggcirclize(df, aes(x = x, y = y, sector = gp)) + geom_trackviolin())
check_plot("geom_trackboxplot",
           ggcirclize(df, aes(x = x, y = y, sector = gp)) + geom_trackboxplot())

set.seed(111)
bed_nz <- circlize::generateRandomBed(nr = 300)
zoom_nz <- circlize::generateRandomBed(nr = 10)[, 1:3]

set.seed(123)
b1 <- circlize::generateRandomBed(nr = 100)[sample(100, 15), ]
b2 <- circlize::generateRandomBed(nr = 100)[sample(100, 15), ]
bed_link <- cbind(b1[, 1:3], b2[, 1:3])
colnames(bed_link) <- c("chr1", "start1", "end1", "chr2", "start2", "end2")
bed_link$group <- sample(LETTERS[1:4], nrow(bed_link), replace = TRUE)

gmap <- aes(end = 360, chr = chr, gstart = start, gend = end, value = value1)
gmap_lab <- aes(end = 360, r0 = 0.7, r1 = 0.7, chr = chr, gstart = start,
                gend = end, label = chr)

check_plot("geom_trackgenomicpoint",
           ggcirclize(bed, gmap) + geom_trackgenomicpoint(chrom_data = chrom_hg19))
check_plot("geom_trackgenomicline",
           ggcirclize(bed, gmap) + geom_trackgenomicline(chrom_data = chrom_hg19))
check_plot("geom_trackgenomicrect",
           ggcirclize(bed, gmap) + geom_trackgenomicrect(chrom_data = chrom_hg19))
check_plot("geom_trackgenomicarea",
           ggcirclize(bed, gmap) + geom_trackgenomicarea(chrom_data = chrom_hg19))
check_plot("geom_trackgenomictile",
           ggcirclize(bed_t, modifyList(gmap, aes(x = x, y = y))) +
             geom_trackgenomictile(chrom_data = chrom_hg19))
check_plot("geom_trackgenomictile2",
           ggcirclize(bed_t, modifyList(gmap, aes(x = x, y = y))) +
             geom_trackgenomictile2(chrom_data = chrom_hg19))
check_plot("geom_trackgenomiclabel",
           ggcirclize(bed, gmap_lab) + geom_trackgenomiclabel(chrom_data = chrom_hg19))
check_plot("geom_trackgenomiclabel2",
           ggcirclize(bed, gmap_lab) + geom_trackgenomiclabel2(chrom_data = chrom_hg19))
check_plot("geom_trackgenomicrainfall",
           ggcirclize(bed, gmap) + geom_trackgenomicrainfall(chrom_data = chrom_hg19))
check_plot("geom_trackgenomicdensity",
           ggcirclize(bed, gmap) + geom_trackgenomicdensity(chrom_data = chrom_hg19))

check_plot("geom_trackgenomiclink",
           ggcirclize(bed_link, aes(end = 360, chr0 = chr1, gstart0 = start1,
                                    chr1 = chr2, gstart1 = start2, colour = group)) +
             geom_trackgenomiclink(aes(r = 1), chrom_data = chrom_hg19))

check_plot("geom_trackhist",
           ggcirclize(mtcars, aes(x = mpg, fill = factor(cyl), sector = factor(cyl))) +
             geom_trackhist(binwidth = 2))

check_plot("geom_genomicNestedZoom",
           ggcirclize(bed_nz, aes(end = 360, r0 = 0.8, r1 = 1, chr = chr,
                                  gstart = start, gend = end, value = value1,
                                  genome = "hg19")) +
             geom_genomicNestedZoom(zoom_data = zoom_nz))
check_plot("geom_genomicNestedZoom-nested_fill",
           ggcirclize(bed_nz, aes(end = 360, r0 = 0.8, r1 = 1, chr = chr,
                                  gstart = start, gend = end, value = value1,
                                  genome = "hg19")) +
             geom_genomicNestedZoom(zoom_data = zoom_nz,
                                    nested_fill = "lightcoral",
                                    nested_col = "darkred"))

# chord diagram
mat <- matrix(sample(1:20, 36, replace = TRUE), 6, 6,
              dimnames = list(paste0("S", 1:6), paste0("E", 1:6)))
cdf <- data.frame(from = rep(rownames(mat), ncol(mat)),
                  to = rep(colnames(mat), each = nrow(mat)),
                  value = as.vector(mat))
check_plot("geom_chordDiagram",
           ggcirclize(cdf, aes(end = 360, r = 0.8, from = from, to = to, value = value)) +
             geom_chordDiagram())

# ------------------------------------------------------------------- helpers
check("utils/get_chrom_data", stopifnot(nrow(get_chrom_data("hg19")) > 0))
check("utils/get_chrom_data-error",
      stopifnot(inherits(try(get_chrom_data("foo"), silent = TRUE), "try-error")))
check("utils/facet_sector",
      facet_sector(data = transform(df, start = 0, end = 180), data.type = "other"))
check("utils/get_symmetric_val", stopifnot(nrow(get_symmetric_val(mat)) == 21))
check("utils/bedMatTolong",
      stopifnot(nrow(bedMatTolong(bed[, c("chr", "start", "end", "value1")])) > 0))

if (length(failures)) {
  stop("ggcirclize smoke test failed:\n", paste0(" * ", failures, collapse = "\n"),
       call. = FALSE)
}

cat("All ggcirclize smoke tests passed.\n")
