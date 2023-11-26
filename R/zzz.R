.onAttach = function(libname, pkgname) {
  version = packageDescription(pkgname, fields = "Version")

  msg = paste0(
  "================================================================================
  ", pkgname, " version ", version, "

  Github page: https://github.com/junjunlab/ggcirclize
  Documentation: https://junjunlab.github.io/ggcirclize-manual/

  The ggcirclize is currently on developing.
  We strongly recommend that you do not rely on this for production, but,
  feel free to explore. If you encounter a clear bug, please file a
  minimal reproducible example at https://github.com/junjunlab/ggcirclize/issues.
  For questions and other discussion, please use,
  https://github.com/junjunlab/ggcirclize/issues.

  This message can be suppressed by:
  suppressPackageStartupMessages(library(ggcirclize))
================================================================================
  ")
  packageStartupMessage(msg)
}
