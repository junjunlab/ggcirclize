# ==============================================================================
# Note: The following codes all origin from ggplot2 package
# ==============================================================================


#' @noRd
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}


#' @noRd
defaults <- function(x, y) c(x, y[setdiff(names(y), names(x))])

#' @noRd
data_frame0 <- function(...) {tibble::tibble(..., .name_repair = "minimal")}

#' @noRd
unique0 <- function(x, ...) if (is.null(x)) x else vctrs::vec_unique(x, ...)

deprecate_soft0 <- function(..., user_env = NULL) {
  user_env <- user_env %||% getOption("ggplot2_plot_env") %||% caller_env(2)
  lifecycle::deprecate_soft(..., user_env = user_env)
}

#' @noRd
check_linewidth <- function(data, name) {
  if (is.null(data$linewidth) && !is.null(data$size)) {
    deprecate_soft0("3.4.0", I(paste0("Using the `size` aesthetic with ", name)), I("the `linewidth` aesthetic"))
    data$linewidth <- data$size
  }
  data
}



split_matrix <- function(x, col_names = colnames(x)) {
  force(col_names)
  x <- lapply(seq_len(ncol(x)), function(i) x[, i])
  if (!is.null(col_names)) names(x) <- col_names
  x
}


#' @noRd
ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

# Is a grouping available?
# (Will return TRUE if an explicit group or a discrete variable with only one
# level existed when add_group() was called.)

# of plyr::id() used by add_group()
NO_GROUP <- -1L

has_groups <- function(data) {
  # If no group aesthetic is specified, all values of the group column equal to
  # NO_GROUP. On the other hand, if a group aesthetic is specified, all values
  # are different from NO_GROUP (since they are a result of plyr::id()). NA is
  # returned for 0-row data frames.
  data$group[1L] != NO_GROUP
}


# Restart handler for using vec_rbind with mix of types
# Ordered is coerced to factor
# If a character vector is present the other is converted to character
with_ordered_restart <- function(expr, .call) {
  withCallingHandlers(
    expr,
    vctrs_error_incompatible_type = function(cnd) {
      x <- cnd[["x"]]
      y <- cnd[["y"]]

      class_x <- class(x)[1]
      class_y <- class(y)[1]

      restart <- FALSE

      if (is.ordered(x) || is.ordered(y)) {
        restart <- TRUE
        if (is.ordered(x)) {
          x <- factor(as.character(x), levels = levels(x))
        }
        if (is.ordered(y)) {
          y <- factor(as.character(y), levels = levels(y))
        }
      } else if (is.character(x) || is.character(y)) {
        restart <- TRUE
        if (is.character(x)) {
          y <- as.character(y)
        } else {
          x <- as.character(x)
        }
      } else if (is.factor(x) || is.factor(y)) {
        restart <- TRUE
        lev <- c()
        if (is.factor(x)) {
          lev <- c(lev, levels(x))
        }
        if (is.factor(y)) {
          lev <- c(lev, levels(y))
        }
        x <- factor(as.character(x), levels = unique(lev))
        y <- factor(as.character(y), levels = unique(lev))
      }

      # Don't recurse and let ptype2 error keep its course
      if (!restart) {
        return(zap())
      }

      msg <- paste0("Combining variables of class <", class_x, "> and <", class_y, ">")
      desc <- paste0(
        "Please ensure your variables are compatible before plotting (location: ",
        format_error_call(.call),
        ")"
      )

      deprecate_soft0(
        "3.4.0",
        I(msg),
        details = desc
      )

      x_arg <- cnd[["x_arg"]]
      y_arg <- cnd[["y_arg"]]
      call <- cnd[["call"]]

      # Recurse with factor methods and restart with the result
      if (inherits(cnd, "vctrs_error_ptype2")) {
        out <- vec_ptype2(x, y, x_arg = x_arg, y_arg = y_arg, call = call)
        restart <- "vctrs_restart_ptype2"
      } else if (inherits(cnd, "vctrs_error_cast")) {
        out <- vec_cast(x, y, x_arg = x_arg, to_arg = y_arg, call = call)
        restart <- "vctrs_restart_cast"
      } else {
        return(zap())
      }

      # Old-R compat for `tryInvokeRestart()`
      try_restart <- function(restart, ...) {
        if (!is_null(findRestart(restart))) {
          invokeRestart(restart, ...)
        }
      }
      try_restart(restart, out)
    }
  )
}


#' @noRd
check_linewidth <- function(data, name) {
  if (is.null(data$linewidth) && !is.null(data$size)) {
    deprecate_soft0("3.4.0", I(paste0("Using the `size` aesthetic with ", name)), I("the `linewidth` aesthetic"))
    data$linewidth <- data$size
  }
  data
}

# Restart handler for using vec_rbind with mix of types
# Ordered is coerced to factor
# If a character vector is present the other is converted to character
with_ordered_restart <- function(expr, .call) {
  withCallingHandlers(
    expr,
    vctrs_error_incompatible_type = function(cnd) {
      x <- cnd[["x"]]
      y <- cnd[["y"]]

      class_x <- class(x)[1]
      class_y <- class(y)[1]

      restart <- FALSE

      if (is.ordered(x) || is.ordered(y)) {
        restart <- TRUE
        if (is.ordered(x)) {
          x <- factor(as.character(x), levels = levels(x))
        }
        if (is.ordered(y)) {
          y <- factor(as.character(y), levels = levels(y))
        }
      } else if (is.character(x) || is.character(y)) {
        restart <- TRUE
        if (is.character(x)) {
          y <- as.character(y)
        } else {
          x <- as.character(x)
        }
      } else if (is.factor(x) || is.factor(y)) {
        restart <- TRUE
        lev <- c()
        if (is.factor(x)) {
          lev <- c(lev, levels(x))
        }
        if (is.factor(y)) {
          lev <- c(lev, levels(y))
        }
        x <- factor(as.character(x), levels = unique(lev))
        y <- factor(as.character(y), levels = unique(lev))
      }

      # Don't recurse and let ptype2 error keep its course
      if (!restart) {
        return(zap())
      }

      msg <- paste0("Combining variables of class <", class_x, "> and <", class_y, ">")
      desc <- paste0(
        "Please ensure your variables are compatible before plotting (location: ",
        format_error_call(.call),
        ")"
      )

      deprecate_soft0(
        "3.4.0",
        I(msg),
        details = desc
      )

      x_arg <- cnd[["x_arg"]]
      y_arg <- cnd[["y_arg"]]
      call <- cnd[["call"]]

      # Recurse with factor methods and restart with the result
      if (inherits(cnd, "vctrs_error_ptype2")) {
        out <- vec_ptype2(x, y, x_arg = x_arg, y_arg = y_arg, call = call)
        restart <- "vctrs_restart_ptype2"
      } else if (inherits(cnd, "vctrs_error_cast")) {
        out <- vec_cast(x, y, x_arg = x_arg, to_arg = y_arg, call = call)
        restart <- "vctrs_restart_cast"
      } else {
        return(zap())
      }

      # Old-R compat for `tryInvokeRestart()`
      try_restart <- function(restart, ...) {
        if (!is_null(findRestart(restart))) {
          invokeRestart(restart, ...)
        }
      }
      try_restart(restart, out)
    }
  )
}

#' @noRd
vec_rbind0 <- function(..., .error_call = current_env(), .call = caller_env()) {
  with_ordered_restart(
    vctrs::vec_rbind(..., .error_call = .error_call),
    .call
  )
}


#' Apply function to unique subsets of a data.frame
#'
#' This function is akin to `plyr::ddply`. It takes a single data.frame,
#' splits it by the unique combinations of the columns given in `by`, apply a
#' function to each split, and then reassembles the results into a sigle
#' data.frame again.
#'
#' @param df A data.frame
#' @param by A character vector of column names to split by
#' @param fun A function to apply to each split
#' @param ... Further arguments to `fun`
#' @param drop Should unused factor levels in the columns given in `by` be
#' dropped.
#'
#' @return A data.frame if the result of `fun` does not include the columns
#' given in `by` these will be prepended to the result.
#'
#' @keywords internal
#' @noRd
dapply <- function(df, by, fun, ..., drop = TRUE) {
  grouping_cols <- .subset(df, by)
  fallback_order <- unique0(c(by, names(df)))
  apply_fun <- function(x) {
    res <- fun(x, ...)
    if (is.null(res)) return(res)
    if (length(res) == 0) return(data_frame0())
    vars <- lapply(setNames(by, by), function(col) .subset2(x, col)[1])
    if (is.matrix(res)) res <- split_matrix(res)
    if (is.null(names(res))) names(res) <- paste0("V", seq_along(res))
    if (all(by %in% names(res))) return(data_frame0(!!!unclass(res)))
    res <- modify_list(unclass(vars), unclass(res))
    res <- res[intersect(c(fallback_order, names(res)), names(res))]
    data_frame0(!!!res)
  }

  # Shortcut when only one group
  if (all(vapply(grouping_cols, single_value, logical(1)))) {
    return(apply_fun(df))
  }

  ids <- vctrs::vec_group_id(grouping_cols)
  group_rows <- split_with_index(seq_len(nrow(df)), ids)
  result <- lapply(seq_along(group_rows), function(i) {
    cur_data <- df_rows(df, group_rows[[i]])
    apply_fun(cur_data)
  })
  vec_rbind0(!!!result)
}


#' @noRd
single_value <- function(x, ...) {
  UseMethod("single_value")
}

#' @noRd
single_value.default <- function(x, ...) {
  # This is set by id() used in creating the grouping var
  identical(attr(x, "n"), 1L)
}


#' @noRd
single_value.factor <- function(x, ...) {
  # Panels are encoded as factor numbers and can never be missing (NA)
  identical(levels(x), "1")
}

#' @noRd
split_with_index <- function(x, f, n = max(f)) {
  if (n == 1) return(list(x))
  f <- as.integer(f)
  attributes(f) <- list(levels = as.character(seq_len(n)), class = "factor")
  unname(split(x, f))
}

#' @noRd
df_col <- function(x, name) .subset2(x, name)

#' @noRd
df_rows <- function(x, i) {
  cols <- lapply(x, `[`, i = i)
  data_frame0(!!!cols, .size = length(i))
}

#' @noRd
# More performant modifyList without recursion
modify_list <- function(old, new) {
  for (i in names(new)) old[[i]] <- new[[i]]
  old
}

# Use chartr() for safety since toupper() fails to convert i to I in Turkish locale
lower_ascii <- "abcdefghijklmnopqrstuvwxyz"
upper_ascii <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
to_lower_ascii <- function(x) chartr(upper_ascii, lower_ascii, x)
to_upper_ascii <- function(x) chartr(lower_ascii, upper_ascii, x)

tolower <- function(x) {
  cli::cli_abort("Please use {.fn to_lower_ascii}, which works fine in all locales.")
}

toupper <- function(x) {
  cli::cli_abort("Please use {.fn to_upper_ascii}, which works fine in all locales.")
}

snakeize <- function(x) {
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  to_lower_ascii(x)
}

snake_class <- function(x) {
  snakeize(class(x)[1])
}

# Is a grouping available?
# (Will return TRUE if an explicit group or a discrete variable with only one
# level existed when add_group() was called.)
has_groups <- function(data) {
  # If no group aesthetic is specified, all values of the group column equal to
  # NO_GROUP. On the other hand, if a group aesthetic is specified, all values
  # are different from NO_GROUP (since they are a result of plyr::id()). NA is
  # returned for 0-row data frames.
  data$group[1L] != NO_GROUP
}


#' Format 'Computed variables' section
#'
#' This is a helper function that helps format the 'Computed variables' section
#' of stat documentation pages. Briefly, it points to the delayed evaluation
#' documentation and it wraps the variable names in
#' 'after_stat()'.
#'
#' @param ... Named variable - description pairs. Variable names get wrapped in
#'   `after_stat()`. Variable names may be of the form "x|y" or "x,y" to format
#'   as "`after_stat(x)` or `after_stat(y)`" and
#'   "`after_stat(x)`, `after_stat(y)`" respectively.
#' @param .details Additional details to include in the preamble.
#' @param .skip_intro A logical, which if `TRUE` omits the link to the delayed
#'   evaluation docs. Can be useful when documenting two stat functions on the
#'   same page with different arguments, but no need to repeat the introduction
#'   in the second stat function (see e.g. `?stat_qq`).
#'
#' @return Formatted code that can be inserted into a .rd file.
#' @keywords internal
#' @noRd
#' @examples
#' rd_computed_vars(
#'   .details = "`stat_foobar()` computes the following variables:",
#'   "foo|bar" = "foobar",
#'   "bar,qux" = "quux",
#'   corge = "grault"
#' )
rd_computed_vars <- function(..., .details = "", .skip_intro = FALSE) {
  args  <- list(...)
  items <- names(args)
  descr <- unname(args)

  # Format preamble
  header <- "@section Computed variables: "
  intro  <- paste0(
    "These are calculated by the 'stat' part of layers and can be accessed ",
    "with [delayed evaluation][aes_eval]. "
  )
  if (.skip_intro) intro <- ""
  preamble <- c(header, paste0(intro, gsub("\n", "", .details)))

  # Format items
  fmt_items <- gsub(",", ")`, `after_stat(", items, fixed = TRUE)
  fmt_items <- gsub("|", ")` *or* `after_stat(",
                    fmt_items, fixed = TRUE)
  fmt_items <- paste0("*  `after_stat(", fmt_items, ")`")

  # Compose item-list
  fmt_descr <- gsub("\n", "", descr)
  fmt_list  <- paste(fmt_items, fmt_descr, sep = "\\cr ")

  c(preamble, fmt_list)
}


IS_NUMBER_true <- 0
IS_NUMBER_false <- 1
IS_NUMBER_oob <- 2

check_number_decimal <- function(x,
                                 ...,
                                 min = NULL,
                                 max = NULL,
                                 allow_infinite = TRUE,
                                 allow_na = FALSE,
                                 allow_null = FALSE,
                                 arg = caller_arg(x),
                                 call = caller_env()) {
  if (missing(x)) {
    exit_code <- IS_NUMBER_false
  } else if (0 == (exit_code <- .standalone_types_check_dot_call(
    ffi_standalone_check_number_1.0.7,
    x,
    allow_decimal = TRUE,
    min,
    max,
    allow_infinite,
    allow_na,
    allow_null
  ))) {
    return(invisible(NULL))
  }

  .stop_not_number(
    x,
    ...,
    exit_code = exit_code,
    allow_decimal = TRUE,
    min = min,
    max = max,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}


.standalone_types_check_dot_call <- .Call

check_bool <- function(x,
                       ...,
                       allow_na = FALSE,
                       allow_null = FALSE,
                       arg = caller_arg(x),
                       call = caller_env()) {
  if (!missing(x) && .standalone_types_check_dot_call(ffi_standalone_is_bool_1.0.7, x, allow_na, allow_null)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    c("`TRUE`", "`FALSE`"),
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}


.stop_not_number <- function(x,
                             ...,
                             exit_code,
                             allow_decimal,
                             min,
                             max,
                             allow_na,
                             allow_null,
                             arg,
                             call) {
  if (allow_decimal) {
    what <- "a number"
  } else {
    what <- "a whole number"
  }

  if (exit_code == IS_NUMBER_oob) {
    min <- min %||% -Inf
    max <- max %||% Inf

    if (min > -Inf && max < Inf) {
      what <- sprintf("%s between %s and %s", what, min, max)
    } else if (x < min) {
      what <- sprintf("%s larger than or equal to %s", what, min)
    } else if (x > max) {
      what <- sprintf("%s smaller than or equal to %s", what, max)
    } else {
      abort("Unexpected state in OOB check", .internal = TRUE)
    }
  }

  stop_input_type(
    x,
    what,
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}


#' @param x The object type which does not conform to `what`. Its
#'   `obj_type_friendly()` is taken and mentioned in the error message.
#' @param what The friendly expected type as a string. Can be a
#'   character vector of expected types, in which case the error
#'   message mentions all of them in an "or" enumeration.
#' @param show_value Passed to `value` argument of `obj_type_friendly()`.
#' @param ... Arguments passed to [abort()].
#' @inheritParams args_error_context
#' @noRd
stop_input_type <- function(x,
                            what,
                            ...,
                            allow_na = FALSE,
                            allow_null = FALSE,
                            show_value = TRUE,
                            arg = caller_arg(x),
                            call = caller_env()) {
  # From standalone-cli.R
  cli <- env_get_list(
    nms = c("format_arg", "format_code"),
    last = topenv(),
    default = function(x) sprintf("`%s`", x),
    inherit = TRUE
  )

  if (allow_na) {
    what <- c(what, cli$format_code("NA"))
  }
  if (allow_null) {
    what <- c(what, cli$format_code("NULL"))
  }
  if (length(what)) {
    what <- oxford_comma(what)
  }

  message <- sprintf(
    "%s must be %s, not %s.",
    cli$format_arg(arg),
    what,
    obj_type_friendly(x, value = show_value)
  )

  abort(message, ..., call = call, arg = arg)
}


oxford_comma <- function(chr, sep = ", ", final = "or") {
  n <- length(chr)

  if (n < 2) {
    return(chr)
  }

  head <- chr[seq_len(n - 1)]
  last <- chr[n]

  head <- paste(head, collapse = sep)

  # Write a or b. But a, b, or c.
  if (n > 2) {
    paste0(head, sep, final, " ", last)
  } else {
    paste0(head, " ", final, " ", last)
  }
}



#' Return English-friendly type
#' @param x Any R object.
#' @param value Whether to describe the value of `x`. Special values
#'   like `NA` or `""` are always described.
#' @param length Whether to mention the length of vectors and lists.
#' @return A string describing the type. Starts with an indefinite
#'   article, e.g. "an integer vector".
#' @noRd
obj_type_friendly <- function(x, value = TRUE) {
  if (is_missing(x)) {
    return("absent")
  }

  if (is.object(x)) {
    if (inherits(x, "quosure")) {
      type <- "quosure"
    } else {
      type <- paste(class(x), collapse = "/")
    }
    return(sprintf("a <%s> object", type))
  }

  if (!is_vector(x)) {
    return(.rlang_as_friendly_type(typeof(x)))
  }

  n_dim <- length(dim(x))

  if (!n_dim) {
    if (!is_list(x) && length(x) == 1) {
      if (is_na(x)) {
        return(switch(
          typeof(x),
          logical = "`NA`",
          integer = "an integer `NA`",
          double =
            if (is.nan(x)) {
              "`NaN`"
            } else {
              "a numeric `NA`"
            },
          complex = "a complex `NA`",
          character = "a character `NA`",
          .rlang_stop_unexpected_typeof(x)
        ))
      }

      show_infinites <- function(x) {
        if (x > 0) {
          "`Inf`"
        } else {
          "`-Inf`"
        }
      }
      str_encode <- function(x, width = 30, ...) {
        if (nchar(x) > width) {
          x <- substr(x, 1, width - 3)
          x <- paste0(x, "...")
        }
        encodeString(x, ...)
      }

      if (value) {
        if (is.numeric(x) && is.infinite(x)) {
          return(show_infinites(x))
        }

        if (is.numeric(x) || is.complex(x)) {
          number <- as.character(round(x, 2))
          what <- if (is.complex(x)) "the complex number" else "the number"
          return(paste(what, number))
        }

        return(switch(
          typeof(x),
          logical = if (x) "`TRUE`" else "`FALSE`",
          character = {
            what <- if (nzchar(x)) "the string" else "the empty string"
            paste(what, str_encode(x, quote = "\""))
          },
          raw = paste("the raw value", as.character(x)),
          .rlang_stop_unexpected_typeof(x)
        ))
      }

      return(switch(
        typeof(x),
        logical = "a logical value",
        integer = "an integer",
        double = if (is.infinite(x)) show_infinites(x) else "a number",
        complex = "a complex number",
        character = if (nzchar(x)) "a string" else "\"\"",
        raw = "a raw value",
        .rlang_stop_unexpected_typeof(x)
      ))
    }

    if (length(x) == 0) {
      return(switch(
        typeof(x),
        logical = "an empty logical vector",
        integer = "an empty integer vector",
        double = "an empty numeric vector",
        complex = "an empty complex vector",
        character = "an empty character vector",
        raw = "an empty raw vector",
        list = "an empty list",
        .rlang_stop_unexpected_typeof(x)
      ))
    }
  }

  vec_type_friendly(x)
}


.rlang_as_friendly_type <- function(type) {
  switch(
    type,

    list = "a list",

    NULL = "`NULL`",
    environment = "an environment",
    externalptr = "a pointer",
    weakref = "a weak reference",
    S4 = "an S4 object",

    name = ,
    symbol = "a symbol",
    language = "a call",
    pairlist = "a pairlist node",
    expression = "an expression vector",

    char = "an internal string",
    promise = "an internal promise",
    ... = "an internal dots object",
    any = "an internal `any` object",
    bytecode = "an internal bytecode object",

    primitive = ,
    builtin = ,
    special = "a primitive function",
    closure = "a function",

    type
  )
}


.rlang_stop_unexpected_typeof <- function(x, call = caller_env()) {
  abort(
    sprintf("Unexpected type <%s>.", typeof(x)),
    call = call
  )
}


vec_type_friendly <- function(x, length = FALSE) {
  if (!is_vector(x)) {
    abort("`x` must be a vector.")
  }
  type <- typeof(x)
  n_dim <- length(dim(x))

  add_length <- function(type) {
    if (length && !n_dim) {
      paste0(type, sprintf(" of length %s", length(x)))
    } else {
      type
    }
  }

  if (type == "list") {
    if (n_dim < 2) {
      return(add_length("a list"))
    } else if (is.data.frame(x)) {
      return("a data frame")
    } else if (n_dim == 2) {
      return("a list matrix")
    } else {
      return("a list array")
    }
  }

  type <- switch(
    type,
    logical = "a logical %s",
    integer = "an integer %s",
    numeric = ,
    double = "a double %s",
    complex = "a complex %s",
    character = "a character %s",
    raw = "a raw %s",
    type = paste0("a ", type, " %s")
  )

  if (n_dim < 2) {
    kind <- "vector"
  } else if (n_dim == 2) {
    kind <- "matrix"
  } else {
    kind <- "array"
  }
  out <- sprintf(type, kind)

  if (n_dim >= 2) {
    out
  } else {
    add_length(out)
  }
}
