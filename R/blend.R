# Shared internal: parse and validate ... into col, col2
.parse_blend_dots <- function(dots) {
  if (length(dots) > 0 &&
      requireNamespace("ggplot2", quietly = TRUE) &&
      ggplot2::is_layer(dots[[1]])) {
    rlang::abort("paletteblend cannot blend ggplot2 layers")
  }

  if (length(dots) == 0) {
    rlang::abort("At least one colour argument is required")
  } else if (length(dots) == 1) {
    col  <- dots[[1]]
    col2 <- col
  } else if (length(dots) == 2) {
    col  <- dots[[1]]
    col2 <- dots[[2]]
  } else {
    rlang::abort("blend functions accept at most 2 colour/palette arguments")
  }

  if (is.null(col) || is.null(col2)) {
    rlang::abort("colour/palette arguments cannot be NULL")
  }

  list(col = col, col2 = col2)
}

# Shared internal: apply Porter-Duff alpha compositing given a blend formula
#
# col1 = source (A), col2 = destination (B)
# aR = aA + aB·(1−aA)
# xR = [ (1−aB)·xaA + (1−aA)·xaB + aA·aB·f(xA,xB) ] / aR
#
# Reference: https://www.cairographics.org/operators/
.composite <- function(col1, col2, blend_fn) {
  len  <- max(length(col1), length(col2))
  col1 <- rep_len(col1, len)
  col2 <- rep_len(col2, len)

  rgb1 <- grDevices::col2rgb(col1, alpha = TRUE) / 255
  rgb2 <- grDevices::col2rgb(col2, alpha = TRUE) / 255

  alpha1  <- rgb1[4, ]
  alpha2  <- rgb2[4, ]
  alpha_r <- alpha1 + alpha2 * (1 - alpha1)

  idx <- alpha_r > 0

  rgb_r <- matrix(0, nrow = 3, ncol = len)
  for (i in 1:3) {
    c1 <- rgb1[i, ]
    c2 <- rgb2[i, ]
    f  <- blend_fn(c1, c2)

    result <- numeric(len)
    result[idx] <- (
      c1[idx] * alpha1[idx] * (1 - alpha2[idx]) +
        c2[idx] * alpha2[idx] * (1 - alpha1[idx]) +
        f[idx]  * alpha1[idx] * alpha2[idx]
    ) / alpha_r[idx]

    rgb_r[i, ] <- pmax(0, pmin(1, result))
  }

  alpha_r <- pmax(0, pmin(1, alpha_r))
  grDevices::rgb(rgb_r[1, ], rgb_r[2, ], rgb_r[3, ], alpha_r)
}

# Shared internal: resolve a palette function or character vector to colours
.resolve_pal <- function(pal, x) {
  if (!is.function(pal)) return(rep_len(pal, length(x)))
  tryCatch(
    pal(x),
    error = function(e) {
      if (length(x) > 1 && inherits(pal, "pal_discrete")) {
        n_colours <- attr(pal, "nlevels") %||% 256
        colours   <- pal(min(n_colours, 256))
        scales::pal_gradient_n(colours = colours)(x)
      } else if (length(x) == 1 && is.numeric(x)) {
        pal(seq(0, 1, length.out = x))
      } else {
        stop(e)
      }
    }
  )
}


#' Blend colours and palettes using multiply mode
#'
#' @description
#' Darkens colours by multiplying them together. Creates darker, more saturated
#' results. Useful for creating shadows, darkening backgrounds, or adding depth.
#'
#' @param ... Either one or two colour/palette arguments:
#'   - If one argument: the colour or palette is blended with itself
#'   - If two arguments: the first is blended with the second
#'   Each argument can be a character vector of colours or a `scales::pal_*()`
#'   function
#' @return Character vector of blended colours or a blending function.
#' @export
#'
#' @examples
#' multiply("#F0F0F0", "#808080")
#' multiply("#FF6B6B")
multiply <- function(...) {
  args <- .parse_blend_dots(list(...))
  col  <- args$col
  col2 <- args$col2

  blend_fn <- function(c1, c2) c1 * c2

  if (is.function(col) || is.function(col2)) {
    function(x) .composite(.resolve_pal(col, x), .resolve_pal(col2, x), blend_fn)
  } else {
    .composite(col, col2, blend_fn)
  }
}


#' Blend colours and palettes using screen mode
#'
#' @description
#' Lightens colours by inverting, multiplying, then inverting again. Creates
#' brighter results. Useful for creating highlights, lightening backgrounds, or
#' adding luminosity.
#'
#' @inheritParams multiply
#' @return Character vector of blended colours or a blending function.
#' @export
#'
#' @examples
#' screen("#2C2C2C", "#808080")
#' screen("#4A4A4A")
screen <- function(...) {
  args <- .parse_blend_dots(list(...))
  col  <- args$col
  col2 <- args$col2

  blend_fn <- function(c1, c2) c1 + c2 - c1 * c2

  if (is.function(col) || is.function(col2)) {
    function(x) .composite(.resolve_pal(col, x), .resolve_pal(col2, x), blend_fn)
  } else {
    .composite(col, col2, blend_fn)
  }
}


#' Blend colours and palettes using darken mode
#'
#' @description
#' Darkens colours by selecting the darker of two colour values for each RGB
#' channel. Useful for creating shadows or combining dark elements.
#'
#' @inheritParams multiply
#' @return Character vector of blended colours or a blending function.
#' @export
#'
#' @examples
#' darken("#FFA600FF", "#8991A1FF")
darken <- function(...) {
  args <- .parse_blend_dots(list(...))
  col  <- args$col
  col2 <- args$col2

  blend_fn <- function(c1, c2) pmin(c1, c2)

  if (is.function(col) || is.function(col2)) {
    function(x) .composite(.resolve_pal(col, x), .resolve_pal(col2, x), blend_fn)
  } else {
    .composite(col, col2, blend_fn)
  }
}


#' Blend colours and palettes using lighten mode
#'
#' @description
#' Lightens colours by selecting the lighter of two colour values for each RGB
#' channel. Useful for creating highlights or combining light elements.
#'
#' @inheritParams multiply
#' @return Character vector of blended colours or a blending function.
#' @export
#'
#' @examples
#' lighten("#FFA600FF", "#8991A1FF")
lighten <- function(...) {
  args <- .parse_blend_dots(list(...))
  col  <- args$col
  col2 <- args$col2

  blend_fn <- function(c1, c2) pmax(c1, c2)

  if (is.function(col) || is.function(col2)) {
    function(x) .composite(.resolve_pal(col, x), .resolve_pal(col2, x), blend_fn)
  } else {
    .composite(col, col2, blend_fn)
  }
}


#' Blend colours and palettes using overlay mode
#'
#' @description
#' Combines multiply and screen depending on the lightness of the second colour.
#' Values below 50% grey are multiplied (darkened); values above are screened
#' (lightened).
#'
#' @inheritParams multiply
#' @return Character vector of blended colours or a blending function.
#' @export
#'
#' @examples
#' overlay("#FFA600FF", "#8991A1FF")
overlay <- function(...) {
  args <- .parse_blend_dots(list(...))
  col  <- args$col
  col2 <- args$col2

  blend_fn <- function(c1, c2) {
    ifelse(c2 <= 0.5,
           2 * c1 * c2,
           1 - 2 * (1 - c1) * (1 - c2))
  }

  if (is.function(col) || is.function(col2)) {
    function(x) .composite(.resolve_pal(col, x), .resolve_pal(col2, x), blend_fn)
  } else {
    .composite(col, col2, blend_fn)
  }
}


#' Blend colours and palettes using hard light mode
#'
#' @description
#' Combines multiply and screen depending on the lightness of the first colour.
#' Like overlay but the first colour controls whether darkening or lightening
#' is applied.
#'
#' @inheritParams multiply
#' @return Character vector of blended colours or a blending function.
#' @export
#'
#' @examples
#' hard_light("#FFA600FF", "#8991A1FF")
hard_light <- function(...) {
  args <- .parse_blend_dots(list(...))
  col  <- args$col
  col2 <- args$col2

  blend_fn <- function(c1, c2) {
    ifelse(c1 <= 0.5,
           2 * c1 * c2,
           1 - 2 * (1 - c1) * (1 - c2))
  }

  if (is.function(col) || is.function(col2)) {
    function(x) .composite(.resolve_pal(col, x), .resolve_pal(col2, x), blend_fn)
  } else {
    .composite(col, col2, blend_fn)
  }
}


#' Blend colours and palettes using soft light mode
#'
#' @description
#' A softer version of hard light. Darkens or lightens depending on the first
#' colour, but with a gentler effect than hard light.
#'
#' @inheritParams multiply
#' @return Character vector of blended colours or a blending function.
#' @export
#'
#' @examples
#' soft_light("#FFA600FF", "#8991A1FF")
soft_light <- function(...) {
  args <- .parse_blend_dots(list(...))
  col  <- args$col
  col2 <- args$col2

  D <- function(x) ifelse(x <= 0.25, ((16 * x - 12) * x + 4) * x, sqrt(x))

  blend_fn <- function(c1, c2) {
    ifelse(c1 <= 0.5,
           c2 - (1 - 2 * c1) * c2 * (1 - c2),
           c2 + (2 * c1 - 1) * (D(c2) - c2))
  }

  if (is.function(col) || is.function(col2)) {
    function(x) .composite(.resolve_pal(col, x), .resolve_pal(col2, x), blend_fn)
  } else {
    .composite(col, col2, blend_fn)
  }
}


#' Blend colours and palettes using colour burn mode
#'
#' @description
#' Darkens the destination colour to reflect the source by increasing contrast.
#' Produces deep, saturated results.
#'
#' @inheritParams multiply
#' @return Character vector of blended colours or a blending function.
#' @export
#'
#' @examples
#' colour_burn("#FFA600FF", "#8991A1FF")
colour_burn <- function(...) {
  args <- .parse_blend_dots(list(...))
  col  <- args$col
  col2 <- args$col2

  blend_fn <- function(c1, c2) {
    ifelse(c1 == 0, 0, 1 - pmin(1, (1 - c2) / c1))
  }

  if (is.function(col) || is.function(col2)) {
    function(x) .composite(.resolve_pal(col, x), .resolve_pal(col2, x), blend_fn)
  } else {
    .composite(col, col2, blend_fn)
  }
}


#' Blend colours and palettes using colour dodge mode
#'
#' @description
#' Brightens the destination colour to reflect the source by decreasing
#' contrast. Produces bright, washed-out results.
#'
#' @inheritParams multiply
#' @return Character vector of blended colours or a blending function.
#' @export
#'
#' @examples
#' colour_dodge("#FFA600FF", "#8991A1FF")
colour_dodge <- function(...) {
  args <- .parse_blend_dots(list(...))
  col  <- args$col
  col2 <- args$col2

  blend_fn <- function(c1, c2) {
    ifelse(c1 >= 1, 1, pmin(1, c2 / (1 - c1)))
  }

  if (is.function(col) || is.function(col2)) {
    function(x) .composite(.resolve_pal(col, x), .resolve_pal(col2, x), blend_fn)
  } else {
    .composite(col, col2, blend_fn)
  }
}


#' Blend colours and palettes using difference mode
#'
#' @description
#' Subtracts the darker colour from the lighter. Identical colours produce
#' black; white inverts the other colour.
#'
#' @inheritParams multiply
#' @return Character vector of blended colours or a blending function.
#' @export
#'
#' @examples
#' difference("#FFA600FF", "#8991A1FF")
difference <- function(...) {
  args <- .parse_blend_dots(list(...))
  col  <- args$col
  col2 <- args$col2

  blend_fn <- function(c1, c2) abs(c1 - c2)

  if (is.function(col) || is.function(col2)) {
    function(x) .composite(.resolve_pal(col, x), .resolve_pal(col2, x), blend_fn)
  } else {
    .composite(col, col2, blend_fn)
  }
}


#' Blend colours and palettes using exclusion mode
#'
#' @description
#' Similar to difference but with lower contrast. Identical colours produce
#' grey rather than black.
#'
#' @inheritParams multiply
#' @return Character vector of blended colours or a blending function.
#' @export
#'
#' @examples
#' exclusion("#FFA600FF", "#8991A1FF")
exclusion <- function(...) {
  args <- .parse_blend_dots(list(...))
  col  <- args$col
  col2 <- args$col2

  blend_fn <- function(c1, c2) c1 + c2 - 2 * c1 * c2

  if (is.function(col) || is.function(col2)) {
    function(x) .composite(.resolve_pal(col, x), .resolve_pal(col2, x), blend_fn)
  } else {
    .composite(col, col2, blend_fn)
  }
}
