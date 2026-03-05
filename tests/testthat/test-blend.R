modes <- list(
  multiply     = multiply,
  screen       = screen,
  darken       = darken,
  lighten      = lighten,
  overlay      = overlay,
  hard_light   = hard_light,
  soft_light   = soft_light,
  colour_burn  = colour_burn,
  colour_dodge = colour_dodge,
  difference   = difference,
  exclusion    = exclusion
)

is_parseable <- function(x) {
  tryCatch({ grDevices::col2rgb(x, alpha = TRUE); TRUE }, error = function(e) FALSE)
}

# ---------------------------------------------------------------------------
# Input type: named colour string
# ---------------------------------------------------------------------------

test_that("all blend modes work with named colour input", {
  for (nm in names(modes)) {
    fn <- modes[[nm]]

    result <- fn("steelblue", "steelblue")
    expect_type(result, "character")
    expect_length(result, 1L)
    expect_true(is_parseable(result),
                info = paste0("mode: ", nm, " — result not parseable"))

    result_self <- fn("steelblue")
    expect_identical(result, result_self,
                     info = paste0("mode: ", nm, " — self-blend differs from two-arg"))
  }
})

# ---------------------------------------------------------------------------
# Input type: hex colour (fully opaque)
# ---------------------------------------------------------------------------

test_that("all blend modes work with fully opaque hex input", {
  hex_a <- "#FFA600FF"
  hex_b <- "#4682B4FF"

  for (nm in names(modes)) {
    fn <- modes[[nm]]

    result <- fn(hex_a, hex_b)
    expect_type(result, "character")
    expect_length(result, 1L)
    expect_true(is_parseable(result),
                info = paste0("mode: ", nm, " — result not parseable"))

    result_self <- fn(hex_a)
    expect_true(is_parseable(result_self),
                info = paste0("mode: ", nm, " — self-blend not parseable"))
  }
})

# ---------------------------------------------------------------------------
# Input type: hex colour with partial alpha
# ---------------------------------------------------------------------------

test_that("all blend modes work with semi-transparent hex input", {
  hex_alpha_a <- "#FFA60080"
  hex_alpha_b <- "#4682B480"

  for (nm in names(modes)) {
    fn <- modes[[nm]]

    result <- fn(hex_alpha_a, hex_alpha_b)
    expect_type(result, "character")
    expect_length(result, 1L)

    parsed <- grDevices::col2rgb(result, alpha = TRUE)
    expect_true(
      parsed["alpha", 1] > 0L && parsed["alpha", 1] < 255L,
      info = paste0("mode: ", nm, " — expected partial alpha")
    )

    result_transparent <- fn("#FFA60000", "#4682B400")
    parsed_t <- grDevices::col2rgb(result_transparent, alpha = TRUE)
    expect_equal(
      as.integer(parsed_t["alpha", 1]), 0L,
      info = paste0("mode: ", nm, " — two transparent inputs should yield transparent output")
    )
  }
})

# ---------------------------------------------------------------------------
# Input type: colour object produced via grDevices::rgb()
# ---------------------------------------------------------------------------

test_that("all blend modes work with colour objects from grDevices::rgb()", {
  col_obj_a <- grDevices::rgb(1, 0.651, 0, 1)
  col_obj_b <- grDevices::rgb(0.275, 0.510, 0.706, 1)

  for (nm in names(modes)) {
    fn <- modes[[nm]]

    result <- fn(col_obj_a, col_obj_b)
    expect_type(result, "character")
    expect_length(result, 1L)
    expect_true(is_parseable(result),
                info = paste0("mode: ", nm, " — result not parseable"))

    col_obj_alpha <- grDevices::rgb(1, 0.651, 0, 0.5)
    result_alpha <- fn(col_obj_alpha, col_obj_b)
    expect_true(is_parseable(result_alpha),
                info = paste0("mode: ", nm, " — partial alpha result not parseable"))
  }
})

# ---------------------------------------------------------------------------
# Input type: saved multi-element character vector (e.g. jumble::jumble)
# ---------------------------------------------------------------------------

test_that("blend modes work with a saved multi-element colour vector", {
  pal_vec <- jumble::jumble
  n <- length(pal_vec)

  for (nm in names(modes)) {
    fn <- modes[[nm]]

    result_self <- fn(pal_vec)
    expect_type(result_self, "character")
    expect_length(result_self, n)
    expect_true(
      all(vapply(result_self, is_parseable, logical(1))),
      info = paste0("mode: ", nm, " — self-blend produced unparseable colour")
    )

    result_recycled <- fn(pal_vec, "#FFA600FF")
    expect_length(result_recycled, n)

    result_vec <- fn(pal_vec, rev(pal_vec))
    expect_length(result_vec, n)
    expect_true(
      all(vapply(result_vec, is_parseable, logical(1))),
      info = paste0("mode: ", nm, " — two-vector blend produced unparseable colour")
    )
  }
})

test_that("saved colour vector blended with a palette function returns a function", {
  pal_vec <- jumble::jumble
  pal_fn  <- scales::pal_hue()

  result <- multiply(pal_vec, pal_fn)
  expect_true(is.function(result))

  colours <- result(length(pal_vec))
  expect_length(colours, length(pal_vec))
  expect_true(all(vapply(colours, is_parseable, logical(1))))
})

# ---------------------------------------------------------------------------
# Input type: function factory (scales::pal_hue())
# ---------------------------------------------------------------------------

test_that("all blend modes return a function when given palette function inputs", {
  pal_a <- scales::pal_hue()
  pal_b <- scales::pal_brewer(palette = "Set2")

  for (nm in names(modes)) {
    fn <- modes[[nm]]

    result_fn <- fn(pal_a, pal_b)
    expect_true(is.function(result_fn),
                info = paste0("mode: ", nm, " — expected function return"))

    colours <- result_fn(5)
    expect_type(colours, "character")
    expect_length(colours, 5L)
    expect_true(
      all(vapply(colours, is_parseable, logical(1))),
      info = paste0("mode: ", nm, " — palette output contains unparseable colour")
    )

    result_self <- fn(pal_a)
    expect_true(is.function(result_self),
                info = paste0("mode: ", nm, " — self-blend should return function"))
    colours_self <- result_self(3)
    expect_length(colours_self, 3L)
  }
})

test_that("palette function blended with a fixed colour returns a function", {
  pal <- scales::pal_hue()
  col <- "#FFA600FF"

  result_fn <- multiply(pal, col)
  expect_true(is.function(result_fn))
  colours <- result_fn(4)
  expect_length(colours, 4L)
  expect_true(all(vapply(colours, is_parseable, logical(1))))

  result_fn2 <- screen(col, pal)
  expect_true(is.function(result_fn2))
  colours2 <- result_fn2(4)
  expect_length(colours2, 4L)
  expect_true(all(vapply(colours2, is_parseable, logical(1))))
})

# ---------------------------------------------------------------------------
# Vector inputs (length > 1)
# ---------------------------------------------------------------------------

test_that("blend modes handle colour vectors correctly", {
  cols_a <- c("steelblue", "#FFA600FF", "#FF000080", grDevices::rgb(0, 1, 0, 1))
  cols_b <- c("#4682B4FF", "tomato",    "#0000FF80", grDevices::rgb(1, 0, 1, 0.75))

  for (nm in c("multiply", "screen", "overlay")) {
    fn <- modes[[nm]]
    result <- fn(cols_a, cols_b)
    expect_length(result, 4L)
    expect_true(
      all(vapply(result, is_parseable, logical(1))),
      info = paste0("mode: ", nm, " — vector element not parseable")
    )
  }

  result_recycled <- multiply(cols_a, "#80808080")
  expect_length(result_recycled, 4L)
})

# ---------------------------------------------------------------------------
# Directional properties
# ---------------------------------------------------------------------------

test_that("multiply darkens relative to inputs", {
  grey      <- "#808080FF"
  result    <- multiply(grey, grey)
  orig_lum  <- mean(grDevices::col2rgb(grey)[1:3, ])
  blend_lum <- mean(grDevices::col2rgb(result)[1:3, ])
  expect_lt(blend_lum, orig_lum)
})

test_that("screen lightens relative to inputs", {
  grey      <- "#808080FF"
  result    <- screen(grey, grey)
  orig_lum  <- mean(grDevices::col2rgb(grey)[1:3, ])
  blend_lum <- mean(grDevices::col2rgb(result)[1:3, ])
  expect_gt(blend_lum, orig_lum)
})

test_that("darken produces values <= max of inputs per channel", {
  a      <- "#FFA600FF"
  b      <- "#4682B4FF"
  result <- darken(a, b)
  rgb_a  <- grDevices::col2rgb(a)
  rgb_b  <- grDevices::col2rgb(b)
  rgb_r  <- grDevices::col2rgb(result)
  expect_true(all(rgb_r[1:3, ] <= pmax(rgb_a[1:3, ], rgb_b[1:3, ])))
})

test_that("lighten produces values >= min of inputs per channel", {
  a      <- "#FFA600FF"
  b      <- "#4682B4FF"
  result <- lighten(a, b)
  rgb_a  <- grDevices::col2rgb(a)
  rgb_b  <- grDevices::col2rgb(b)
  rgb_r  <- grDevices::col2rgb(result)
  expect_true(all(rgb_r[1:3, ] >= pmin(rgb_a[1:3, ], rgb_b[1:3, ])))
})

test_that("difference of identical colours is black", {
  col    <- "#FFA600FF"
  result <- difference(col, col)
  rgb_r  <- grDevices::col2rgb(result)
  expect_equal(as.integer(rgb_r[1:3, ]), c(0L, 0L, 0L))
})

test_that("multiply with white is identity", {
  col    <- "#FFA600FF"
  result <- multiply(col, "#FFFFFFFF")
  expect_equal(
    grDevices::col2rgb(result, alpha = TRUE),
    grDevices::col2rgb(col,    alpha = TRUE)
  )
})

test_that("screen with black is identity", {
  col    <- "#FFA600FF"
  result <- screen(col, "#000000FF")
  expect_equal(
    grDevices::col2rgb(result, alpha = TRUE),
    grDevices::col2rgb(col,    alpha = TRUE)
  )
})

# ---------------------------------------------------------------------------
# Output channel range
# ---------------------------------------------------------------------------

test_that("all blend modes stay within [0, 255] for extreme inputs", {
  extremes <- c(
    "#000000FF", "#FFFFFFFF", "#FF000080", "#00FF0080",
    "steelblue", "#FFA600FF", grDevices::rgb(1, 0.5, 0, 0.25)
  )

  for (nm in names(modes)) {
    fn     <- modes[[nm]]
    result <- fn(extremes, rev(extremes))
    parsed <- grDevices::col2rgb(result, alpha = TRUE)
    expect_true(
      all(parsed >= 0L & parsed <= 255L),
      info = paste0("mode: ", nm, " — channel value out of [0, 255]")
    )
  }
})

# ---------------------------------------------------------------------------
# Error handling
# ---------------------------------------------------------------------------

test_that("blend errors with zero colour arguments", {
  expect_error(multiply(), "At least one colour argument is required")
})

test_that("blend errors with more than two colour arguments", {
  expect_error(multiply("steelblue", "tomato", "#FFA600FF"))
})

test_that("blend errors with NULL inputs", {
  expect_error(multiply(NULL, "steelblue"))
  expect_error(multiply("steelblue", NULL))
})

test_that("blend errors when passed a ggplot2 layer", {
  layer <- ggplot2::annotate("text", x = 1, y = 1, label = "test")
  expect_error(multiply(layer, "steelblue"), "cannot blend ggplot2 layers")
})
