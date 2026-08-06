test_that("error if sheet is non-existant", {
  skip_if_not_installed("flextable")
  data("mtcars")

  ft <- flextable::flextable(mtcars)
  wb <- openxlsx2::wb_workbook()

  wb_add_flextable(
    wb = wb,
    ft = ft,
    sheet = "nonexistant",
    dims = "B4"
  ) |>
    expect_error()

  wb_apply_cell_styles(wb, "nonexistant", NULL) |>
    expect_error()
})

test_that("add fill", {
  skip_if_not_installed("flextable")
  data("mtcars")

  ft <- flextable::flextable(mtcars)
  wb <- openxlsx2::wb_workbook()$add_worksheet("fill")


  ft <- flextable::bg(ft,
    i = ~ am == 1,
    j = ~am,
    bg = "orange",
    part = "body"
  ) |>
    flextable::bg(
      i = ~ hp > 100,
      j = ~hp,
      bg = "red",
      part = "body"
    )


  wb <- wb_add_flextable(
    wb = wb,
    ft = ft,
    sheet = "fill",
    dims = "D5"
  )


  expect_true(wb$get_cell_style("fill", dims = "G6") !=
    wb$get_cell_style("fill", dims = "H6"))
  expect_true(wb$get_cell_style("fill", dims = "L6") !=
    wb$get_cell_style("fill", dims = "H6"))
  expect_true(wb$get_cell_style("fill", dims = "L6") !=
    wb$get_cell_style("fill", dims = "G6"))
})

test_that("rotated text keeps its rotation", {
  skip_if_not_installed("flextable")
  data("mtcars")

  ft <- flextable::flextable(head(mtcars)[, c("mpg", "cyl", "disp")]) |>
    flextable::rotate(j = "mpg", rotation = "btlr", part = "header") |>
    flextable::rotate(j = "cyl", rotation = "tbrl", part = "header")

  wb <- openxlsx2::wb_workbook()$add_worksheet("rotate")
  wb <- wb_add_flextable(
    wb = wb,
    ft = ft,
    sheet = "rotate",
    dims = "A1"
  )

  cell_xf <- function(dims) {
    style_id <- as.integer(wb$get_cell_style("rotate", dims = dims))
    wb$styles_mgr$styles$cellXfs[[style_id + 1L]]
  }

  expect_match(cell_xf("A1"), 'textRotation="90"', fixed = TRUE)
  expect_match(cell_xf("B1"), 'textRotation="180"', fixed = TRUE)
  expect_false(grepl("textRotation", cell_xf("C1"), fixed = TRUE))
})
