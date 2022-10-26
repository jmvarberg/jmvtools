test_that("Check that object is a datatable or coerrcible into one", {
    expect_error(jmv_datatables(ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) + ggplot2::geom_point()))
    expect_no_error(jmv_datatables(mtcars))

})
