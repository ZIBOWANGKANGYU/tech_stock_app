library(shiny)
library(quantmod)
library(tibble)
library(tidyr)
library(ggplot2)
library(stringr)
library(shinytest2)
# This is the test file for the app.R file.
# I will use the shinytest2 package to test the app.
# The website here: https://rstudio.github.io/shinytest2/ for more information on how to use the shinytest2 package.
# The entire app is pasted below for reference: 

# Test that initially, only AAPL is selected

test_that("only AAPL is selected initially", {
  app <- shinytest2::AppDriver$new(stock_app)
  # Test that the selectInput has the correct value
  app$get_value(input = "stock") %>% expect_equal("AAPL")
})

# Test that initially, the stock_data table has the correct columns and rows
# Use app$get_value() to get the value of the stock_data table

test_that("stock_data table has correct columns and rows", {
  app <- shinytest2::AppDriver$new(stock_app)
  stock_data <- app$get_value(export = "stock_data")
  # Test that the stock_data table has the correct columns: "date", "close", "symbol"
  expect_true(all(c("date", "close", "symbol") %in% colnames(stock_data)))
  # Test that the stock_data table has the correct rows
  expect_true(nrow(stock_data) > 0)
})

# Test that initially, the stock_table table has the correct class, columns and rows
# Use app$get_value() to get the value of the stock_table table

test_that("stock_table table has correct class, columns and rows", {
  app <- shinytest2::AppDriver$new(stock_app)
  stock_table <- app$get_value(export = "stock_table_rv")
  # Test that the stock_table table has the correct class
  expect_true(is.data.frame(stock_table))
  # Test that the stock_table table has the correct columns: "date", "AAPL"
  expect_true(all(c("date", "AAPL") %in% colnames(stock_table)))
  # Test that the stock_table table has the correct rows
  expect_true(nrow(stock_table) > 0)
})

# Test that initially, the stock_plot plot has the correct class
# Use app$get_value() to get the value of the stock_plot plot

test_that("stock_plot plot has correct class", {
  app <- shinytest2::AppDriver$new(stock_app)
  stock_plot <- app$get_value(export = "stock_plot_rv")
  # Test that the stock_plot plot has the correct class
  expect_true(is.ggplot(stock_plot))
})

## We will select both "AAPL" and "GOOGL" and test that the stock_data table has the correct columns and rows
test_that("stock_data table has correct columns and rows for multiple stock symbols", {
  app <- shinytest2::AppDriver$new(stock_app)
  # Select both "AAPL" and "GOOGL"
  app$set_inputs(stock = c("AAPL", "GOOGL"))
  stock_data <- app$get_value(export = "stock_data")
  # Test that the stock_data table has the correct columns: "date", "close", "symbol"
  expect_true(all(c("date", "close", "symbol") %in% colnames(stock_data)))
  # Test that the "symbol" column has the correct values
  expect_true(all(stock_data$symbol %in% c("AAPL", "GOOGL")))
  # Test that the stock_data table has the correct rows
  expect_true(nrow(stock_data) > 0)
})

## We will select both "AAPL" and "GOOGL" and test that the stock_table table has the correct class, columns and rows

test_that("stock_table table has correct class, columns and rows for multiple stock symbols", {
  app <- shinytest2::AppDriver$new(stock_app)
  # Select both "AAPL" and "GOOGL"
  app$set_inputs(stock = c("AAPL", "GOOGL"))
  stock_table <- app$get_value(export = "stock_table_rv")
  # Test that the stock_table table has the correct class
  expect_true(is.data.frame(stock_table))
  # Test that the stock_table table has the correct columns: "date", "AAPL", "GOOGL"
  expect_true(all(c("date", "AAPL", "GOOGL") %in% colnames(stock_table)))
  # Test that the stock_table table has the correct rows
  expect_true(nrow(stock_table) > 0)
})

## We will select both "AAPL" and "GOOGL" and test that the stock_plot plot has the correct class

test_that("stock_plot plot has correct class for multiple stock symbols", {
  app <- shinytest2::AppDriver$new(stock_app)
  # Select both "AAPL" and "GOOGL"
  app$set_inputs(stock = c("AAPL", "GOOGL"))
  stock_plot <- app$get_value(export = "stock_plot_rv")
  # Test that the stock_plot plot has the correct class
  expect_true(is.ggplot(stock_plot))
})

## We will select both "AAPL" and "GOOGL" and test that the stock_plot plot has the correct x axis title: "Date"

test_that("stock_plot plot has correct x axis label for multiple stock symbols", {
  app <- shinytest2::AppDriver$new(stock_app)
  # Select both "AAPL" and "GOOGL"
  app$set_inputs(stock = c("AAPL", "GOOGL"))
  stock_plot <- app$get_value(export = "stock_plot_rv")
  # Test that the stock_plot plot has the correct x axis title
  # For any ggplot2 plot, the x axis title is stored ggplot_build(stock_plot)$plot$labels$x
  expect_equal(ggplot2::ggplot_build(stock_plot)$plot$labels$x, "Date")
})

## We will select both "AAPL" and "GOOGL" and test that the stock_plot plot has the correct y axis title: "Close Price"

test_that("stock_plot plot has correct y axis label for multiple stock symbols", {
  app <- shinytest2::AppDriver$new(stock_app)
  # Select both "AAPL" and "GOOGL"
  app$set_inputs(stock = c("AAPL", "GOOGL"))
  stock_plot <- app$get_value(export = "stock_plot_rv")
  # Test that the stock_plot plot has the correct y axis title
  # For any ggplot2 plot, the y axis title is stored ggplot_build(stock_plot)$plot$labels$y
  expect_equal(ggplot2::ggplot_build(stock_plot)$plot$labels$y, "Close Price")
})

## We will select both "AAPL" and "GOOGL" and test that the stock_plot plot has the correct title: "Stock Price Data"

test_that("stock_plot plot has correct title for multiple stock symbols", {
  app <- shinytest2::AppDriver$new(stock_app)
  # Select both "AAPL" and "GOOGL"
  app$set_inputs(stock = c("AAPL", "GOOGL"))
  stock_plot <- app$get_value(export = "stock_plot_rv")
  # Test that the stock_plot plot has the correct title
  # For any ggplot2 plot, the title is stored ggplot_build(stock_plot)$plot$labels$title
  expect_equal(ggplot2::ggplot_build(stock_plot)$plot$labels$title, "Stock Price Data")
})

## We will select both "AAPL" and "GOOGL" and test that the stock_plot plot has the correct legend title: "Stock Symbol"

test_that("stock_plot plot has correct legend title for multiple stock symbols", {
  app <- shinytest2::AppDriver$new(stock_app)
  # Select both "AAPL" and "GOOGL"
  app$set_inputs(stock = c("AAPL", "GOOGL"))
  stock_plot <- app$get_value(export = "stock_plot_rv")
  # Test that the stock_plot plot has the correct legend title
  # For any ggplot2 plot, the legend title is stored ggplot_build(stock_plot)$plot$labels$colour
  expect_equal(ggplot2::ggplot_build(stock_plot)$plot$labels$colour, "Stock Symbol")
})

## Now, we want to consolidate the tests above. 
## We will start the app only once, and run all test_that() functions sequentially.

app <- shinytest2::AppDriver$new(stock_app)

test_that("only AAPL is selected initially", {
  # Test that the selectInput has the correct value
  app$get_value(input = "stock") %>% expect_equal("AAPL")
})

test_that("stock_data table has correct columns and rows", {
  stock_data <- app$get_value(export = "stock_data")
  # Test that the stock_data table has the correct columns: "date", "close", "symbol"
  expect_true(all(c("date", "close", "symbol") %in% colnames(stock_data)))
  # Test that the stock_data table has the correct rows
  expect_true(nrow(stock_data) > 0)
})

test_that("stock_table table has correct class, columns and rows", {
  stock_table <- app$get_value(export = "stock_table_rv")
  # Test that the stock_table table has the correct class
  expect_true(is.data.frame(stock_table))
  # Test that the stock_table table has the correct columns: "date", "AAPL"
  expect_true(all(c("date", "AAPL") %in% colnames(stock_table)))
  # Test that the stock_table table has the correct rows
  expect_true(nrow(stock_table) > 0)
})

test_that("stock_plot plot has correct class", {
  stock_plot <- app$get_value(export = "stock_plot_rv")
  # Test that the stock_plot plot has the correct class
  expect_true(is.ggplot(stock_plot))
})

test_that("stock_data table has correct columns and rows for multiple stock symbols", {
  # Select both "AAPL" and "GOOGL"
  app$set_inputs(stock = c("AAPL", "GOOGL"))
  stock_data <- app$get_value(export = "stock_data")
  # Test that the stock_data table has the correct columns: "date", "close", "symbol"
  expect_true(all(c("date", "close", "symbol") %in% colnames(stock_data)))
  # Test that the "symbol" column has the correct values
  expect_true(all(stock_data$symbol %in% c("AAPL", "GOOGL")))
  # Test that the stock_data table has the correct rows
  expect_true(nrow(stock_data) > 0)
})

test_that("stock_table table has correct class, columns and rows for multiple stock symbols", {
  # Select both "AAPL" and "GOOGL"
  app$set_inputs(stock = c("AAPL", "GOOGL"))
  stock_table <- app$get_value(export = "stock_table_rv")
  # Test that the stock_table table has the correct class
  expect_true(is.data.frame(stock_table))
  # Test that the stock_table table has the correct columns: "date", "AAPL", "GOOGL"
  expect_true(all(c("date", "AAPL", "GOOGL") %in% colnames(stock_table)))
  # Test that the stock_table table has the correct rows
  expect_true(nrow(stock_table) > 0)
})

test_that("stock_plot plot has correct class for multiple stock symbols", {
  # Select both "AAPL" and "GOOGL"
  app$set_inputs(stock = c("AAPL", "GOOGL"))
  stock_plot <- app$get_value(export = "stock_plot_rv")
  # Test that the stock_plot plot has the correct class
  expect_true(is.ggplot(stock_plot))
})

test_that("stock_plot plot has correct x axis label for multiple stock symbols", {
  # Select both "AAPL" and "GOOGL"
  app$set_inputs(stock = c("AAPL", "GOOGL"))
  stock_plot <- app$get_value(export = "stock_plot_rv")
  # Test that the stock_plot plot has the correct x axis title
  # For any ggplot2 plot, the x axis title is stored ggplot_build(stock_plot)$plot$labels$x
  expect_equal(ggplot2::ggplot_build(stock_plot)$plot$labels$x, "Date")
})

test_that("stock_plot plot has correct y axis label for multiple stock symbols", {
  # Select both "AAPL" and "GOOGL"
  app$set_inputs(stock = c("AAPL", "GOOGL"))
  stock_plot <- app$get_value(export = "stock_plot_rv")
  # Test that the stock_plot plot has the correct y axis title
  # For any ggplot2 plot, the y axis title is stored ggplot_build(stock_plot)$plot$labels$y
  expect_equal(ggplot2::ggplot_build(stock_plot)$plot$labels$y, "Close Price")
})

test_that("stock_plot plot has correct title for multiple stock symbols", {
  # Select both "AAPL" and "GOOGL"
  app$set_inputs(stock = c("AAPL", "GOOGL"))
  stock_plot <- app$get_value(export = "stock_plot_rv")
  # Test that the stock_plot plot has the correct title
  # For any ggplot2 plot, the title is stored ggplot_build(stock_plot)$plot$labels$title
  expect_equal(ggplot2::ggplot_build(stock_plot)$plot$labels$title, "Stock Price Data")
})

test_that("stock_plot plot has correct legend title for multiple stock symbols", {
  # Select both "AAPL" and "GOOGL"
  app$set_inputs(stock = c("AAPL", "GOOGL"))
  stock_plot <- app$get_value(export = "stock_plot_rv")
  # Test that the stock_plot plot has the correct legend title
  # For any ggplot2 plot, the legend title is stored ggplot_build(stock_plot)$plot$labels$colour
  expect_equal(ggplot2::ggplot_build(stock_plot)$plot$labels$colour, "Stock Symbol")
})

# Stop the app
app$stop()