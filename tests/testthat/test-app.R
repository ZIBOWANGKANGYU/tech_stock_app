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

# test_that("stock_table has the right HTML", {
#   app$set_inputs(stock = c("AAPL", "GOOGL"))
#   browser()
#   stock_table <- app$get_value(output = "stock_table")
#   stock_table
#   # "<table  class = 'table shiny-table table- spacing-s' style = 'width:auto;'>\n<thead> <tr> <th style='text-align: left;'> date </th> <th style='text-align: right;'> AAPL </th> <th style='text-align: right;'> GOOGL </th>  </tr> </thead> <tbody>\n  <tr> <td> 19844.00 </td> <td align=\"right\"> 169.30 </td> <td align=\"right\"> 163.86 </td> </tr>\n  <tr> <td> 19845.00 </td> <td align=\"right\"> 173.03 </td> <td align=\"right\"> 166.62 </td> </tr>\n  <tr> <td> 19846.00 </td> <td align=\"right\"> 183.38 </td> <td align=\"right\"> 167.24 </td> </tr>\n  <tr> <td> 19849.00 </td> <td align=\"right\"> 181.71 </td> <td align=\"right\"> 168.10 </td> </tr>\n  <tr> <td> 19850.00 </td> <td align=\"right\"> 182.40 </td> <td align=\"right\"> 171.25 </td> </tr>\n  <tr> <td> 19851.00 </td> <td align=\"right\"> 182.74 </td> <td align=\"right\"> 169.38 </td> </tr>\n  <tr> <td> 19852.00 </td> <td align=\"right\"> 184.57 </td> <td align=\"right\"> 169.96 </td> </tr>\n  <tr> <td> 19853.00 </td> <td align=\"right\"> 183.05 </td> <td align=\"right\"> 168.65 </td> </tr>\n  <tr> <td> 19856.00 </td> <td align=\"right\"> 186.28 </td> <td align=\"right\"> 169.14 </td> </tr>\n  <tr> <td> 19857.00 </td> <td align=\"right\"> 187.43 </td> <td align=\"right\"> 170.34 </td> </tr>\n  <tr> <td> 19858.00 </td> <td align=\"right\"> 189.72 </td> <td align=\"right\"> 172.51 </td> </tr>\n  <tr> <td> 19859.00 </td> <td align=\"right\"> 189.84 </td> <td align=\"right\"> 174.18 </td> </tr>\n  <tr> <td> 19860.00 </td> <td align=\"right\"> 189.87 </td> <td align=\"right\"> 176.06 </td> </tr>\n  <tr> <td> 19863.00 </td> <td align=\"right\"> 191.04 </td> <td align=\"right\"> 176.92 </td> </tr>\n  <tr> <td> 19864.00 </td> <td align=\"right\"> 192.35 </td> <td align=\"right\"> 177.85 </td> </tr>\n  <tr> <td> 19865.00 </td> <td align=\"right\"> 190.90 </td> <td align=\"right\"> 176.38 </td> </tr>\n  <tr> <td> 19866.00 </td> <td align=\"right\"> 186.88 </td> <td align=\"right\"> 173.55 </td> </tr>\n  <tr> <td> 19867.00 </td> <td align=\"right\"> 189.98 </td> <td align=\"right\"> 174.99 </td> </tr>\n  <tr> <td> 19871.00 </td> <td align=\"right\"> 189.99 </td> <td align=\"right\"> 176.40 </td> </tr>\n  <tr> <td> 19872.00 </td> <td align=\"right\"> 190.29 </td> <td align=\"right\"> 175.90 </td> </tr>\n  <tr> <td> 19873.00 </td> <td align=\"right\"> 191.29 </td> <td align=\"right\"> 172.11 </td> </tr>\n  <tr> <td> 19874.00 </td> <td align=\"right\"> 192.25 </td> <td align=\"right\"> 172.50 </td> </tr>\n  <tr> <td> 19877.00 </td> <td align=\"right\"> 194.03 </td> <td align=\"right\"> 173.17 </td> </tr>\n  <tr> <td> 19878.00 </td> <td align=\"right\"> 194.35 </td> <td align=\"right\"> 173.79 </td> </tr>\n  <tr> <td> 19879.00 </td> <td align=\"right\"> 195.87 </td> <td align=\"right\"> 175.41 </td> </tr>\n  <tr> <td> 19880.00 </td> <td align=\"right\"> 194.48 </td> <td align=\"right\"> 176.73 </td> </tr>\n  <tr> <td> 19881.00 </td> <td align=\"right\"> 196.89 </td> <td align=\"right\"> 174.46 </td> </tr>\n  <tr> <td> 19884.00 </td> <td align=\"right\"> 193.12 </td> <td align=\"right\"> 175.01 </td> </tr>\n  <tr> <td> 19885.00 </td> <td align=\"right\"> 207.15 </td> <td align=\"right\"> 176.62 </td> </tr>\n  <tr> <td> 19886.00 </td> <td align=\"right\"> 213.07 </td> <td align=\"right\"> 177.79 </td> </tr>\n  <tr> <td> 19887.00 </td> <td align=\"right\"> 214.24 </td> <td align=\"right\"> 175.16 </td> </tr>\n  <tr> <td> 19888.00 </td> <td align=\"right\"> 212.49 </td> <td align=\"right\"> 176.79 </td> </tr>\n  <tr> <td> 19891.00 </td> <td align=\"right\"> 216.67 </td> <td align=\"right\"> 177.24 </td> </tr>\n  <tr> <td> 19892.00 </td> <td align=\"right\"> 214.29 </td> <td align=\"right\"> 175.09 </td> </tr>\n  <tr> <td> 19894.00 </td> <td align=\"right\"> 209.68 </td> <td align=\"right\"> 176.30 </td> </tr>\n  <tr> <td> 19895.00 </td> <td align=\"right\"> 207.49 </td> <td align=\"right\"> 179.63 </td> </tr>\n  <tr> <td> 19898.00 </td> <td align=\"right\"> 208.14 </td> <td align=\"right\"> 179.22 </td> </tr>\n  <tr> <td> 19899.00 </td> <td align=\"right\"> 209.07 </td> <td align=\"right\"> 184.03 </td> </tr>\n  <tr> <td> 19900.00 </td> <td align=\"right\"> 213.25 </td> <td align=\"right\"> 183.88 </td> </tr>\n  <tr> <td> 19901.00 </td> <td align=\"right\"> 214.10 </td> <td align=\"right\"> 185.41 </td> </tr>\n  <tr> <td> 19902.00 </td> <td align=\"right\"> 210.62 </td> <td align=\"right\"> 182.15 </td> </tr>\n  <tr> <td> 19905.00 </td> <td align=\"right\"> 216.75 </td> <td align=\"right\"> 182.99 </td> </tr>\n  <tr> <td> 19906.00 </td> <td align=\"right\"> 220.27 </td> <td align=\"right\"> 185.24 </td> </tr>\n  <tr> <td> 19907.00 </td> <td align=\"right\"> 221.55 </td> <td align=\"right\"> 185.82 </td> </tr>\n  <tr> <td> 19909.00 </td> <td align=\"right\"> 226.34 </td> <td align=\"right\"> 190.60 </td> </tr>\n  <tr> <td> 19912.00 </td> <td align=\"right\"> 227.82 </td> <td align=\"right\"> 189.03 </td> </tr>\n  <tr> <td> 19913.00 </td> <td align=\"right\"> 228.68 </td> <td align=\"right\"> 188.98 </td> </tr>\n  <tr> <td> 19914.00 </td> <td align=\"right\"> 232.98 </td> <td align=\"right\"> 191.18 </td> </tr>\n  <tr> <td> 19915.00 </td> <td align=\"right\"> 227.57 </td> <td align=\"right\"> 185.57 </td> </tr>\n  <tr> <td> 19916.00 </td> <td align=\"right\"> 230.54 </td> <td align=\"right\"> 185.07 </td> </tr>\n   </tbody> </table>"
#   # Above is the expected HTML for the stock_table table. 
#   # However, the actual data and date of stock prices will change. 
#   # We need to create a function to transform the HTML into a format that can be compared.
#   # The function below will remove the date and close price values from the HTML.
#   # This will allow us to compare the HTML without worrying about the actual values.
#   
#   transform_html <- function(html) {
#     # Remove the date values
#     html <- gsub("<td> [0-9]+\\.[0-9]+ </td>", "<td> DATE </td>", html)
#     # Remove the close price values
#     html <- gsub("<td align=\"right\"> [0-9]+\\.[0-9]+ </td>", "<td align=\"right\"> CLOSE </td>", html)
#     return(html)
#   }
#   
#   transformed_html <- transform_html(stock_table)
#   
# })

# Stop the app
app$stop()
