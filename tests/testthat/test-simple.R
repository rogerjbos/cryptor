test_that("get_coingecko_ping works", {
  expect_equal(get_coingecko_ping(), "(V3) To the Moon!")
})

test_that("get_coingecko_map works", {
  expect_equal(ncol(get_coingecko_map()), 3)
})

test_that("get_coingecko_currencies works", {
  expect_gt(length(get_coingecko_currencies()), 60)
})

test_that("get_coingecko_price works", {
  expect_gt(get_coingecko_price("cardano"), 0)
  expect_gt(get_coingecko_price("bitcoin"), 0)
})

test_that("get_coingecko_ohlc works", {
  expect_gt(nrow(get_coingecko_ohlc("cardano", days = 30)), 27)
  expect_gt(nrow(get_coingecko_ohlc("bitcoin", days = "max")), 30)
})

test_that("get_coingecko_history works", {
  expect_gt(nrow(get_coingecko_history("cardano", days = 30)), 27)
  expect_gt(nrow(get_coingecko_history("bitcoin", days = "max")), 30)
})

test_that("get_coingecko_name works", {
  expect_gte(ncol(get_coingecko_name("luna")), 3)
  expect_gte(nrow(get_coingecko_name("luna")), 2)
  expect_gte(nrow(get_coingecko_name("LUNA")), 2)
})

test_that("get_coingecko_markets works", {
  expect_gt(nrow(get_coingecko_markets(ord = 'market_cap_desc')), 90)
  expect_gt(nrow(get_coingecko_markets(ord = 'gecko_desc')), 90)
})

test_that("get_coingecko_coins works", {
  expect_gt(ncol(get_coingecko_coins('bitcoin')$coins), 20)
})

test_that("get_coingecko_asset_platforms works", {
  expect_gt(nrow(get_coingecko_asset_platforms()), 60)
})

test_that("get_coingecko_asset_coin_categories works", {
  expect_gt(nrow(get_coingecko_coin_categories()), 60)
})

test_that("get_coingecko_defi works", {
  expect_gte(ncol(get_coingecko_defi()), 7)
})

test_that("get_coingecko_trending works", {
  expect_gt(nrow(get_coingecko_trending()), 5)
})

test_that("get_coingecko_global works", {
  expect_gt(ncol(get_coingecko_global()), 58)
})

test_that("get_coingecko_public_treasury works", {
  expect_gt(nrow(get_coingecko_public_treasury()), 6)
})

