iq_file <- paste0(test_path(),"/iq_table_subtest.tsv")

test_that("iq_table works", {
  t <- iq_table(iq_file, "Vocab", header=TRUE)
  
  expect_length(t, 4)
  expect_equal(nrow(t), 119)
  expect_equal(names(t), c("Age", "Subtest", "score", "raw_score"))
  expect_equal(unique(t$Subtest), "Vocabulary")
  
  t <- iq_table(iq_file, "M", header=TRUE)
  expect_length(t, 4)
  expect_equal(names(t), c("Age", "Subtest", "score", "raw_score"))
  expect_equal(unique(t$Subtest), "Matrix")
  
  expect_error(iq_table(iq_file, "q", header=TRUE), 
               "should be one of")
  
})

t <- iq_table(iq_file, "Vocab", header=TRUE)

test_that("iq_get works",{
  expect_equal(iq_get(33, 15.5, t), 31)
  expect_equal(iq_get(33, 20, t), NA_integer_)
  expect_equal(iq_get(34, 20, t), 27)
  expect_equal(class(iq_get(34, 20, t)), "numeric")

})


test_that("iq_convert works", {
  dt <- data.frame(
    wasi_vocab = c(33, 34, NA, 34),
    age = c(15.5, 20, 20, NA)
  )
  
  expect_equal(iq_raw2score(dt$wasi_vocab[1], dt$age[1], t), 31)
  expect_equal(iq_raw2score(dt$wasi_vocab, dt$age, t), c(31, 27, NA, NA))
  
})

test_that("iq_wppsi_adjust works", {
  expect_equal(iq_wppsi_adjust(10, 14), 36)
  
  dt <- data.frame(
    s1 = c(10,14,18),
    s2 = c(12, 11, 20)
  )
  
  dt2 <- mutate(dt, viq = iq_wppsi_adjust(s1, s2))
  expect_length(dt2,3)
  expect_equal(nrow(dt2), nrow(dt))
  expect_equal(dt2$viq, c(33, 38, 57))
})

test_that("iq_wppsi_fs works", {
  expect_equal(iq_wppsi_fs(10, 10), 10)
  expect_equal(iq_wppsi_fs(30, 20), 25)
})

test_that("iq_t2iq works", {
  expect_error(iq_t2iq(), "iq_table")
  
  dt <- data.frame(
    s1 = c(10,14,18),
    s2 = c(12, 11, 20),
    t1 = c(35, 42, 32),
    t2 = c(33, 40, 36)
  )
  
  t <- data.frame(
    raw = 60:85,
    converted = 90:115
  )
  
  expect_warning(iq_t2iq(dt, dplyr::starts_with("s"), t),
                 "outside the allowed range")
  
  expect_equal(iq_t2iq(dt, dplyr::starts_with("t"), t),
                 c(98L, 112L, 98L))
  
})


test_that("convert_t2iq works", {
  t <- data.frame(from = c(10,20,30),
                  to = c(40, 50, 60))
  expect_equal(convert_t2iq(c(10,30), t),
               c(40,60))
  
  tt <- expect_warning(convert_t2iq(c(10,30, 70), t),
                 "outside the allowed")
  expect_equal(tt, c(40,60,NA))
})