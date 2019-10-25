

test_that("iq_table works", {
  t <- iq_table("iq_table_subtest.tsv", "Vocab", header=TRUE)
  
  expect_length(t, 4)
  expect_equal(nrow(t), 66)
  expect_equal(names(t), c("Age", "Subtest", "score", "raw_score"))
  expect_equal(unique(t$Subtest), "Vocabulary")
  
  t <- iq_table("iq_table_subtest.tsv", "M", header=TRUE)
  expect_length(t, 4)
  expect_equal(names(t), c("Age", "Subtest", "score", "raw_score"))
  expect_equal(unique(t$Subtest), "Matrix")
  
  expect_error(iq_table("iq_table_subtest.tsv", "q", header=TRUE), 
               "should be one of")
})

test_that("iq_get works",{
  
  t <- iq_table("iq_table_subtest.tsv", "Vocab", header=TRUE)
  
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
  
  t <- iq_table("iq_table_subtest.tsv", "Vocab", header=TRUE)
  
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