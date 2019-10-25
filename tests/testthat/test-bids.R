test_that("bidsify works", {
  
  expect_equal( bidsify(1100300, 2), "sub-1100300_ses-02")
  expect_equal( class(bidsify(1100300, 2)), "character")
  expect_length(bidsify(1100300, 2), 1)
  
  expect_equal(bidsify(1100300, 2, "ousAvanto"), "sub-1100300_ses-02ousAvanto") 
  expect_error(bidsify(1100300, 2, "Avanto"), "should be one of")
  
  expect_equal(bidsify(1100300, 2, "ousAvanto", type = "folder"), "sub-1100300/ses-02ousAvanto") 
  
})
