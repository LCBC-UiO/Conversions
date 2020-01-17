test_that("bmi_calc2 works", {
  expect_equivalent(bmi_calc2(176, 72), 17.52067, tolerance = .0001)
  expect_equivalent(bmi_calc(176, 72), 23.2438, tolerance = .0001)
  
  dt <- data.frame(height = c(172, 164, 192), 
                   weight = c(72, 45, 110))
  
  ret_dt <- mutate(dt, 
         bmi = bmi_calc(height, weight),
         bmi2 = bmi_calc2(height, weight))
  
  expect_equal(ret_dt$bmi,
               c(24.33, 16.731, 29.83940), 
               tolerance = .001)
  
  expect_equal(ret_dt$bmi2,
               c(18.557, 13.0648, 21.5347), 
               tolerance = .001)
  
})
