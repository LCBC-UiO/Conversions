test_that("calc_bmi2 works", {
  expect_equivalent(calc_bmi2(176, 72), 17.52067, tolerance = .0001)
  
  dt <- data.frame(height = c(172, 164, 192), 
                   weight = c(72, 45, 110))
  
  ret_dt <- mutate(dt, 
         bmi = calc_bmi(height, weight),
         bmi2 = calc_bmi2(height, weight))
  
  expect_equal(ret_dt$bmi,
               c(24.3374797187669, 16.7311124330756, 29.8394097222222))
  
  expect_equal(ret_dt$bmi2,
               c(18.5571543130536, 13.0648038462732, 21.5347390444807))
  
})
