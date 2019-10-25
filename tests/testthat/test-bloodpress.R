test_that("bloodpress_mean works", {
  
  dt <- data.frame(
    BloodPress_Diastolic_1 = c(80,32,66,NA),
    BloodPress_Diastolic_2 = c(58,45,NA,99),
    BloodPress_Systolic_1 = c(40,NA,80,120),
    BloodPress_Systolic_2 = c(NA, 65,45,100)
  )
  
  expect_equal(bloodpress_mean(dt, dplyr::contains("Diastolic")),
               c(69, 38.5, 66, 99)
  )
  expect_equal(bloodpress_mean(dt, dplyr::contains("Systolic")),
               c(40, 65, 62.5, 110)
  )
  
  expect_equal(bloodpress_mean(dt, dplyr::contains("Diastolic"), na.rm=FALSE),
               c(69, 38.5, NA, NA)
  )
  expect_equal(bloodpress_mean(dt, dplyr::contains("Systolic"), na.rm=FALSE),
               c(NA, NA, 62.5, 110)
  )
  
})


test_that("bloodpress_map works", {
  
  dt <- data.frame(
      diastolic = c(69, NA, 66, 99),
      systolic = c(40, 65, NA, 110)
    )
  
  
  expect_equal(bloodpress_map(dt$diastolic[1], dt$systolic[1]),
               59.33333, tolerance = .001)
  
  dt <- dt %>% 
    mutate(map = bloodpress_map(diastolic, systolic))
  
  expect_equal(dt$map,
               c(59.3333333333333, NA, NA, 102.666666666667
               )
  )

  
})