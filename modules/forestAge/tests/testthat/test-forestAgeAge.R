test_that("test forestAgeAge function",{
  modulePath <- modulePath   # specify the modulePath
  ageMap <- raster(xmn = 50, xmx = 50 + 2*100,
                   ymn = 50, ymx = 50 + 2*100,
                   res = c(100, 100), val = c(50, 80, 190, 230))
  Fires <- setValues(ageMap,0)
  Fires[2] <- 1
  mySim <- simInit(times = list(start = 0, end = 1),
                   params = list(forestAge = list(returnInterval = 50)),
                   modules = list("forestAge"),
                   objects = list("ageMap" = ageMap,
                                  "Fires" = Fires),
                   paths = list(modulePath = modulePath,
                                outputPath = "~/output"))
  output <- try(forestAgeAge(mySim))
  if(is(output, "try-error")){
    output <- mySim$forestAgeAge(mySim)
  }
  expect_equal(getValues(output$ageMap),c(100, 0, 200, 200))
})