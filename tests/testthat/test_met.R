# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

library(eco)

csv3 <- system.file("extdata", "test_met_vsn3.csv", package="eco")

test_that("read.met() handles type=\"csv3\" files", {
    expect_silent(d <- read.met.csv2(csv3, encoding="UTF-8-BOM"))
    # Sort both because the ordering is different when done interactively
    # and in the test (for reasons I don't understand).
    expect_equal(sort(names(d@data)), sort(c(paste("Date/Time", "(LST)"),
                "dewPoint", "direction", "humidex", "humidity",
                "precipitation",  "pressure", "speed", "temperature",
                "time", paste("Time", "(LST)"), "u", "v", "visibility",
                "weather", "windChill")))

    expect_equal(d@metadata$latitude, 44.88)
    expect_equal(d@metadata$longitude, -63.51)
    expect_equal(d@metadata$station, "HALIFAX STANFIELD INT'L A")
    expect_equal(d@data$temperature, c(1.7, 1.9, 2, 2, 2.4))
    expect_equal(d@data$humidity, rep(100L, 5))
    expect_equal(d@data$speed, c(2.77777777777778, 2.77777777777778,
            3.05555555555556, 2.77777777777778, 3.61111111111111))
    expect_equal(d@data$u, c(2.12790123088605, 1.78552113801816,
            2.34069135397465, 1.38888888888889, 2.76627160015187))
    expect_equal(d@data$v, c(-1.78552113801816, -2.12790123088605,
            -1.96407325181998, -2.40562612162344, -2.32117747942361))
    expect_equal(d@data$time, as.POSIXct(c("2022-01-01 00:00:00",
                "2022-01-01 01:00:00", "2022-01-01 02:00:00",
                "2022-01-01 03:00:00", "2022-01-01 04:00:00"),
            tz="UTC"))
})


