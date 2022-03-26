setClass("eco", slots=c(metadata="list", data="list", processingLog="list"))

#' read met file
#' @param file etc
#' @param skip etc
#' @param encoding etc
read.met.csv2 <- function(file, skip=NULL, encoding="UTF-8-BOM")
{
    if (is.character(file)) {
        file <- file(file, "r", encoding=encoding)
        on.exit(close(file))
    }
    res <- new("eco")
    owarn <- options()$warn
    options(warn=-1)
    text <- readLines(file, 1, warn=FALSE)
    dataNames <- strsplit(gsub('"', '', text[1]), ",")[[1]]
    data <- read.csv(file, header=FALSE)
    options(warn=owarn)
    if ("Dew Point Temp (\u00B0C)" %in% dataNames) {
        res@metadata$units$dewPoint <- list(unit=expression(degree*C), scale="ITS-90")
        res@metadata$dataNamesOriginal$dewPoint <- "Dew Point Temp (\u00B0C)"
        dataNames[dataNames == "Dew Point Temp (\u00B0C)"] <- "dewPoint"
    }
    if ("Hmdx" %in% dataNames) {
        res@metadata$units$humidex <- list(unit=expression(degree*C), scale="ITS-90")
        res@metadata$dataNamesOriginal$humidex <- "Hmdx"
        dataNames[dataNames == "Hmdx"] <- "humidex"
    }
    if ("Longitude (x)" %in% dataNames) {
        res@metadata$dataNamesOriginal$longitude <- "Longitude (x)"
        dataNames[dataNames == "Longitude (x)"] <- "longitude"
    }
    if ("Latitude (y)" %in% dataNames) {
        res@metadata$dataNamesOriginal$latitude <- "Latitude (y)"
        dataNames[dataNames == "Latitude (y)"] <- "latitude"
    }
    if ("Precip. Amount (mm)" %in% dataNames) {
        res@metadata$units$precipitation <- list(unit=expression("mm"), scale="")
        res@metadata$dataNamesOriginal$precipitation <- "Precip. Amount (mm)"
        dataNames[dataNames == "Precip. Amount (mm)"] <- "precipitation"
    }
    if ("Rel Hum (%)" %in% dataNames) {
        res@metadata$units$humidity <- list(unit=expression("%"), scale="")
        res@metadata$dataNamesOriginal$humidity <- "Rel Hum (%)"
        dataNames[dataNames == "Rel Hum (%)"] <- "humidity"
    }
    if ("Station Name" %in% dataNames) {
        res@metadata$dataNamesOriginal$latitude <- "Station Name"
        dataNames[dataNames == "Station Name"] <- "station"
    }
    if ("Stn Press (kPa)" %in% dataNames) {
        res@metadata$units$pressure <- list(unit=expression(kPa), scale="")
        res@metadata$dataNamesOriginal$pressure <- "Stn Press (kPa)"
        dataNames[dataNames == "Stn Press (kPa)"] <- "pressure"
    }
    if ("Temp (\u00B0C)" %in% dataNames) {
        res@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
        res@metadata$dataNamesOriginal$temperature <- "Temp (\u00B0C)"
        dataNames[dataNames == "Temp (\u00B0C)"] <- "temperature"
    }
    if ("Mean Temp (\u00B0C)" %in% dataNames) {
        res@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
        res@metadata$dataNamesOriginal$temperature <- "Mean Temp (\u00B0C)"
        dataNames[dataNames == "Mean Temp (\u00B0C)"] <- "temperature"
    }
     if ("Visibility (km)" %in% dataNames) {
        res@metadata$units$visibility <- list(unit=expression(km), scale="")
        res@metadata$dataNamesOriginal$visibility <- "Visibility (km)"
        dataNames[dataNames == "Visibility (km)"] <- "visibility"
    }
    if ("Wind Chill" %in% dataNames) {
        res@metadata$units$windChill <- list(unit=expression(degree*C), scale="ITS-90")
        res@metadata$dataNamesOriginal$windChill <- "Wind Chill"
        dataNames[dataNames == "Wind Chill"] <- "windChill"
    }
    if ("Weather" %in% dataNames) {
        res@metadata$units$weather <- list(unit=expression(), scale="")
        res@metadata$dataNamesOriginal$weather <- "Weather"
        dataNames[dataNames == "Weather"] <- "weather"
    }
    ##> print(dataNames)
    ##> head(data)
    ##> str(data)
    ##> browser()
    names(data) <- dataNames
    ##> print("DANNY")
    ##> print(dataNames)
    res@data <- data
    ## climateIdentifier
    if ("Climate ID" %in% dataNames) {
        res@metadata$climateIdentifier <- data[["Climate ID"]][1]
        res@data[["Climate ID"]] <- NULL
    }
    ## dataNames <- names(data)
    nsamples <- dim(data)[1]
    ## Time
    if ("Time" %in% dataNames) {
        hour <- as.numeric(lapply(as.character(data$Time), function(x) strsplit(x, ":")[[1]][1]))
        minute <- as.numeric(lapply(as.character(data$Time), function(x) strsplit(x, ":")[[1]][2]))
    } else if ("Time (LST)" %in% dataNames) {
        hour <- as.numeric(lapply(as.character(data[["Time (LST)"]]), function(x) strsplit(x, ":")[[1]][1]))
        minute <- as.numeric(lapply(as.character(data[["Time (LST)"]]), function(x) strsplit(x, ":")[[1]][2]))
    } else {
        hour <- rep(0, nsamples)
        minute <- rep(0, nsamples)
    }
    second <- 0
    day <- if ("Day" %in% dataNames) data[["Day"]] else 1
    time <- ISOdatetime(data[["Year"]], data[["Month"]], day, hour, minute, second, tz="UTC")
    res@data$time <- time
    res@data[["Date/Time"]] <- NULL
    res@data[["Year"]] <- NULL
    res@data[["Month"]] <- NULL
    res@data[["Day"]] <- NULL
    res@data[["Time"]] <- NULL
    ## wind
    if ("Wind Spd (km/h)" %in% dataNames && "Wind Dir (10s deg)" %in% dataNames) {
        res@data$speed <- data[["Wind Spd (km/h)"]] * 1000 / 3600 # convert km/h to m/s
        res@metadata$dataNamesOriginal$speed <- "-"
        res@data[["Wind Spd (km/h)"]] <- NULL
        res@data$direction <- 10 * res@data[["Wind Dir (10s deg)"]] # convert 10s of degrees to degrees
        res@metadata$dataNamesOriginal$direction <- "-"
        res@data[["Wind Dir (10s deg)"]] <- NULL
        rpd <- atan2(1, 1) / 45            # radian/degree
        theta <- rpd * (90 - res@data$direction)
        ## Note the (-) to get from "wind from" to "wind speed towards"
        res@data$u <- -res@data$speed * sin(theta)
        res@data$v <- -res@data$speed * cos(theta)
        zero <- is.na(res@data$direction) & res@data$speed == 0
        res@data$u[zero] <- 0
        res@data$v[zero] <- 0
        res@metadata$units$direction  <- list(unit=expression(degree), scale="")
        res@metadata$units$speed <- list(unit=expression(m/s), scale="")
        res@metadata$units$u <- list(unit=expression(m/s), scale="")
        res@metadata$units$v <- list(unit=expression(m/s), scale="")
    }

    ## Move some things to metadata, if they are uni-valued. This is so
    ## code written for the csv1 style will work for csv2 style also.
    if (1 == length(unique(data$longitude))) {
        res@metadata$longitude <- data$longitude[1]
        res@data$longitude <- NULL
    }
    if (1 == length(unique(data$latitude))) {
        res@metadata$latitude <- data$latitude[1]
        res@data$latitude <- NULL
    }
    if (1 == length(unique(data$station))) {
        res@metadata$station <- as.character(data$station[1])
        res@data$station <- NULL
    }

    ## Flags
    res@metadata$flags <- list()
    knownFlags <- list(dewpoint="Dew Point Temp Flag",
                       humidex="Hmdx Flag",
                       direction="Wind Dir Flag",
                       humidity="Rel Hum Flag",
                       precipitation="Precip. Amount Flag",
                       pressure="Stn Press Flag",
                       speed="Wind Spd Flag",
                       temperature="Temp Flag",
                       visibility="Visibility Flag",
                       windChill="Wind Chill Flag"
                       )
    knownFlagNames <- names(knownFlags)
    for (iflag in seq_along(knownFlags)) {
        ##message('iflag=',iflag,'->',knownFlags[[iflag]], '; ', knownFlagNames[[iflag]])
        res@metadata$flags[[knownFlagNames[iflag]]] <- res@data[[knownFlags[[iflag]]]]
        res@data[[knownFlags[[iflag]]]] <- NULL
    }
    res@data <- res@data[order(names(res@data))] # put in alphabetical order for easier scanning in summary() views
    res
}

