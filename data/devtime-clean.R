fx.devtime <- here::here("data", "devtime.xlsx")
tx.devtime <- here::here("data", "devtime.txt")

units = "days"

lines <- c('3B'="A", '6A'="D", '10B'="H", '12B'="L")

series <- list(8:40, 47:81, 92:164, 171:221, 237:321, 330:401, 412:487, 498:576, 586:682, 692:758, 769:842, 855:921, 932:999, 1010:1090, 1101:1192, 1204:1248, 1256:1336, 1346:1423, 1439:1498, 1509:1581, 1594:1650)

data <- data.frame()

for (iss in seq_along(series)) {
    for (ll in names(lines)) {
        raw <- readxl::read_excel(fx.devtime, sheet=1, range=paste0(lines[ll], series[[iss]][1]-1, ":", lines[ll], max(series[[iss]])), col_names=FALSE, .name_repair = "unique_quiet")
        if (sum(!is.na(raw)) > 1) {
            start <- raw[[1]][1]
            x <- raw[[1]][-1]
            x <- x[!is.na(x)]
            for (ii in which(diff(x) < 0)) {
                warning("Cell ", lines[ll], series[[iss]][ii], ": development times unsorted.")
            }
            data <- rbind(data, data.frame(Line=ll, Rep=paste0("R", iss), devtime=round(as.numeric(difftime(x,start,units=units)), digits=3)))
        }
    }
}

write.table(file=tx.devtime, data, row.names=FALSE, sep="\t", quote=FALSE)
