

library(dplyr)


fileName <- "payments.csv"
setwd(dirname(parent.frame(2)$ofile))

if (!exists("data") || is.null(data) || !is.data.frame(data) || nrow(data) <= 0) {
  print("reading data set")
  data <- read.csv(fileName)
}


if (!exists("ny") || is.null(ny) || length(ny) <= 0) {
  print("subsetting NY state")
  ny <- filter(data, Provider.State == 'NY')
}


colsToCheck <- c(which(names(ny) == "Average.Covered.Charges"), 
                 which(names(ny) == "Average.Total.Payments"))
cc <-  complete.cases( ny[  , c(10,11)])
nyCompl <- ny[cc, ]


with(nyCompl,plot(Average.Covered.Charges, Average.Total.Payments))
linea <- lm( Average.Total.Payments ~ Average.Covered.Charges, data = ny)
abline(linea)
plot1<- "plot1.pdf"
unlink(plot1, force = TRUE)
dev.copy2pdf(file=plot1)
# dev.off()


stati <- as.character(unique(data$Provider.State))
medCond <- as.character(unique(data$DRG.Definition))
nrCol <- length(stati)/2
if ((nrCol - floor(nrCol)) != 0 ) {
  nrCol <- floor(nrCol)+1
}
par( mfrow = c(2,nrCol), oma = c(2,2,4,1))
for (i in seq_along(stati)) {
  print(stati[i])
  dataSub <- data[data$Provider.State == stati[i] , ]
  # head(dataSub)
  with(dataSub,plot(Average.Covered.Charges, Average.Total.Payments, 
                    col = DRG.Definition,
                    main = stati[i],
       xlab = "Avg. Cov. Charges", ylab = "Avg Paym"))
  linea <- lm( Average.Total.Payments ~ Average.Covered.Charges, data = dataSub)
  abline(linea)
}
mtext("Covered charges and total payments by state\n(and med. condition, by color)", outer = TRUE, cex = 1.5)
plot2 <- "plot2.pdf"
unlink(plot2, force = TRUE)
dev.copy2pdf(file=plot2)

