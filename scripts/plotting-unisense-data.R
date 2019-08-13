library(ggplot2)
library(dplyr) 
setwd("/Users/Taruna/Dropbox/Wilbanks-lab/Unisense/")
input <- "/Users/Taruna/Dropbox/Wilbanks-lab/Unisense/data-raw/O2-and-N2O-experiment-29Jun2019.txt"
myData <- read.table(input, sep = ",", header = TRUE)
colnames(myData)
#myData <- myData %>% rename("Time..YYYY.MM.DD.hh.mm.ss." = "YYYY-MM-DD hh:mm:ss",
                            # "Time..ms." = "Time ms ",
                            # "Time.since.start..s" = "Time since start (s)",
                            # "Measurement" = "Measurement",
                            # "Raw..Sensor.5...O2..Degrees." = "Sensor 5 O2 Degrees",
                            # "Sensor.5...O2..μmol.L." = "O2 μM",
                            # "Raw..Sensor.1...N2O..MilliVolt." = "Sensor 1 N2O MilliVolt",
                            # "Sensor.1...N2O..μmol.L." = "N2O μM")
# Plot the O2 measurements from 2019-06-29 14:10:58 to 2019-06-29 14:54:45
start141058 <- which(myData$Time..YYYY.MM.DD.hh.mm.ss. == "2019-06-29 14:10:58")
stop145445 <- which(myData$Time..YYYY.MM.DD.hh.mm.ss. == "2019-06-29 14:54:45")
oxygen_141058_to_145445 <- myData[start141058:stop145445, c("Time..YYYY.MM.DD.hh.mm.ss.","Time.since.start..s.", "Sensor.5...O2..μmol.L.")]
View(oxygen_141058_to_145445)
#oxygen_141058_to_145445[1096,]
#oxygen_141058_to_145445_row1096removed <- oxygen_141058_to_145445[-c(1096),]
pdf("O2-in-dark-and-light.pdf")
O2InDarkAndLight <- ggplot(oxygen_141058_to_145445) +
  geom_point(aes(x=Time.since.start..s., 
                 y=Sensor.5...O2..μmol.L.), 
                 color="red") +
                 labs(y="O2 uM", x = "Time since start (s)", title = "O2 Measurements in Light and Dark") +
                 theme_bw()
print(O2InDarkAndLight)
dev.off()

# Plot the N2O measurements from 2019-06-29 14:55:50 to 2019-06-29 15:28:58
start145550 <- which(myData$Time..YYYY.MM.DD.hh.mm.ss. == "2019-06-29 14:55:50")
stop152858 <- which(myData$Time..YYYY.MM.DD.hh.mm.ss. == "2019-06-29 15:28:58")
N2O_145550_to_152858 <- myData[start145550:stop152858, c("Time..YYYY.MM.DD.hh.mm.ss.","Time.since.start..s.", "Sensor.1...N2O..μmol.L.")]
head(N2O_145550_to_152858)
pdf("N2O-in-dark-and-light.pdf")
N2OInDarkThenLightThenDark <- ggplot(N2O_145550_to_152858) +
                              geom_point(aes(x=Time.since.start..s., y=Sensor.1...N2O..μmol.L.), color="blue") +
                              labs(y="N2O uM", x = "Time since start (s)", title = "N2O Measurements in Dark followed by Light, followed by Dark") +
                              theme_bw()
print(N2OInDarkThenLightThenDark)
dev.off()


# Plot the O2 measurements from 2019-06-29 15:37:50 to 2019-06-29 16:03:24
start153750 <- which(myData$Time..YYYY.MM.DD.hh.mm.ss. == "2019-06-29 15:37:50")
stop160324 <- which(myData$Time..YYYY.MM.DD.hh.mm.ss. == "2019-06-29 16:03:24")
oxygen_153750_to_160324 <- myData[start153750:stop160324, c("Time..YYYY.MM.DD.hh.mm.ss.","Time.since.start..s.", "Sensor.5...O2..μmol.L.")]
head(oxygen_153750_to_160324)
pdf("O2-in-dark-and-light-2.pdf")
O2InDarkAndLight2 <- ggplot(oxygen_153750_to_160324) +
  geom_point(aes(x=Time.since.start..s., 
                 y=Sensor.5...O2..μmol.L.), 
             color="red") +
  labs(y="O2 uM", x = "Time since start (s)", title = "O2 Measurements in Dark and Then Light") +
  theme_bw()
print(O2InDarkAndLight2)
dev.off()




# Plot the N2O measurements from 2019-06-29 16:04:50 to 2019-06-29 16:26:36
start160450 <- which(myData$Time..YYYY.MM.DD.hh.mm.ss. == "2019-06-29 16:04:50")
stop162636 <- which(myData$Time..YYYY.MM.DD.hh.mm.ss. == "2019-06-29 16:26:36")
N2O_160450_to_162636 <- myData[start160450:stop162636, c("Time..YYYY.MM.DD.hh.mm.ss.","Time.since.start..s.", "Sensor.1...N2O..μmol.L.")]
head(N2O_160450_to_162636)
pdf("N2O-in-dark-for-10m-and-light-for-10m.pdf")
N2OInLightThenDark <- ggplot(N2O_160450_to_162636) +
  geom_point(aes(x=Time.since.start..s., y=Sensor.1...N2O..μmol.L.), color="blue") +
  labs(y="N2O uM", x = "Time since start (s)", title = "N2O Measurements in Light for About 10 mins followed by in Dark for About 10 mins") +
  theme_bw()
print(N2OInLightThenDark)
dev.off()



# Plot the N2O measurements from 2019-06-29 16:33:51 to 2019-06-30 07:19:09
overnight_N2O <- "/Users/Taruna/Dropbox/Wilbanks-lab/Unisense/data-raw/O2-and-N2O-experiment-Overnight-w-measurementType-condition-29Jun2019_to_30Jun2019.txt"
overnight_N2O_data <- read.table(overnight_N2O, sep = ",", header = TRUE)
colnames(overnight_N2O_data)
head(overnight_N2O_data)
# start163351 <- which(myData$Time..YYYY.MM.DD.hh.mm.ss. == "2019-06-29 16:33:51")
# stop071909 <- which(myData$Time..YYYY.MM.DD.hh.mm.ss. == "2019-06-30 07:19:09")
# N2O_163351_to_071909 <- myData[start163351:stop071909, c("Time..YYYY.MM.DD.hh.mm.ss.","Time.since.start..s.", "Sensor.1...N2O..μmol.L.")]
# head(N2O_163351_to_071909)
pdf("N2O-in-dark-for-10m-Then-overnight-2hDark-2hLight-cycle.pdf")
N2Oovernight <- ggplot(overnight_N2O_data) +
  geom_point(aes(x=Time.since.start..s., y=Sensor.1...N2O..μmol.L., color=MeasurementType_Condition)) +
  labs(y="N2O uM", x = "Time since start (s)", title = "N2O Measurements in Dark About 10 mins followed by 2hr Light and 2hr Dark Cycle Overnight") +
  theme_bw()
print(N2Oovernight)
dev.off()

pdf("xlim-N2O-in-dark-for-10m-Then-overnight-2hDark-2hLight-cycle.pdf")
N2Oovernight <- ggplot(overnight_N2O_data) +
  geom_point(aes(x=Time.since.start..s., y=Sensor.1...N2O..μmol.L., color=MeasurementType_Condition)) +
  labs(y="N2O uM", x = "Time since start (s)", title = "N2O Measurements in Dark About 10 mins followed by 2hr Light and 2hr Dark Cycle Overnight") +
  xlim(c(8000,10000)) +
  theme_bw()
print(N2Oovernight)
dev.off()
