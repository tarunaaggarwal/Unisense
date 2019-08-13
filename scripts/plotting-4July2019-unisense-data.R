library(ggplot2)
library(dplyr) 
library(ggpmisc)
# create a wrapper to wrap the title text
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

setwd("/Users/Taruna/Dropbox/Wilbanks-lab/Unisense/")
n2o <- "/Users/Taruna/Dropbox/Wilbanks-lab/Unisense/data-raw/N2O-dark-equilibrated-and-anoxic-in-CO2N2-water-overnight-live-GBs-post-parade-4Jul2019.txt"
o2 <- "/Users/Taruna/Dropbox/Wilbanks-lab/Unisense/data-raw/O2-dark-equilibrated-and-anoxic-in-CO2N2-water-overnight-live-GBs-post-parade-4Jul2019.txt"
n2o_table <- read.table(n2o, sep = ",", header = TRUE)
o2_table <- read.table(o2, sep = ",", header = TRUE)
n2o_table <- tbl_df(n2o_table)
o2_table <- tbl_df(o2_table)
head(n2o_table)
head(o2_table)
colnames(n2o_table)
colnames(o2_table)

# plot N2O and O2 for the chamber without any GBs. Start time 2019-07-04 10:09:04 and end time 2019-07-04 10:25:22
start100904 <- which(n2o_table$Time..YYYY.MM.DD.hh.mm.ss. == "2019-07-04 10:09:04")
stop102522 <- which(n2o_table$Time..YYYY.MM.DD.hh.mm.ss. == "2019-07-04 10:25:22")
n2o_100904_to_102522 <- n2o_table[start100904:stop102522, c("Time..YYYY.MM.DD.hh.mm.ss.","Time.since.start..s.", "Sensor.1...N2O..μmol.L.")]
head(n2o_100904_to_102522)

pdf("N2O-Measurements-in-0.2uM-filtered-Marsh-Water.pdf")
n2o_without_GBs_initial <- ggplot(n2o_100904_to_102522) +
  geom_point(aes(x=Time.since.start..s., 
                 y=Sensor.1...N2O..μmol.L.), 
             color="blue") +
  labs(y="N2O uM", x = "Time since start (s)", title = "N2O Measurements in 0.2uM filtered Marsh Water") +
  ylim(0, 0.4) +
  theme_bw()
print(n2o_without_GBs_initial)
dev.off()



# Plot the O2 measurements from 2019-07-04 10:09:08 to 2019-07-04 10:25:23
start100908 <- which(o2_table$Time..YYYY.MM.DD.hh.mm.ss. == "2019-07-04 10:09:08")
stop102523 <- which(o2_table$Time..YYYY.MM.DD.hh.mm.ss. == "2019-07-04 10:25:23")
oxygen_100908_to_102523 <- o2_table[start100908:stop102523, c("Time..YYYY.MM.DD.hh.mm.ss.","Time.since.start..s.", "Sensor.6...O2..μmol.L.")]
head(oxygen_100908_to_102523)
pdf("O2-Measurements-in-0.2uM-filtered-Marsh-Water.pdf")
o2_without_GBs_initial <- ggplot(oxygen_100908_to_102523) +
  geom_point(aes(x=Time.since.start..s., 
                 y=Sensor.6...O2..μmol.L.), 
             color="red") +
  labs(y="O2 uM", x = "Time since start (s)", title = "O2 Measurements in 0.2uM filtered Marsh Water") +
  ylim(0, 245) +
  theme_bw()
print(o2_without_GBs_initial)
dev.off()







# plot N2O for the chamber without any GBs. Start time 2019-07-04 10:28:26 and end time 2019-07-04 10:53:05
start102826 <- which(n2o_table$Time..YYYY.MM.DD.hh.mm.ss. == "2019-07-04 10:28:26")
stop105305 <- which(n2o_table$Time..YYYY.MM.DD.hh.mm.ss. == "2019-07-04 10:53:05")
n2o_102826_to_105305 <- n2o_table[start102826:stop105305, c("Time..YYYY.MM.DD.hh.mm.ss.","Time.since.start..s.", "Sensor.1...N2O..μmol.L.")]
head(n2o_102826_to_105305)

pdf("N2O-Measurements-in-0.2uM-filtered-Marsh-Water-Part2.pdf")
n2o_without_GBs <- ggplot(n2o_102826_to_105305) +
  geom_point(aes(x=Time.since.start..s., 
                 y=Sensor.1...N2O..μmol.L.), 
             color="blue") +
  labs(y="N2O uM", x = "Time since start (s)", title = "N2O Measurements in 0.2uM filtered Marsh Water") +
  ylim(0, 0.4) +
  theme_bw()
print(n2o_without_GBs)
dev.off()



# Plot the O2 measurements from 2019-07-04 10:28:31 to 2019-07-04 10:53:08
start102831 <- which(o2_table$Time..YYYY.MM.DD.hh.mm.ss. == "2019-07-04 10:28:31")
stop105308 <- which(o2_table$Time..YYYY.MM.DD.hh.mm.ss. == "2019-07-04 10:53:08")
oxygen_102831_to_105308 <- o2_table[start102831:stop105308, c("Time..YYYY.MM.DD.hh.mm.ss.","Time.since.start..s.", "Sensor.6...O2..μmol.L.")]
head(oxygen_102831_to_105308)
pdf("O2-Measurements-in-0.2uM-filtered-Marsh-Water-Part2.pdf")
o2_without_GBs <- ggplot(oxygen_102831_to_105308) +
  geom_point(aes(x=Time.since.start..s., 
                 y=Sensor.6...O2..μmol.L.), 
             color="red") +
  labs(y="O2 uM", x = "Time since start (s)", title = "O2 Measurements in 0.2uM filtered Marsh Water") +
  ylim(0, 245) +
  theme_bw()
print(o2_without_GBs)
dev.off()




############################# Data Measurements ############################# 


# plot N2O for the chamber with anoxic water and no GBs. Start time 2019-07-04 11:43:35 and end time 2019-07-04 12:04:30 
start114335 <- which(n2o_table$Time..YYYY.MM.DD.hh.mm.ss. == "2019-07-04 11:43:35")
stop120430 <- which(n2o_table$Time..YYYY.MM.DD.hh.mm.ss. == "2019-07-04 12:04:30")
n2o_114335_to_120430 <- n2o_table[start114335:stop120430, c("Time..YYYY.MM.DD.hh.mm.ss.","Time.since.start..s.", "Sensor.1...N2O..μmol.L.")]
head(n2o_114335_to_120430)

pdf("N2O-Measurements-in-Anoxic-Water.pdf")
n2o_anoxic_water <- ggplot(n2o_114335_to_120430) +
  geom_point(aes(x=Time.since.start..s., 
                 y=Sensor.1...N2O..μmol.L.), 
             color="blue") +
  labs(y="N2O uM", x = "Time since start (s)", title = "N2O Measurements In Just the Axonic Water that GBs were Incubated in Overnight") +
  theme_bw()
print(n2o_anoxic_water)
dev.off()





# plot N2O for the chamber with GBs in the anoxic water that they were incubated in overnight
#Start time 2019-07-04 12:04:30 and end time 2019-07-04 17:21:29
n2o_acetylene <- "/Users/Taruna/Dropbox/Wilbanks-lab/Unisense/data-raw/n2o_120430_to_172129.txt"
n2o_acetylene_table <- read.table(n2o_acetylene, sep = ",", header = TRUE)
n2o_acetylene_table <- tbl_df(n2o_acetylene_table)
colnames(n2o_acetylene_table)
head(n2o_acetylene_table)

#write.csv(n2o_120430_to_172129, "n2o_120430_to_172129.csv")
pdf("N2O-Measurements-in-GBs-in-Anoxic-Water-Under-Acetylene.pdf")
n2o_GBs_in_anoxic_water_acetylene <- ggplot(n2o_120430_to_172129) +
  geom_point(aes(x=Time.since.start..s., 
                 y=Sensor.1...N2O..μmol.L., color=Treatment)) +
  labs(y="N2O uM", x = "Time since start (s)", title = "N2O Measurements In GBs Incubated in Axonic Water Overnight and Exposed to Acetylene") +
  theme_bw()
print(n2o_GBs_in_anoxic_water_acetylene)
dev.off()

###### Plot the GBs in anoxic water 
GBs_in_anoxic_water <- 
  ggplot(n2o_120430_to_172129[n2o_120430_to_172129$Treatment=="GBs_in_anoxic_water",]) +
  geom_point(aes(x=Time.since.start..s., 
                 y=Sensor.1...N2O..μmol.L.), color="hotpink") +
  labs(y="N2O uM", x = "Time since start (s)", title = "N2O Concentrations in Green Berries Incubated Overnight in Anoxic Water") +
  theme_bw()


GBs_in_anoxic_water_with_acetylene <- n2o_120430_to_172129[n2o_120430_to_172129$Treatment=="Acetylene_in_water",]
head(GBs_in_anoxic_water_with_acetylene)
View(GBs_in_anoxic_water_with_acetylene)
colnames(GBs_in_anoxic_water_with_acetylene)
start122224 <- which(GBs_in_anoxic_water_with_acetylene$Time..YYYY.MM.DD.hh.mm.ss. == "2019-07-04 12:22:24")
stop125544 <- which(GBs_in_anoxic_water_with_acetylene$Time..YYYY.MM.DD.hh.mm.ss. == "2019-07-04 12:55:44")
n2o_122224_to_125544 <- GBs_in_anoxic_water_with_acetylene[start122224:stop125544, c("Time..YYYY.MM.DD.hh.mm.ss.","Time.since.start..s.", "Sensor.1...N2O..μmol.L.","Treatment")]
head(n2o_122224_to_125544)
pdf("N2O-Measurements-in-0.2uM-filtered-Marsh-Water-Under-Acetylene-SteepPartOfCurve.pdf")
n2o_acetylene_steeppartofcurve <- 
         ggplot(data = n2o_122224_to_125544, 
         aes(x=Time.since.start..s., y=Sensor.1...N2O..μmol.L.)) + 
         geom_point(color='deepskyblue') +
         geom_smooth(method = "lm", color="black", se=FALSE, formula = y ~ x) +
         stat_poly_eq(formula = y ~ x,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +         
         labs(y="N2O uM", x = "Time since start (s)") +
         theme_bw() +
         ggtitle(wrapper("N2O Concentrations in Green Berries Incubated Overnight in Anoxic Water After Addition of Acetylene - Steep Part of the Whole Curve", width = 60)) 


  ggplot(n2o_122224_to_125544) +
  geom_point(aes(x=Time.since.start..s., 
                 y=Sensor.1...N2O..μmol.L.), 
             color="deepskyblue") +
  labs(y="O2 uM", x = "Time since start (s)", title = "O2 Measurements in 0.2uM filtered Marsh Water") +
  ylim(0, 245) +
  theme_bw()
print(o2_without_GBs)
dev.off()

GBs_in_anoxic_water_with_acetylene_plot <-
ggplot(data = GBs_in_anoxic_water_with_acetylene, 
              aes(x=Time.since.start..s., y=Sensor.1...N2O..μmol.L.)) + 
              geom_point(color='deepskyblue') +
              geom_smooth(method = "lm", color="black") +
              labs(y="N2O uM", x = "Time since start (s)") +
              theme_bw() +
              ggtitle(wrapper("N2O Concentrations in Green Berries Incubated Overnight in Anoxic Water After Addition of Acetylene", width = 60)) 
              


# Plot the O2 measurements from 2019-07-04 10:28:31 to 2019-07-04 10:53:08
start102831 <- which(o2_table$Time..YYYY.MM.DD.hh.mm.ss. == "2019-07-04 10:28:31")
stop105308 <- which(o2_table$Time..YYYY.MM.DD.hh.mm.ss. == "2019-07-04 10:53:08")
oxygen_102831_to_105308 <- o2_table[start102831:stop105308, c("Time..YYYY.MM.DD.hh.mm.ss.","Time.since.start..s.", "Sensor.6...O2..μmol.L.")]
head(oxygen_102831_to_105308)
pdf("O2-Measurements-in-0.2uM-filtered-Marsh-Water-Part2.pdf")
o2_without_GBs <- ggplot(oxygen_102831_to_105308) +
  geom_point(aes(x=Time.since.start..s., 
                 y=Sensor.6...O2..μmol.L.), 
             color="red") +
  labs(y="O2 uM", x = "Time since start (s)", title = "O2 Measurements in 0.2uM filtered Marsh Water") +
  ylim(0, 245) +
  theme_bw()
print(o2_without_GBs)
dev.off()