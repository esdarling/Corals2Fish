
# ====================================
# = Import Graham coral + fish data  =
# ====================================
setwd("/Users/emilydarling/Dropbox/1-On the go/Graham_FD test/Data")         
#setwd("/nas02/home/e/s/esdarlin/Rproj/jobscr5")

dat <- read.csv("Graham_coralfish_25Sept2014.csv", header = TRUE, stringsAsFactors = TRUE) 
head(dat)
nrow(dat) 
names(dat)    

hist(dat$Longitude)

unique(dat$Year)
unique(dat$Country)

#Fig - make map of sites 
#Google earth 4 panels for each country

#Full-size map
#ip <- get_map(location = "Indonesia", zoom = 3, 
              source = "google", maptype = "satellite")

ip <- get_map(location = c(lon=100, lat = 0), zoom = 3,
              source = "google", maptype = "satellite")

ip <- get_map(location = c(-10,30,80,160),
              source = "google", maptype = "satellite")
ggmap(ip)
str(ip)

levels(dat$Country)
ggmap(ip) + geom_point(aes(x = Longitude, y = Latitude, fill = Country), 
                        size = 5, shape = 21, alpha = 0.2, 
                        position = position_jitter(width = 0.5, height = 0.5),
                        colour="black", 
                        data = dat) +
  scale_fill_manual(values = c("green","purple","turquoise","yellow")) +
  theme_bw(base_size = 20) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.position = "none")

setwd("/Users/emilydarling/Documents/Work/GitHub/Corals2Fish/1_plots")  
ggsave("IP map.pdf", height=22, width = 24, units = "in") 

#Seychelles
#sey <- get_map(location = "Seychelles", zoom = 10, 
               source = "google", maptype = "satellite")

sey <- get_map(location = c(lon = 5565, lat= -4.5), zoom = 10, 
               source = "google", maptype = "satellite")

ggmap(sey) + geom_point(aes(x = Longitude, y = Latitude), 
               size = 5, shape = 21, alpha = 0.6, 
               #position = position_jitter(width = 0.1, height = 0.1),
               colour="black", fill="yellow", 
               data = subset(dat, Country == "Seychelles")) +
  theme_bw(base_size = 12) +
  xlab("Longitude") +
  ylab("Latitude")

setwd("/Users/emilydarling/Documents/Work/GitHub/Corals2Fish/1_plots")  
ggsave("Sey map.pdf", height=4, width = 4, units = "in")   

#Maldives
#mal <- get_map(location = "Maldives", zoom = 7, 
#source = "google", maptype = "satellite")
#ggmap(mal)

mal <- get_map(location = c(lon = 73.2, lat= 0.5), zoom = 10, 
               source = "google", maptype = "satellite")

ggmap(mal) + geom_point(aes(x = Longitude, y = Latitude), 
                        size = 5, shape = 21, alpha = 0.6, 
                        #position = position_jitter(width = 0.1, height = 0.1),
                        colour="black", fill="turquoise", 
                        data = subset(dat, Country == "Maldives")) +
  theme_bw(base_size = 12) +
  xlab("Longitude") +
  ylab("Latitude")

setwd("/Users/emilydarling/Documents/Work/GitHub/Corals2Fish/1_plots")  
ggsave("Mal map.pdf", height=4, width = 4, units = "in")      

#Chagos
chag <- get_map(location = "Chagos", zoom = 9, 
               source = "google", maptype = "satellite")
ggmap(chag)

ggmap(chag) + geom_point(aes(x = Longitude, y = Latitude), 
                        size = 5, shape = 21, alpha = 0.4, 
                        #position = position_jitter(width = 0.1, height = 0.1),
                        colour="black", fill="purple", 
                        data = subset(dat, Country == "Chagos")) +
  theme_bw(base_size = 12) +
  xlab("Longitude") +
  ylab("Latitude")

setwd("/Users/emilydarling/Documents/Work/GitHub/Corals2Fish/1_plots")  
ggsave("Chago map.pdf", height=4, width = 4, units = "in") 

#GBR
#gbr <- get_map(location = "Australia", zoom = 4, 
                source = "google", maptype = "satellite")
#ggmap(gbr)

gbr <- get_map(location = c(lon = 147, lat= -18.5), zoom = 9, 
                source = "google", maptype = "satellite")

ggmap(gbr) + geom_point(aes(x = Longitude, y = Latitude), 
                         size = 5, shape = 21, alpha = 0.4, 
                         position = position_jitter(width = 0.05, height = 0.05),
                         colour="black", fill="green", 
                         data = subset(dat, Country == "Australia")) +
  theme_bw(base_size = 12) +
  xlab("Longitude") +
  ylab("Latitude")

setwd("/Users/emilydarling/Documents/Work/GitHub/Corals2Fish/1_plots")  
ggsave("GBR map.pdf", height=4, width = 4, units = "in")  





