source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")

R <- read.csv("1000F-projects.csv")

R[is.na(R)] = 0

R

# get the inverse path
R2 = t(apply(R, 1, function(x){
  k = x!=0
  v = rank((x[k] - 5) * -1)
  x[k] = v
  x
}))

R2

R = rbind(R, R2)

R = as.rankings(R)

R

adj = PlackettLuce::adjacency(R)

net = network(R)

plot(net)

pdf("output/1000FARMS-crop-network.pdf", 
    width = 8,
    height = 8)
plot(net)
dev.off()

png("output/1000FARMS-crop-network.png", 
    width = 20,
    height = 20,
    units = "cm", 
    res = 500)
plot(net)
dev.off()

