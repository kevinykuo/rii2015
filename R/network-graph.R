library(dplyr)
library(magrittr)
library(networkD3)

cedants <- c("State Farm", "Allstate", "Liberty Mutual", "Berkshire Hathaway", "Travelers",
             "AIG", "Nationwide", "Progressive", "Farmers", "USAA", "Hartford", "Chubb", "CNA",
             "American Family", "Allianz", "Metlife", "Prudential", "New York Life", "TIAA",
             "Lincoln Financial", "Transamerica", "AXA")

reinsurers <- c("Munich Re", "Swiss Re", "Hannover Re", "Lloyd's", "SCORE S.E.", "RGA", "PartnerRe")

cedantIndices <- 1:length(cedants) - 1
reinsurerIndices <- 1:length(reinsurers) + length(cedants) - 1

treaties <- data.frame(cedant = cedantIndices) %>%
  mutate(reinsurer = sample(reinsurerIndices, nrow(.), replace = TRUE)) %>%
  bind_rows(data.frame(
    cedant = sample(cedantIndices, floor(length(cedantIndices) / 3)),
    reinsurer = sample(reinsurerIndices, floor(length(cedantIndices) / 3), replace = TRUE)
  )) %>%
  rbind(sample(reinsurerIndices, 2), sample(reinsurerIndices, 2), sample(reinsurerIndices, 2)) %>%
  unique %>%
  mutate(premiumCeded = rgamma(nrow(.), 10))

companies <- data.frame(company = cedants, group = "primary") %>%
  rbind(cbind(company = reinsurers, group = "reinsurer")) %>%
  mutate(size = rgamma(nrow(.), 10, rate = 0.3))

forceNetwork(Links = treaties, Nodes = companies, 
             Source = "cedant", Target = "reinsurer",
             Value = "premiumCeded", NodeID = "company", Nodesize = "size",
             Group = "group", opacity = 0.8,
             colourScale = "d3.scale.category10()")
