# Copyright 2019 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


# generate plots for taxonomic groups

library(ggplot2)
library(dplyr)
library(envreportutils)


csi.plot <- csi %>%
  group_by(Taxonomic_Group, Year) %>%
  summarize(mean = mean(mean_wt), lci = mean(lci), uci = mean(uci), N = N) %>%
  mutate(Year = as.numeric(Year))


# add plotting parameters

x_scale <- scale_x_continuous(limits = c(1990, max(csi.plot$Year) + 1),
                              breaks = seq(1992, 2018, 4),
                              expand = c(0,0))

normpal <- c("Breeding Birds" = "#e41a1c",
             "Freshwater Fish" = "#377eb8",
             "Mammals" = "#4daf4a",
             "Reptiles & Amphibians" = "#4daf4a")

normpal <- c("#e41a1c", "#377eb8","#4daf4a",  "#4daf4a")


# plot facet plot

p1 <- ggplot(csi.plot, aes(x = Year, y = mean, group = Taxonomic_Group)) + # same issue here as line below
  facet_wrap(~ Taxonomic_Group) +
  scale_colour_manual(values= normpal) +
  geom_point(aes(y = mean), size = 2) +
  geom_text(aes(label=N), vjust=2, col= " dark grey", size = 3) +
  geom_line(aes(y = mean)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2) +
  theme_soe() +
  x_scale


p <- plot_ly(p1)
chart_link = plotly_POST(p, filename="facetwrap/basic")


# or the plotly option
p1 <- ggplot(csi.plot, aes(x = Year, y = mean)) + # same issue here as line below
  facet_wrap(~ Taxonomic_Group) +
  scale_colour_manual(values= normpal) +
  geom_point(aes(y = mean), size = 2) +
  #geom_text(aes(label=N), vjust=2, col= " dark grey", size = 3) +
  geom_line(aes(y = mean)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2) +
  theme_soe() +
  x_scale


p <- ggplotly(p1)

# add the numbers to the plot of sp.



# Plot by BC Listing ------------------------------------------------------


csi.bc.plot <- csi_bc %>%
  group_by(`BC List`, Year) %>%
  summarize(mean = mean(mean_wt), lci = mean(lci), uci = mean(uci))

csi.bc.plot <- csi.bc.plot %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!`BC List` %in% c("extinct", "unknown", "no status"))


p2 <- ggplot(csi.bc.plot, aes(x = Year, y = mean, group = `BC List`)) + # same issue here as line below
  facet_wrap(~ `BC List`) +
  scale_colour_manual(values= normpal) +
  geom_point(aes(y = mean), size = 2) +
  geom_line(aes(y = mean)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2) +
  theme_soe() +
  x_scale


# save plots

multi_plot <- function(plotdata, filename) {
  svg_px( paste0(filename,".svg"), width = 500, height = 400)
  plot(plotdata)
  dev.off()
  png_retina(paste0(filename,".png"), width = 500, height = 400,
             units = "px", type = "cairo-png", antialias = "default")
  plot(plotdata)
  dev.off()
}


multi_plot(p1, "./print_ver/csi_tax")

multi_plot(p2, "./print_ver/csi_bclist")





# Plotly graphics - test --------------------------------------------------

library(plotly)

p <- plot_ly(economics, x = ~date, y = ~unemploy / pop)

