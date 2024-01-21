# Wheel for post-grad requirements
# Max Griswold
# 1/18/24

library(circlize)
library(data.table)
library(plyr)

setwd("C:/Users/griswold/Documents/GitHub/Policy-Wheels/")

df <- fread('./data/post_grad_requirements.csv', na.strings = "-")

# Hold onto only relevant columns requested by PI:
df <- df[, c("State", "Relevant Req’t", "Topic: Prescribing", " Topic: SUD, misuse", "Topic: Pain"), with = F]

setnames(df, names(df), c("state", "rel_req", "Prescribing practices", "SUD/Addiction", "Pain/pain management"))
df <- melt(df, id.vars = "state", value.name = "implemented", variable.name = "topic")

# I'm assuming that the column for relevant requirement isn't that helpful so I'm dropping it
# (if relevant requirement is No, then all topic areas are No):
df <- df[topic != "rel_req",]
df[, implemented := ifelse(implemented == "Yes", 1, 0)]

df[, implemented := max(.SD$implemented), by = c("state", "topic")]
df <- unique(df)

# Reviewers stated Arizona was coded incorrectly so recode:
df[state == "AZ" & (topic == "Prescribing practices" | topic == "Pain/Pain Management"), implemented := 0]

plot_colors <- c("#66c2a5", "#fc8d62", "#8da0cb")

# Order states so that region-names make sense when applied to areas of the policy circle:
states <- c("OH", "WI", "DE", "FL", "GA", "MD", "NC", "SC", "VA", "DC", "WV", "IA", 
            "KS", "MN", "MO", "NE", "ND", "SD", "AL", "KY", "MS", "TN", "AR", "LA", 
            "OK", "TX", "AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", "CA", "OR", 
            "WA", "AK", "HI", "CT", "ME", "MA", "NH", "RI", "VT", "NY", "NJ", "PA", 
            "IL", "IN", "MI")


# Set up dictionary for policy wheel options:
policies <- unique(df$topic)
wheel_opts <- data.table("topic" = policies,
                         "i" = 1:3,
                         "col" = plot_colors)

# Below fills in policy wheel cells by year and policy
fill_in_cells <- function(p){
  
  state_implementors <- unique(df[topic == p & implemented == 1]$state)
  
  track_index <- wheel_opts[topic == p]$i
  track_color <- wheel_opts[topic == p,]$col
  
  for (s in state_implementors){
    draw.sector(get.cell.meta.data("cell.start.degree", sector.index = s),
                get.cell.meta.data("cell.end.degree", sector.index = s),
                rou1 = get.cell.meta.data("cell.top.radius", track.index = track_index),
                rou2 = get.cell.meta.data("cell.bottom.radius", track.index = track_index),
                col = track_color)
  }
}

plot_policy_wheel <- function(){
  
  # Establish margins & cell padding
  par(mai=c(0.35, 0.35, 0.35, 0.35), xpd = TRUE)
  
  circos.par(cell.padding = c(0, 0, 0, 0))
  
  # Add fifty-one sectors (i.e. one for each state + DC)
  circos.initialize(factors = states, xlim = c(0, 1))
  
  # Add three tracks (i.e. one for each policy)
  replicate(3, circos.track(ylim = c(0,1), track.height = 0.09), simplify = F)
  
  # Add state labels
  for(s in unique(states)) {
    highlight.sector(sector.index = s, track.index = 1,
                     text = s, padding = c(-.5,1,1.5,1), cex = 1.3, text.vjust = .5, col = NA, facing = "downward")
  }
  
  # Add line segments to distinguish between regions
  for(s in c("CT", "NY", "IL", "DE", "IA", "AL", "AR", "AZ", "CA")) {
    circos.segments(x0 = -0.1, y0 = 0, x1 = -0.07, y1 = 5.8, lwd = 3.5, sector.index = s)
  }
  
  # Label the regions
  # New England
  highlight.sector(sector.index = c("CT", "ME", "MA", "NH", "RI", "VT"), track.index = 1,
                   text = "New England", padding = c(1,0,5,0), cex = 1.4, font = 2,
                   border = NA, col = NA, facing = "bending.inside")
  
  # Mid-Atlantic
  highlight.sector(sector.index = c("NY", "NJ", "PA"), track.index = 1,
                   text = "Mid-Atlantic", padding = c(1,0,5,0), cex = 1.4, font = 2,
                   border = NA, col = NA, facing = "bending.inside")
  
  # East North Central
  highlight.sector(sector.index = c("IL", "IN", "MI", "OH", "WI"), track.index = 1,
                   text = "East North \nCentral", padding = c(1,0,5,0), cex = 1.4,
                   font = 2, border = NA, col = NA, facing = "bending.inside")
  
  # South Atlantic
  highlight.sector(sector.index = c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "DC", "WV"),
                   track.index = 1, text = "South Atlantic", padding = c(1,0,5,0),
                   cex = 1.4, font = 2, border = NA, col = NA, facing = "bending.outside")
  
  # West North Central
  highlight.sector(sector.index = c("IA", "KS", "MN", "MO", "NE", "ND", "SD"),
                   track.index = 1, text = "West North \nCentral", padding = c(1,0,5,0),
                   cex = 1.4, font = 2, border = NA, col = NA, facing = "bending.outside")
  
  # East South Central
  highlight.sector(sector.index = c("AL", "KY", "MS", "TN"), track.index = 1,
                   text = "East South \nCentral", padding = c(1,0,5,0), cex = 1.4, font = 2,
                   border = NA, col = NA, facing = "bending.outside")
  
  # West South Central
  highlight.sector(sector.index = c("AR", "LA", "OK", "TX"), track.index = 1,
                   text = "West South \nCentral", padding = c(1,0,5,0), cex = 1.4, font = 2,
                   border = NA, col = NA, facing = "bending.outside")
  
  # Mountain
  highlight.sector(sector.index = c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY"),
                   track.index = 1, text = "Mountain", padding = c(1,0,5,0), cex = 1.4,
                   font = 2, border = NA, col = NA, facing = "bending.inside")
  
  # Pacific
  highlight.sector(sector.index = c("CA", "OR", "WA", "AK", "HI"), track.index = 1,
                   text = "Pacific", padding = c(1,0,5,0), cex = 1.4,
                   border = NA, col = NA, font = 2, facing = "bending.inside")
  
  # Highlight states that implemented legislation by Jan 1 of that year
  lapply(policies, fill_in_cells)
  
  circos.clear()
  
}

pdf(file = "cme_requirements_policy_wheel.pdf", height = 11.7, width = 16.5)

# Create layout for the plot
layout.mat <- matrix(c(1, 2), ncol=1)                           
layout(layout.mat, respect = TRUE, heights = c(5, 1), widths = c(6))

plot_policy_wheel()

# Add legend in correct order of colors:
col_order <- matrix(1:3, nrow = 1, ncol = 3, byrow = T)

par(xpd=TRUE, family = 'sans')
plot.new()
plot.window(xlim = c(0,1.5), ylim = c(0,2.5))
legend("center",
       pch = c(15, 15, 15),
       legend = wheel_opts$topic[col_order],
       col = wheel_opts$col[col_order],
       xjust = 0.5, y.intersp = 1.3, 
       x.intersp = 1.3, cex = 1.2, 
       pt.cex = 2.7, bty = "n", ncol = 3,
       title = as.expression(bquote(bold("States with any relevant CME requirements, December 2020"))))

dev.off()

