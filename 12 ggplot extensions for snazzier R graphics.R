install.packages("tidyverse")
install.packages("rio")
install.packages("lattice")
install.packages("ggplot2")
library(ggplot2)
library(scales)
library(rio)
snowfall2000s <-
  read.csv("https://gist.githubusercontent.com/smach/5544e1818a76a2cf95826b78a80fc7d5/raw/8fd7cfd8fa7b23cba5c13520f5f06580f4d9241c/boston_snowfall.2000s.csv")
ggplot(snowfall2000s, aes(x = Winter, y = Total)) +
  geom_col(color = "black", fill="#0072B2") +
  theme_minimal() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line =
          element_line(colour = "gray"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
  ) +
  ylab("") + xlab("")

dim(snowfall2000s)

# Here’s how to turn that into a custom geom called my_geom_col:
  
install.packages("ggpackets")
library(ggpackets)
my_geom_col <- ggpacket() +
  geom_col(color = "black", fill="#0072B2") +
  theme_minimal() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line =
          element_line(colour = "gray"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
  ) +
  ylab("") + xlab("")

# Here’s how simple it is to use that new geom:
# Graph created with a custom ggpackets geom.


ggplot(snowfall2000s, aes(x = Winter, y = Total)) +
  my_geom_col()


# histogram from the Palmer penguins data set with ggblanket
install.packages("ggblanket")
install.packages("palmerpenguins")
library(ggblanket)
library(palmerpenguins)
penguins |> 
  gg_histogram(x = body_mass_g, col = species)

# Several other packages try to simplify ggplot2 and change its defaults, too, including ggcharts. Its simplified functions use syntax like

install.packages("ggcharts")
library(ggcharts)
column_chart(snowfall2000s, x = Winter, y = Total)


# Simple text customization: ggeasy - Simple text customization: ggeasy

# Highlight items in your plots: gghighlight. gghighlight(Total > 90)

install.packages("gghighlight")
library(gghighlight)
ggplot(snowfall2000s, aes(x = Winter, y = Total)) +
  my_geom_col() +
  gghighlight(Total > 90)


# Or if I want to call out specific years, such as 2011-12 and 2014-15, I can set those as my gghighlight() condition:

ggplot(snowfall2000s, aes(x = Winter, y = Total)) +
  my_geom_col() +
  gghighlight(Winter %in% c('2011-12', '2014-15'))


# Add themes or color palettes: ggthemes and others
# Here’s an example with the ggthemes package’s solarized theme and colorblind palette:

install.packages("ggthemes")
library(ggthemes)
ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g, color = species)) +
  geom_point() +
  ggthemes::theme_solarized() +
  scale_color_colorblind()

# Convey uncertainty: ggdist
install.packages("dplyr")
install.packages("ggdist")
install.packages("simEd")
library(ggdist)
library(dplyr)
set.seed(12345) # for reproducibility
data.frame(
  abc = c("a", "b", "b", "c"),
  value = rnorm(200, c(1, 8, 8, 3), c(1, 1.5, 1.5, 1))) %>%
  ggplot(aes(y = abc, x = value, fill = abc)) +
  stat_slab(aes(thickness = stat(pdf*n)), scale = 0.7) +
  stat_dotsinterval(side = "bottom", scale = 0.7, slab_size = NA) +
  scale_fill_brewer(palette = "Set2")

# ggiraph
install.packages("plotly")
library(plotly)
ggplotly(
  ggplot(snowfall2000s, aes(x = Winter, y = Total)) +
    geom_col() +
    labs(title = "Annual Boston Snowfall", subtitle = "2000 to 2016")
)
plot_ly(snowfall2000s, x = ~Winter, y = ~Total, type = "bar")


# Use the girafe() function to make the plot interactive
install.packages("ggiraph")
library(ggiraph)
mygraph <- ggplot(snowfall2000s, aes(x = Winter, y = Total, tooltip = Total)) +
  geom_col_interactive(color = "black", fill="#0072B2") +
  theme_minimal() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line =
          element_line(colour = "gray"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(label = comma) +
  ylab("") + xlab("") +
  labs(title = "Annual Boston Snowfall", subtitle = "2000 to 2016")
girafe(ggobj = mygraph)


# Explain the stats behind your plot: ggstatsplot
# Correlation visualization created with ggstats.

install.packages("ggstatsplot")
library(ggstatsplot)
ggcorrmat(
  data     = ggplot2::msleep,
  colors   = c("#B2182B", "white", "#4D4D4D"),
  title    = "Correlalogram for the msleep mammals sleep data set in ggplot2",
  subtitle = "Sleep units: hours; weight units: kilograms"
)


# Drag-and-drop ggplot: esquisse
# Creating an ordered bar graph with the esquisse package.

# Multiple plots: patchwork
install.packages("patchwork")
library(patchwork)
p1 <- ggplot(mtcars) + 
  geom_boxplot(aes(gear, disp, group = gear)) + 
  ggtitle('Box Plot')
p2 <- ggplot(mtcars) + 
  geom_point(aes(mpg, disp)) + 
  ggtitle('1st Scatter Plot')
p3 <- ggplot(mtcars) + 
  geom_point(aes(hp, wt, colour = mpg)) + 
  ggtitle('2nd Scatter Plot')
p1 / (p2 + p3) +
  plot_annotation(title = 'My overall dataviz title',
                  subtitle = 'From the patchwork package',
                  caption = 'Source: mtcars data set'
  )

# ganttrify
install.packages("ganttrify",
                 repos = c(
                   "https://giocomai.r-universe.dev",
                   "https://cloud.r-project.org"
                 )
)


############################################################

library("ganttrify")

ganttrify(
  project = ganttrify::test_project,
  project_start_date = "2021-03",
  font_family = "Roboto Condensed"
)


#  add spot labels for events, deliverables, outputs, milestones
ganttrify(
  project = ganttrify::test_project,
  spots = ganttrify::test_spots,
  project_start_date = "2021-03",
  font_family = "Roboto Condensed"
)


# geomtextpath lets you easily generate curved text labels
install.packages("geomtextpath")
library(geomtextpath)
ggplot(iris, aes(x = Sepal.Length, colour = Species, label = Species)) +
  theme(legend.position = "none") +
  geom_textdensity(size = 6, fontface = 2, spacing = 50, vjust = -0.2, hjust = "ymax") +
  ylim(c(0, 1.3))


# ggforce has various geoms that offer additional ggplot2 functionality.
# One I found particularly interesting is facet_zoom()
install.packages("ggforce")
install.packages("ggsci")
library(ggforce)
ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
  geom_point() +
  facet_zoom(x = Species == "virginica") +
  ggsci::scale_color_startrek() # trying ggsci's startrek palette too


# ggbump offers a geom for creating bump charts — line-and-point charts 
install.packages("ggbump")
if(!require(pacman)) install.packages("pacman")
library(ggbump)
pacman::p_load(tidyverse, cowplot, wesanderson)

df <- tibble(country = c("India", "India", "India", "Sweden", "Sweden", "Sweden", "Germany", "Germany", "Germany", "Finland", "Finland", "Finland"),
             year = c(2011, 2012, 2013, 2011, 2012, 2013, 2011, 2012, 2013, 2011, 2012, 2013),
             value = c(492, 246, 246, 369, 123, 492, 246, 369, 123, 123, 492, 369))

knitr::kable(head(df))

ggplot(df, aes(year, rank, color = country)) +
  geom_point(size = 7) +
  geom_text(data = df %>% filter(year == min(year)),
            aes(x = year - .1, label = country), size = 5, hjust = 1) +
  geom_text(data = df %>% filter(year == max(year)),
            aes(x = year + .1, label = country), size = 5, hjust = 0) +
  geom_bump(size = 2, smooth = 8) +
  scale_x_continuous(limits = c(2010.6, 2013.4),
                     breaks = seq(2011, 2013, 1)) +
  theme_minimal_grid(font_size = 14, line_size = 0) +
  theme(legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "RANK",
       x = NULL) +
  scale_y_reverse() +
  scale_color_manual(values = wes_palette(n = 4, name = "GrandBudapest1"))ggplot(df, aes(x=year, y=rank, color = country)) +
  geom_point(size = 7) +
  geom_text(data = df %>% filter(year == min(year)),
            aes(x = year - .1, label = country), size = 5, hjust = 1) +
  geom_text(data = df %>% filter(year == max(year)),
            aes(x = year + .1, label = country), size = 5, hjust = 0) +
  geom_bump(size = 2, smooth = 8) +
  scale_x_continuous(limits = c(2010.6, 2013.4),
                     breaks = seq(2011, 2013, 1)) +
  theme_minimal_grid(font_size = 14, line_size = 0) +
  theme(legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "RANK",
       x = NULL) +
  scale_y_reverse() +
  scale_color_manual(values = wes_palette(n = 4, name = "GrandBudapest1"))


install.packages("gptstudio")
install.packages("pkgprompt")
