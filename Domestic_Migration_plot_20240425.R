



library(zoo)
library(data.table)
library(tidyverse)
library(gglaplot)
library(scales)
library(readxl)
library(knitr)
library(sf)
library(sp)
library(kableExtra)
library(magick)

library(plotly)    
library(RColorBrewer)
library(htmltools)
library(prettydoc)
library(rmdformats)
library(orca)
library(processx)
library(svglite)



data_dir <- 'C:/Migration/Migration_R/DATA/Domestic/copied_from_Qdrive_20240213/'
chart_dir <- 'CHARTS/'

# Disable scientific notation
options(scipen=999)

?svglite


  ## FOR SECTION 2 on trends (see dom_migration_longit_20220708.R)

# Import data for longitudinal chart 
london_longit_net_wide2 <- fread(paste0(data_dir, 
"london_domMig_net_longit_wide2.csv")) %>% 
  data.frame()  


# chart with no y axis title for families report (edited 25 April 2024 to remove caption and title)


london_longit_net_int2  <- london_longit_net_wide2 %>% 
  mutate(sumvalue_in = sumvalue_inK*1000) %>% 
  mutate(sumvalue_out = sumvalue_outK*1000) %>% 
  mutate(sumvalue_net = sumvalue_netK*1000) %>%
  plot_ly(x=~year, y=~sumvalue_inK, text = ~sumvalue_in,
          type = "bar",
          color = I("#943fa6"), 
          name = "Migration to London", 
          hovertext = ~formatC(sumvalue_in, big.mark = ",",
                                       format = "d"),   # integer (no decimal)
          hovertemplate = paste("Year: %{x|%Y}",
                   #   "<br>","Migrants to London: %{y:.0f}<extra></extra>")) %>%
                    "<br>","Migrants to London: %{hovertext}<extra></extra>")) %>%
      #add additional series as 'trace' variables
  add_trace(y = ~sumvalue_outK, type = "bar", text = ~sumvalue_out, color = I('#63c5b5'),
            name = 'Migration from London',
            hovertext = ~formatC(sumvalue_out, big.mark = ",",
                                       format = "d"),   # integer (no decimal)
            hovertemplate = paste("Year: %{x|%Y}",
                                  "<br>","Migrants from London: %{hovertext}<extra></extra>")) %>% 
  add_trace(y = ~sumvalue_netK, 
            name = "Net domestic migration",
            type = "scatter",
            mode = "line", 
            line = list(shape = "spline", color = "black"),
            hovertext = ~formatC(sumvalue_net, big.mark = ",",
                                       format = "d"),   # integer (no decimal)
            hovertemplate = paste("Year: %{x|%Y}",
                                  "<br>","London's net domestic migration: %{hovertext}<extra></extra>")) %>%
  layout(
      #title
      title= list(x = 0.05,
    #     text = paste0("<b>Domestic Migration, London<b>"), 
                 text = paste0(""),   # added
         font=list(size = 15, family = "Arial")),
         font=list(size = 14, family = "Arial", color = "black", fontface = "bold"),
    #axis options
    xaxis = list(title = "", showgrid = FALSE,
                 tick0=2002, dtick=5),
    yaxis = list(title = "", 
                 showgrid = FALSE,
                 titlefont = list(size = 14), #ticksuffix = "K"
                 tickfont = list(size = 14)),
          #legend
    legend = list(orientation = "h",   # show entries horizontally
                  x = 0, y = 1.05, # position
                  font = list(size = 14),
                  bgcolor = 'rgba(0,0,0,0)'), # legend background transparent
      #Source at bottom of chart
    # annotations = list(x = 0, y = -0.2, text = "Source: ONS, Chart: GLA demography", 
     annotations = list(x = 0, y = -0.2, text = "", 
                       showarrow = F, xref='paper', yref='paper', 
                       xanchor='left', yanchor='auto', xshift=0, yshift=0,
                       font=list(size=14))) %>%
    layout(margin = list(l = 50, r = 50, b = 80, t = 50))

london_longit_net_int2

#svglite("C:/Families/Charts_for_Daryl/22_B_dom_mig_longit_net.svg", width = 4, height = 4, main = #"london_longit_net_int2")
#dev.off()



#plotly::export(p = london_longit_net_int2, file = (paste0(chart_dir, "dom_mig_longit_net.png")))




#plotly::export(p = london_longit_net_int2, file = "dom_mig_longit_net.png")

#london_longit_net_int2.write_image("C:/Families/Charts_for_Daryl/22_B_dom_mig_longit_net.svg"


#orca(london_longit_net_int2, "C:/Families/Charts_for_Daryl/22_B_dom_mig_longit_net.svg")

