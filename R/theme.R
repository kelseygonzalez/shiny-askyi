library(ggplot2)


ibm_colors <- c("#001141","#002d9c","#0043ce","#4589ff","#d0e2ff","#262626","#525252","#c6c6c6")
ibm_red = c("#2d0709", "#520408", "#750e13", "#a2191f", "#da1e28", "#fa4d56", "#ff8389", "#ffb3b8", "#ffd7d9", "#fff1f1")
ibm_megenta = c("#2a0a18", "#510224", "#740937", "#9f1853", "#d02670", "#ee5396", "#ff7eb6", "#ffafd2", "#ffd6e8", "#fff0f7")
ibm_purple = c("#1c0f30", "#31135e", "#491d8b", "#6929c4", "#8a3ffc", "#a56eff", "#be95ff", "#d4bbff", "#e8daff", "#f6f2ff")
ibm_blue = c("#001141", "#001d6c", "#002d9c", "#0043ce", "#0f62fe", "#4589ff", "#78a9ff", "#a6c8ff",
             "#d0e2ff", "#edf5ff")
ibm_cyan = c("#061727", "#012749", "#003a6d", "#00539a", "#0072c3", "#1192e8", "#33b1ff", "#82cfff", "#bae6ff", "#e5f6ff")
ibm_teal = c("#081a1c", "#022b30", "#004144", "#005d5d", "#007d79", "#009d9a", "#08bdba", "#3ddbd9", "#9ef0f0", "#d9fbfb")
ibm_green = c("#071908", "#022d0d", "#044317", "#0e6027", "#198038", "#24a148", "#42be65", "#6fdc8c", "#a7f0ba", "#defbe6")
ibm_black = c("#000000", "#121619", "#21272a", "#343a3f", "#4d5358", "#697077", "#878d96", "#a2a9b0", "#c1c7cd", "#dde1e6", "#f2f4f8", "#ffffff")



ibm_theme <- theme_minimal() +
  theme(plot.title.position = "plot",
        legend.position = "right",
        text=element_text(size=12),
        plot.title=element_text(face="bold",
                                size=16,
                                color = ibm_colors[1]),
        plot.subtitle = element_text(face="italic",
                                     size=13,
                                     color = ibm_colors[6]),
        axis.text = element_text(size = 9,
                                 color = ibm_colors[7]),
        axis.title  = element_text(size=10,
                                   color = ibm_colors[7]),
        strip.text = element_text(size = 10,
                                  color = ibm_colors[7]))
theme_set(ibm_theme)
