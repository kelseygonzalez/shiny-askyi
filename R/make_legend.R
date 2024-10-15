# # sites <- c("Old Main", "Gould Simpson")
# make_legend <- function(sites) {
#   colors <- gsi_site_colors[sites]
#   site_colors <-
#     map2(names(colors), colors, \(x, y) {
#       HTML(paste(bs_icon("circle-fill", color = y), x))
#     })
#   
#   HTML(glue::glue_collapse(site_colors, sep = " | "))
# }
# # make_legend(sites)