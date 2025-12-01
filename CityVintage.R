# https://github.com/koenderks/rcityviews?tab=readme-ov-file#create-your-own-in-r

library(rcityviews)

# Try with wait time between requests
p <- cityview(name = "Bochum", zoom = 5, timeout = 300)  # 5 min timeout
ggplot2::ggsave(filename = "Bochum.png", plot = p, 
                height = 500, width = 500, units = "mm", dpi = 100)