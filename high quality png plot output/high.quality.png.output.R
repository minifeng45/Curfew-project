###FUNCTION: high quality png.file output
png.output = function(plot = p,
                      filename = "",
                      width = 1600,
                      height = 1600,
                      res = 500,
                      family = "GB1"){
  png(
    filename = filename, # must .png!!!
    type = "cairo", # avoid zigzag
    res = res, # resolution
    width = width, height = height,
    bg = "transparent", # transparent background 
    family=family #Font change
  )
  return(plot)
}
dev.off()