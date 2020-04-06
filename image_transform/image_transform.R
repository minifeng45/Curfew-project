# lead in the image operation library
library(magick) 

# insert the image which you want to transform

image_transform = function(pathin="",
                           width = 100,
                           chars = c(".",""),
                           cex = 1.0,
                           pin = c(5,5)){
img = image_read(pathin) #insert image from your working directory
gray = image_convert(img, colorspace='gray') # transform colorful scale to gray scale
gray = image_resize(gray, paste0(width,'x')) # set the image size, ex: 1280x960, 720x 480
# Note: it depends on your image original size, because it would follow the scale of your image
gray = as.integer(image_data(gray))[, , 1] #image to be vectorized
w = ncol(gray) # set the width
h = nrow(gray) # set the height
index = findInterval(c(gray),                                                                   
                     seq(0, 255, length.out=length(chars)+1), #divide 0-255 with chars number +1
                     rightmost.closed=T)
labels <- chars[index] #different interval value as different labels
labels_mat <- matrix(labels, ncol=w) # pack as matrix

###Draw plot
par(pin = pin) #set the plot frame size. 
#Note, if you use R studio, make sure the space for plot is enough(amplify the window of plot)
#Basic setting for plot
plot(0,
     xlab='Predicted',
     ylab='Residual',
     xlim=c(0,w),
     ylim=c(0,h),
     asp = 1,
     xaxs="i",
     yaxs="i",
     type='n',
     axes = FALSE,
     mgp = c(0.3,1,0))
grid <- expand.grid(y = h:1, x = 1:w)   #set the grid based on the image size
text(grid$x, grid$y, labels, cex=cex)   #dot the "labels" on the grid
#make the border of the plot
text(0,c(1:h),".",cex=cex)  #left line
text(w,c(1:h),".",cex=cex)  #right line
text(c(1:w),0,".",cex=cex)  #bottom line
text(c(1:w),h,".",cex=cex)  #top line
text(sample(1:w,300,replace = T),
     sample(1:h,300,replace = T),".",cex=cex) #random dot some point on the plot for emulation
}

#demo
image_transform(pathin = "selfpofile.JPG", width = 120,pin = c(4,4))
