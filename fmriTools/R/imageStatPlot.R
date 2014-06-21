##creates a nice plot of an statistical image overlaying a mask
##everything is in masked vector space, not image space
##uses ggplot2
## Brian Caffo 6-20-2014

##
# imageStatPlot <- function(imgVec, kVal, imgDim, mask, bgImg, threshold = 0, bg = "black") {
#     bg2d <- vec2img(vec=bgImg, imgDim, mask = mask, pad=NA)[,,kVal]
#     temp <- imgVec
#     temp[abs(imgVec) < threshold] <- NA 
#     img2d <- vec2img(vec=temp, imgDim, mask = mask, pad=NA)[,,kVal]
# 
#     
#     image(bg2d, frame = FALSE, 
#           axes = FALSE, 
#           bg = "black", 
#           col = grey(seq(1, 0, length = 12)),
#           asp = 1
#           )
#     image(img2d, add = TRUE)
#     
#     cut(temp[,,45], breaks = imgBreaks, bg = bg)
# }



imageStatPlot <- function(imgVec, imgDim, mask, bgVec, ...) {
    bg3d <- vec2img(vec=bgVec, imgDim, mask = mask, pad=NA)
    img3d <- vec2img(vec=imgVec, imgDim, mask = mask, pad=NA)

    myIcaPlot <- function(k, t){
        bg2d <- bg3d[,,k]
        img2d <- img3d[,,k]
        img2d[abs(img2d) < t] <- NA
        
        image(bg2d, frame = FALSE, 
              axes = FALSE, 
              bg = "black", 
              col = grey(seq(1, 0, length = 12)),
              asp = 1
              )
        image(img2d, add = TRUE, ...)
    }
    
    manipulate(myIcaPlot(k, cut), 
           k = slider(1, imgDim[3], initial = round(imgDim[3] / 2)),
           cut = slider(0, 4, step = .1, initial = 2))

}

# imageStatPlot <- function(img, kVal, imgDim, mask, bgImg, threshold = 0) {
#     xy <- ind2sub(dims=imgDim,indexVal=mask)
#     idx <- xy[,3] == kVal    
#     sub <- (abs(img) > threshold)[idx]
#     
#     bgImg[bgImg == 0] <- NA
#     dat <- data.frame(xy[idx,], 
#                       mask = 1, 
#                       img = img[idx], 
#                       bgImg = bgImg[idx], 
#                       sub = (abs(img) > threshold)[idx])
#     
#     dat <- rbind(data.frame(xy[idx,], img = bgImg[idx], bg = 1),
#           data.frame(xy[sub,], img = img[sub], bg = 0))
# 
#     ggplot(dat, aes(x = i, y = j, fill = factor(bg), alpha = img)) + geom_tile() #+ scale_alpha(range = c(0.2, 0.5))
#     
#     ggplot(dat, aes(x = i, y = j, fill = bgImg)) + geom_tile()
#                
#     scale_fill_gradient(low="white", high="black")
#     
#     p <- ggplot(dat, aes(x = i, y = j)) 
#     p + geom_tile(aes(alpha = bgImg)) + scale_alpha(range = c(0, 0.5))
#     p + geom_tile(aes(width = 1, height = 1, fill = img), data = dat[dat$sub,]) + scale_fill_gradient(low = "#132B43", high = "#56B1F7")
# 
# }
# 
# myIcaPlot <- function(q, k, t){
#     imageStatPlot(out$S[,q], k, imgDim, mask, bgImg, t)
# }
# 
# manipulate(myIcaPlot(q, k, cut), 
#            q = slider(1, Q), 
#            k = slider(1, imgDim[3], initial = 45),
#            cut = slider(0, 4, step = .1))
