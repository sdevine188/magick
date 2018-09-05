library(magick)
library(dplyr)
library(ggplot2)
library(scales)
library(tesseract)
library(pdftools)

# https://cran.r-project.org/web/packages/magick/vignettes/intro.html

# setwd
setwd("H:/R/magick")

# build a legend in ggplot and add overlay on pdf image of data.tree using magick

# get color hex values
show_col(viridis_pal()(10))
show_col(viridis_pal()(100))
viridis_pal()(100)

# create data for plot
viridis_percentage_scale_tbl <- tibble(x = seq(from = .01, to = 1.00, by = .01), y = seq(from = 1, to = 100, by = 1))

# create plot
plot_w_legend <- viridis_percentage_scale_tbl %>% ggplot(data = ., aes(x = x, y = y, color = x)) + geom_point() + 
        scale_color_viridis_c(labels = percent) + 
        guides(color = guide_colorbar(title = "IBFA denial rate"))
plot_w_legend

# save plot as png
ggsave(filename = "plot_w_legend_png.png", plot = plot_w_legend, dpi = 300)

# load png into magick
plot_w_legend_image <- image_read(path = "plot_w_legend_png.png")
plot_w_legend_image

# snip out legend
# image_crop(image, "100x150+50+100"): crop out width:100px and height:150px starting +50px from the leftmost, 
# + 50px from topmost
legend <- image_crop(image = plot_w_legend_image, "700x950+1100+550")
legend 

# save legend
image_write(image = legend, path = "legend.png", format = 'png', density = '300x300')
image_write(image = legend, path = "legend.pdf", format = 'pdf', density = '300x300')

# overlay legend on new plot
image_composite(image = image_border(image = plot_w_legend_image, color = "white", geometry = "500x500"), 
                composite_image = image_resize(image = legend, geometry = "400x400"), offset = "+180+800")


###############################################################################################3
###############################################################################################3
###############################################################################################3


# copied from tesseract_magick_scratchpad in tesseract repo

# https://cran.r-project.org/web/packages/tesseract/vignettes/intro.html#preprocessing_with_magick

setwd("H:/R/tesseract")
list.files()

test_image <- ocr("test_ocr_image.png", engine = eng)
test_image
cat(test_image)

eng <- tesseract("eng")
test_image_data <- ocr_data("test_ocr_image.png", engine = eng)
cat(test_image)
test_image_data
test_image_data %>% arrange(confidence)
test_image_data %>% mutate(row_id = row_number()) %>% filter(word == "cor")


####################################################


ca_1 <- ocr("scanned_docs/california1.pdf")
ca_1
cat(ca_1)

ca_1_data <- ocr_data("scanned_docs/california1.pdf")
ca_1_data
ca_1_data %>% arrange(confidence)
mean(ca_1_data$confidence) # 74.11


#######################################################


# with magick preprocessing - better results
ca_1_png <- pdf_convert("scanned_docs/california1.pdf", dpi = 600)
ca_1_text <- image_read(ca_1_png) %>%
        image_resize("2000x") %>%
        image_convert(type = 'Grayscale') %>%
        image_trim(fuzz = 40) %>%
        image_write(format = 'png', density = '300x300') %>%
        ocr(.) 
cat(ca_1_text)

ca_1_text_data <- image_read(ca_1_png) %>%
        image_resize("2000x") %>%
        image_convert(type = 'Grayscale') %>%
        image_trim(fuzz = 40) %>%
        image_write(format = 'png', density = '300x300') %>%
        ocr_data(.) 
ca_1_text_data %>% arrange(confidence)
mean(ca_1_text_data$confidence) # 83.00