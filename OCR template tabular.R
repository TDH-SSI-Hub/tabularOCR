column.names<-c()
geometries<-c()
engines<-c()

folder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(folder)

readval<-readRDS('inputs.RDS')
values<-list()
input<-readval[[3]]
values$test.all<-readval[[1]]
values$active.forms<-readval[[2]]
values$form.pages<-readval[[4]]
values$col.geom.all<-readval[[5]]
values$col.engine.all<-readval[[6]]

values$col.geom.all[['c1']]

library('magick')
library('tesseract')

source('OCR functions.r')

all.pdfs<-list.files(pattern='.pdf')

base.df<-read.pdf(pdf=image_read_pdf(all.pdfs),forms=unlist(values$active.forms),classes=input,pages=values$form.pages, geoms=values$col.geom.all,engine=values$col.engine.all )

write.csv(base.df,'lab_output.csv')






