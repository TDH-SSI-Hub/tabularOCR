initial.columns<-c('GLcode','Desc','Amount1','Amount2')

hline <- function(y = 0, color = "black", size=1) {
  list(type = "line",x0 = 0,x1 = 1,xref = "paper",y0 = y,y1 = y,
       line = list(color = color, width=size)
  )
}

vline <- function(x = 0, color = "black", size=1) {
  list(type = "line",x0 = x,x1 = x,yref = "paper",y0 = 0,y1 = 1,
       line = list(color = color, width=size)
  )
}

detect.lines<-function(pdf, tmarg, bmarg){
  clean.lines<-pdf %>%
    image_canny("0x1+10%+30%") %>% image_hough_txt('300x70+500') %>%
    strsplit('\n') %>% unlist() %>% .[4:length(.)] %>%
    str_extract_all('\\d+', simplify = T)  %>% as.data.frame()
  
  colnames(clean.lines)<- c('x1','y1','x2','y2','count','angle','distance')
  clean.lines<-apply(clean.lines,c(1,2), as.numeric) %>% as.data.frame()
  clean.lines<-clean.lines$y1[clean.lines$angle %in% 87:93 & clean.lines$y1 %in% tmarg:bmarg]
  return(clean.lines)
}


lab.elements<-c('Name','First.Name','Last.Name','DOB','Address','Address2','City','ZIP','County','Spec.Col.Dt','Accession','Test','Result')
space<-' '
lowercase<-'abcdefghijklmnopqrstuvwxyz'
uppercase<-toupper(lowercase)
numbers<-'0123456789'
punctuation<-':()/-.,'
engines<-list(Space=space,a_z=lowercase,A_Z=uppercase,Num=numbers,Punc=punctuation)

get.string<-function(string){
  paste0("'",paste0(unlist(string), collapse = "','"),"'")
}

read.element<-function(image,geometry,engine){
  if (!is.na(geometry)){
    if (!is.na(engine)){
      engine1<-tesseract(options = list(tessedit_char_whitelist = engine))
    }else{engine1<-'eng'}
    gsub('\n','', ocr(image_crop(image,geometry),engine = engine1))
  }else{NA}
}

read.col<-function(read.page,geom.vec, engine){
  if(length(geom.vec)>0){
    return(sapply(geom.vec, function(g) read.element(image=read.page,geometry=g,engine=engine) ))
  }else{return(NA)}
}


read.page<-function(pdf,form,classes,geoms, engine){
  report.df<-data.frame('Class'=classes[[paste0(form,'name')]]
                        ,'GLcode'=read.col(geom.vec=geoms[[form]][['GLcode']],engine=engine[[form]][['GLcode']],read.page=pdf)
                        ,'Desc'=read.col(geom.vec=geoms[[form]][['Desc']],engine=engine[[form]][['Desc']],read.page=pdf)
                        ,'Amount1'=read.col(geom.vec=geoms[[form]][['Amount1']],engine=engine[[form]][['Amount1']],read.page=pdf)
                        ,'Amount2'=read.col(geom.vec=geoms[[form]][['Amount2']],engine=engine[[form]][['Amount2']],read.page=pdf)
  )
  rownames(report.df)<-NULL
  return(report.df)
}



read.form<-function(pdf,form,pages,...){#classes,geoms, engine
  rbindlist(lapply(pages[[form]], function(p) read.page(pdf=pdf[p],form=form,...)))
}

read.pdf<-function(pdf,forms,...) {#classes, pages,geoms, engine
  rbindlist(lapply(forms, function(f) read.form(pdf=pdf,form=f,...)))
}

read.pdf2<-function(pdf,forms,...) {#classes, pages,geoms, engine
  rbindlist(lapply(forms, function(f) read.form(pdf,f,...)))
}

sum.gl.code<-function(glc){
  sum(OCR$amount[OCR$Mapping==glc&!is.na(OCR$Mapping)], na.rm = T)
}

create.geom.all<-function(left,right,rows, buffer){
  brows<-rows[2:length(rows)]
  size<-paste0(right-left-buffer*2,'x',brows-rows[1:(length(rows)-1)]-buffer*2)
  loc<-paste0("+",left+buffer,"+",rows[1:(length(rows)-1)]+buffer)
  paste0(size,loc)
}