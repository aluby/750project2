parse_txt = function(filename){
  
}

tokenize = function(docname) {
  document = read.table(docname, colClasses = "character", comment.char = "")
  return(as.matrix(document))
}

filter_unique = function(token.doc){
  return(unique(token.doc, MARGIN=2))
}

filter_stopwords = function(unique.token.doc, stopword_csv){
  stopwords = read.csv(stopword_csv, header=FALSE)
  stopwords = as.matrix(stopwords)
  return(unique.token.doc[!unique.token.doc %in% stopwords])
}

weight_by_counts = function(final.token.doc){
  
}