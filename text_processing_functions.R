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

weight_by_counts = function(filtered.token, original.token){
  counts = apply(as.matrix(filtered.token), 1, function(unique_row)
    {return(sum(unique_row == original.token))})
  counts = t(as.matrix(counts))
  colnames(counts) = filtered.token
  return(counts)
}

sort_by_weight = function(weights){
  return(sort(weights, decreasing=TRUE))
}
filter_rare = function(weighted_words, nmin = 1){
  return(weighted_words[,which(weighted_words>nmin)])
}