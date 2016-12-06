parse_txt = function(filename){
  
}

tokenize = function(docname) {
  document = read.table(docname, colClasses = "character", comment.char = "")
  return(t(as.matrix(tolower(document))))
}

filter_unique = function(token.doc){
  return(unique(token.doc, MARGIN=2))
}

filter_stopwords = function(unique.token.doc, stopword_csv = 'other-text-files/stopwords_xpo6.csv'){
  stopwords = read.csv(stopword_csv, header=FALSE, colClasses = "character")
  stopwords = as.matrix(stopwords)
  return(t(as.matrix(unique.token.doc[!unique.token.doc %in% stopwords])))
}

filter_rare = function(weighted_words, nmin = 1){
  return(t(as.matrix(weighted_words[,which(weighted_words>nmin)])))
}

weight_by_counts = function(filtered.token, original.token){
  counts = apply(as.matrix(filtered.token), 2, function(unique_row)
    {return(sum(unique_row == original.token))})
  counts = t(as.matrix(counts))
  colnames(counts) = filtered.token
  return(counts)
}

sort_by_weight = function(weights){
  sorted = sort(weights, decreasing=TRUE, index=TRUE)
  result = t(as.matrix(sorted$x))
  colnames(result) = colnames(weights)[sorted$ix]
  return(result)
}
