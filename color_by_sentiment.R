color_by_sentiment = function(words_to_color, sent_colors = c('gray', 'red', 'purple', 'blue', 'pink',
                                                              'yellow', 'black')){
  sentiment_dictionary = read.delim('inqtabs.txt')
  negative = tolower(sentiment_dictionary$Entry[sentiment_dictionary$Negativ == 'Negativ'])
  positive = tolower(sentiment_dictionary$Entry[sentiment_dictionary$Positiv == 'Positiv'])
  male = tolower(sentiment_dictionary$Entry[sentiment_dictionary$MALE =='MALE'])
  female = tolower(sentiment_dictionary$Entry[sentiment_dictionary$Female =='Female'])
  nonadult = tolower(sentiment_dictionary$Entry[sentiment_dictionary$Nonadlt =='Nonadlt'])
  neutral = tolower(sentiment_dictionary$Entry[!(tolower(sentiment_dictionary$Entry) %in% positive) & 
                                               !(tolower(sentiment_dictionary$Entry) %in% negative) &
                                               !(tolower(sentiment_dictionary$Entry) %in% female) &
                                               !(tolower(sentiment_dictionary$Entry) %in% male) &
                                               !(tolower(sentiment_dictionary$Entry) %in% nonadult)])
  word_color = 1*(words_to_color %in% negative) + 
               2*(words_to_color %in% positive) + 
               3*(words_to_color %in% male) +
               4*(words_to_color %in% female) +
               5*(words_to_color %in% nonadult) +
               6*(words_to_color %in% neutral)
  word_color = word_color+1
  word_color = sent_colors[word_color]
  word_color[word_color == 'gray'] = paste0('gray', sample(20:70, sum(word_color=='gray'), replace=TRUE))
  return(word_color)
}