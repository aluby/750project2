source('text_processing_functions.R')
source('vis_calculation_functions.R')
source('color_by_sentiment.R')

driver = function(text_filename, vscale = 1, hscale = 1, nmin_filter = 1, draw_pretty = TRUE,
                  draw_bounding_boxes = FALSE, color_by_sentiment = FALSE) {
  test = tokenize(text_filename)
  unique_test = filter_unique(test)
  unique_test = filter_stopwords(unique_test, 'stopwords_xpo6.csv')
  weights = weight_by_counts(unique_test, test)
  weights = filter_rare(weights, nmin = nmin_filter)
  weights = sort_by_weight(weights)
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  bounding_boxes = bounding_box(colnames(weights), weights / (min(weights) *
                                                                2), vscale = vscale, hscale = hscale)
  coordinates = place_words(weights, bounding_boxes)
  if (!draw_pretty) {
    draw_orig(coordinates, bounding_boxes, weights)
  }
  else
    if (color_by_sentiment)
      redraw_pretty(coordinates, bounding_boxes, weights, word_color =
                      color_by_sentiment(colnames(weights)))
  else
    redraw_pretty(coordinates, bounding_boxes, weights, word_color =
                    paste0('gray', sample(20:70, ncol(weights), replace =
                                            TRUE)))
  if (draw_bounding_boxes) {
    add_bounding_boxes(coordinates, bounding_boxes)
  }
}
