source('text_processing_functions.R')
source('vis_calculation_functions.R')
source('color_by_sentiment.R')

word_cloud = function(text_filename,
                      vscale = 1,
                      hscale = 1,
                      nmin_filter = 2,
                      draw_pretty = TRUE,
                      draw_bounding_boxes = FALSE,
                      color_by_sentiment = TRUE) {
  test = tokenize(text_filename)
  unique_test = filter_unique(test)
  unique_test = filter_stopwords(unique_test)
  weights = weight_by_counts(unique_test, test)
  weights = filter_rare(weights, nmin = nmin_filter)
  weights = sort_by_weight(weights)
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  bounding_boxes = bounding_box(colnames(weights),
                                weights/min(weights),
                                vscale = vscale,
                                hscale = hscale)
  results = place_words(weights, bounding_boxes)
  coordinates = results$coords
  bounding_boxes = results$boxes
  if (!draw_pretty) {
    draw_orig(coordinates, bounding_boxes, weights)
  }
  else
    if (color_by_sentiment)
      redraw_pretty(coordinates,
                    bounding_boxes,
                    weights,
                    word_color =
                      color_by_sentiment(colnames(weights)))
  else
    redraw_pretty(coordinates,
                  bounding_boxes,
                  weights,
                  word_color =
                    paste0('gray', sample(20:70, ncol(weights), replace =
                                            TRUE)))
  if (draw_bounding_boxes) {
    add_bounding_boxes(coordinates, bounding_boxes)
  }
  return(list(coords = coordinates, boxes = bounding_boxes, weights = weights))
}

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("Please provide space-separated text file", call. = FALSE)
}
if (length(args) == 1){
  word_cloud(args)
}
if (length(args) == 7){
  word_cloud(args[1], as.numeric(args[2]), as.numeric(args[3]), as.numeric(args[4]), 
             as.logical(args[5]), as.logical(args[6]), as.logical(args[7]))
}
if (length(args)>1 & length(args) !=7){
  stop("Please provide ONLY the filename, or all seven function arguments in order", call. = FALSE)
}
