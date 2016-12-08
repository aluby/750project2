bounding_box = function(word, weight, hscale = 1, vscale = 1) {
  return(list(
    height = vscale * strheight(word) * weight,
    width = hscale * strwidth(word) * weight
  ))
}

# IDEA:
# for(word in weighted_words){
#  find_coords(word)
#  while(overlaps) adjust_coords(word)
#  plot(word)
# }

find_coords = function(word_index) {
  if (word_index == 1) {
    return(list(
      x_coord = runif(1, min = 0, max = 1),
      y_coord = .5
      ))
  }
  return(list(
    x_coord = runif(1, min = 0, max = 1),
    y_coord = runif(1, min = 0, max = 1)
  ))
}

overlaps = function(test_x_coord,
                    test_y_coord,
                    coords,
                    boxes,
                    word_index) {
  test_box_xrange = c(test_x_coord, test_x_coord + boxes$width[word_index])
  test_box_yrange = c(test_y_coord, test_y_coord + boxes$height[word_index])
  other_words_xrange = t(rbind(coords$x_coord, coords$x_coord + boxes$width))
  other_words_yrange = t(rbind(coords$y_coord, coords$y_coord + boxes$height))
  overlaps_x = test_box_xrange[1] < other_words_xrange[, 2] &
    test_box_xrange[2] > other_words_xrange[, 1]
  overlaps_y = test_box_yrange[1] < other_words_yrange[, 2] &
    test_box_yrange[2] > other_words_yrange[, 1]
  return(overlaps_x & overlaps_y)
}

spiral_points = function(x_orig,
                         y_orig,
                         change = .1,
                         nrotations = 100) {
  t = seq(0, nrotations * 2 * pi, by = change)
  r = t / max(t)
  return(cbind(x = r * cos(t) + x_orig, y = r * sin(t) + y_orig))
}

place_words = function(weighted_words, boxes) {
  coords = list(x_coord = rep(NA, length(weighted_words)),
                y_coord = rep(NA, length(weighted_words)))
  for (word in 1:length(weighted_words)) {
    test_coords = find_coords(word)
    tries = 1
    overlap = overlaps(test_coords$x_coord,
                       test_coords$y_coord,
                       coords,
                       boxes,
                       word)
    points_to_try = spiral_points(test_coords$x_coord, test_coords$y_coord, change = .2)
    while (sum(overlap, na.rm = TRUE) > 0 & tries <= nrow(points_to_try)) {
      test_coords$x_coord = points_to_try[tries, 1]
      test_coords$y_coord = points_to_try[tries, 2]
      overlap = overlaps(test_coords$x_coord,
                         test_coords$y_coord,
                         coords,
                         boxes,
                         word)
      tries = tries + 1
    }
    coords$x_coord[word] = test_coords$x_coord
    coords$y_coord[word] = test_coords$y_coord
    if (tries > nrow(points_to_try)) {
      print(paste('Word could not be placed: ', colnames(weighted_words)[word]))
      coords$x_coord[word] = 0
      coords$y_coord[word] = 0
      boxes$width[word] = 0
      boxes$height[word] = 0
    }
    
  }
  return(list(coords = coords, boxes = boxes))
}

add_bounding_boxes = function(coords, boxes){
  for(ind in 1:length(coords$x_coord)){
    if(!is.na(coords$x_coord[ind]))
    rect(
      xleft = coords$x_coord[ind],
      xright = coords$x_coord[ind] + boxes$width[ind],
      ybottom = coords$y_coord[ind],
      ytop = coords$y_coord[ind] + boxes$height[ind]
    )
  }
}

draw_orig = function(coords, boxes, weights){
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  coords$x_coord[boxes$width == 0] = -50
    text(
      x = coords$x_coord,
      y = coords$y_coord,
      labels = colnames(weights),
      cex = boxes$width / strwidth(colnames(weights)),
      adj = c(0,0)
    )
}

redraw_pretty = function(coords, boxes, weights, add_boxes = FALSE, word_color = NULL){
  max_x = max(coords$x_coord + boxes$width)
  min_x = min(coords$x_coord)
  max_y = max(coords$y_coord + boxes$height)
  min_y = min(coords$y_coord)
  #dev.new(width = (max_x - min_x)*2, height = (max_y - min_y))
  png('images/word_cloud_pretty.png')
  plot.new()
  plot.window(xlim = c(min_x, max_x), ylim = c(min_y, max_y))
  coords$x_coord[boxes$width == 0] = -50
  text(x = coords$x_coord, 
       y = coords$y_coord + .25 * boxes$height, 
       labels = colnames(weights), 
       cex = boxes$width / strwidth(colnames(weights)),
       #lheight = boxes$height,
       #ps = weights,
       adj = c(0,0),
       col = word_color)
  if(add_boxes){add_bounding_boxes(coords, boxes)}
  dev.off()
}
