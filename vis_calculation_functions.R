bounding_box = function(word, hscale = 1, vscale = 1) {
  return(list(
    height = vscale * strheight(word),
    width = hscale * strwidth(word)
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
      x_coord = runif(1, min = -.5, max = .5),
      y_coord = 0
    ))
  }
  return(list(
    x_coord = runif(1, min = -.5, max = .5),
    y_coord = runif(1, min = -.5, max = .5)
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
  plot.new()
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
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
    if (tries > nrow(points_to_try)) {
      print(c('Fail ', word))
    }
    coords$x_coord[word] = test_coords$x_coord
    coords$y_coord[word] = test_coords$y_coord
    text(
      x = coords$x_coord[word],
      y = coords$y_coord[word],
      labels = colnames(weighted_words)[word],
      cex = weighted_words[word]/min(weighted_words),
      adj = c(0,0)
      #cex = .01
    )
  }
  return(coords)
}

add_bounding_boxes = function(coords, boxes){
  for(ind in 1:length(coords$x_coord)){
    rect(
      xleft = coords$x_coord[ind],
      xright = coords$x_coord[ind] + boxes$width[ind],
      ybottom = coords$y_coord[ind],
      ytop = coords$y_coord[ind] + boxes$height[ind]
    )
  }
}

redraw_pretty = function(coords, boxes, weights){
  max_x = max(coords$x_coord + boxes$width)
  min_x = min(coords$x_coord)
  max_y = max(coords$y_coord + boxes$height)
  min_y = min(coords$y_coord)
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  new_xleft = (coords$x_coord + abs(min_x))/2
  new_ybottom = (coords$y_coord + abs(min_y))/2
  text(x = new_xleft, 
       y = new_ybottom, 
       labels = colnames(weights), 
       cex = (weights/min(weights))/2,
       adj = c(0,0))
}