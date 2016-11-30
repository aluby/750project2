bounding_box = function(word, hscale=1, vscale=1){
  return(list(height = vscale*strheight(word), width = hscale*strwidth(word)))
}

# IDEA: 
# for(word in weighted_words){
#  find_coords(word)
#  while(overlaps) adjust_coords(word)
#  plot(word)
# }

find_coords = function(word){
  if(word==1){
  return(list(x_coord = runif(1, min = -1, max = 1), y_coord = 0))
  }
  return(list(x_coord = runif(1, min = -1, max = 1), y_coord = runif(1, min = -1, max = 1)))
}

overlaps = function(test_x_coord, test_y_coord, coords, boxes, word_index){
  test_box_xrange = c(test_x_coord, test_x_coord + boxes$width[word_index])
  test_box_yrange = c(test_y_coord, test_y_coord + boxes$height[word_index])
  other_words_xrange = cbind(coords$x_coord, coords$x_coord + boxes$width)
  other_words_yrange = cbind(coords$y_coord, coords$y_coord + boxes$height)
  overlaps_x = test_box_xrange[1] < other_words_xrange[,2] &
    test_box_xrange[2] > other_words_xrange[,1]
  overlaps_y = test_box_yrange[1] < other_words_yrange[,2] &
    test_box_yrange[2] > other_words_yrange[,1]
  return(overlaps_x & overlaps_y)
}

to_polar = function(x, y){
  r = sqrt(x^2 + y^2)
  t = atan(y/x)
  return(c(r,t))
}

spiral_points = function(x_orig, y_orig, change = .1, nrotations = 100000){
  pol = to_polar(x_orig,y_orig)
  t = seq(pol[2], nrotations * 2 * pi, by=change)
  r = t/max(t)
  return(cbind(x=r*cos(t)+x_orig, y = r* sin(t) + y_orig))
}

adjust_coords = function(test_x_coord, test_y_coord, points_to_try){
  return(list(x_coord = test_x_coord + .01, y_coord = test_y_coord+.01))
}

place_words = function(weighted_words, boxes){
  coords = list(x_coord = rep(NA, length(weighted_words)), y_coord = rep(NA, length(weighted_words)))
  plot.new()
  plot.window(xlim = c(-1, 1), ylim = c(-1,1))
  for(word in 1:length(weighted_words)){
    test_coords = find_coords(word)
    tries = 1
    overlap = overlaps(test_coords$x_coord, test_coords$y_coord, coords, boxes, word)
    points_to_try = spiral_points(test_coords$x_coord, test_coords$y_coord, change = .5)
    while(sum(overlap, na.rm = TRUE)>0 & tries<100000){
      test_coords$x_coord = points_to_try[tries,1]
      test_coords$y_coord = points_to_try[tries,2]
      overlap = overlaps(test_coords$x_coord, test_coords$y_coord, coords, boxes, word)
      tries = tries + 1
    }
    if(tries>99999){print(c('Fail ', word))}
    coords$x_coord[word] = test_coords$x_coord
    coords$y_coord[word] = test_coords$y_coord
    text(x = coords$x_coord[word], y = coords$y_coord[word], labels = names(weighted_words)[word], 
         #cex = weighted_words[word]/max(weighted_words)*5
         #cex = .01
         )
  }
  return(coords)
}