library(testthat)
source('text_processing_functions.R')
source('vis_calculation_functions.R')

test_that("tokenize(test) returns 1xn matrix", {
  write.table(c('Now is the time to head west toward wilder lands'), 
              file='test.txt', col.names=FALSE, row.names=FALSE, quote=FALSE)
  expect_that(dim(tokenize('test.txt')), equals(c(1,10)))
  expect_that(class(tokenize('test.txt')), equals('matrix'))
})

test_that("tokenize() throws error on file that DNE", {
  expect_error(tokenize('test1.txt'))
})

#Sample random files to test functions
art_file = sample(list.files('nyt-collection-text/art'), 1)
music_file = sample(list.files('nyt-collection-text/music'),1)

test_that("tokenize(random_file) is a matrix with 1 row and at least 1 column",{
  expect_that(nrow(tokenize(paste0('nyt-collection-text/art/', art_file))), 
              equals(1))
  expect_gt(ncol(tokenize(paste0('nyt-collection-text/art/', art_file))), 
            1)
  expect_that(class(tokenize(paste0('nyt-collection-text/art/', art_file))),
              equals('matrix'))
  expect_that(nrow(tokenize(paste0('nyt-collection-text/music/', music_file))), 
              equals(1))
  expect_gt(ncol(tokenize(paste0('nyt-collection-text/music/', music_file))), 
            1)
  expect_that(class(tokenize(paste0('nyt-collection-text/music/', music_file))),
              equals('matrix'))
})

art_token = tokenize(paste0('nyt-collection-text/art/', art_file))
music_token = tokenize(paste0('nyt-collection-text/music/', music_file))

test_that("filter_unique(random_token) returns 1xn matrix with no repeats",{
  unique_art = filter_unique(art_token)
  expect_equal(dim(unique_art)[1], 1)
  expect_gte(dim(unique_art)[2], 1)
  expect_equal(class(unique_art), 'matrix')
  for(ind in 1:dim(unique_art)[2]){
    expect_false(unique_art[1,ind] %in% unique_art[1,-ind])
  }
})

unique_art = filter_unique(art_token)

test_that("filter_stopwords(random_token, stopwords) returns 1xn matrix with no stopwords",{
  stopwords = 'the,vision,art,artist'
  write.table(stopwords, 'test-stopwords.csv', sep = ",", quote = FALSE, row.names = FALSE, col.names = FALSE)
  stop_art = filter_stopwords(art_token, stopword_csv = 'test-stopwords.csv')
  expect_equal(dim(stop_art)[1], 1)
  expect_gte(dim(stop_art)[2], 1)
  expect_equal(class(stop_art), 'matrix')
  expect_equal(sum(c('the', 'vision', 'art', 'artist') %in% stop_art), 0)
})

filtered_art = filter_stopwords(unique_art, stopword_csv = 'test-stopwords.csv')

test_that("weight_by_count(filtered, random) returns 1xm matrix with counts", {
  weights = weight_by_counts(filtered_art, art_token)
  expect_equal(dim(weights)[1], 1)
  expect_gte(dim(weights)[2], 1)
  expect_equal(class(weights), 'matrix')
  expect_gt(min(weights),0)
  expect_lt(max(weights), dim(art_token)[2])
})

weights = weight_by_counts(filtered_art, art_token)

test_that("filter_rare(weights) returns 1xk matrix with rare words removed", {
  rare_art = filter_rare(weights)
  expect_equal(dim(rare_art)[1], 1)
  expect_gte(dim(rare_art)[2], 1)
  expect_equal(class(rare_art), 'matrix')
  expect_gt(min(rare_art), 1)
  rand_min = sample(2:10,1)
  rare_art_rand = filter_rare(weights, rand_min)
  expect_equal(dim(rare_art_rand)[1], 1)
  expect_gte(dim(rare_art_rand)[2], 0)
  expect_equal(class(rare_art_rand), 'matrix')
  if(dim(rare_art_rand)[2]<0){
  expect_gt(min(rare_art_rand), rand_min)
  }
})

test_that("sort_by_weight(weights) returns 1xm matrix in descending order",{
  sorted_weights = sort_by_weight(weights)
  expect_equal(dim(sorted_weights)[1], 1)
  expect_gte(dim(sorted_weights)[2], 1)
  expect_equal(class(sorted_weights), 'matrix')
  expect_gt(min(sorted_weights), 0)
})

test_that("bounding_box(random_word) scales correctly",{
  nchar = sample(1:26, 1)
  rand_chars = toString(sample(letters, nchar))
  rand_word = gsub(', ', '', rand_chars)
  expect_equal(bounding_box(rand_word, 1, vscale=2)$height, 2*strheight(rand_word))
  expect_equal(bounding_box(rand_word, 1, hscale=3)$width, 3*strwidth(rand_word))
})


test_that("find_coords(word_index) performs as expected",{
  expect_equal(find_coords(1)$y_coord, .5)
  for(loops in 1:100){
  expect_false(find_coords(sample(2:100,1))$y_coord == 0)
  expect_false(find_coords(sample(1:100,1))$x_coord == .5)
  expect_lt(abs(find_coords(sample(1:100,1))$y_coord),1)
  expect_lt(abs(find_coords(sample(1:100,1))$x_coord), 1)
  }
})

test_that("spiral_points(rand_x, rand_y) performs as expected",{
  rand_x = runif(1, min = -1, max = 1)
  rand_y = runif(1, min = -1, max = 1)
  spirals = spiral_points(rand_x, rand_y, change=1, nrotations=20)
  expect_equal(dim(spirals)[1], round(40*pi/1,0))
  dist_from_start = sqrt((spirals[,1]-rand_x)^2 + (spirals[,2]-rand_y)^2)
  #points should be within 1 of the random starting point
  expect_equal(sum(!dist_from_start<=1), 0)
  expect_equal(sum(!dist_from_start>=0),0)
  #points should be getting farther away from starting point
  expect_equal(sum(!diff(dist_from_start)>0),0)
})

test_that("overlaps(..) works for a fixed example",{
  weights = as.matrix(rep(.2, 10))
  boxes = list(height = rep(.2, 10), width = rep(.2, 10))
  seq_coords = list(x_coord = (1:10)/10.1, y_coord = rep(0,10))
  overlap = rep(NA, 10)
  for(ind in 1:dim(weights)[2]){
    
    #overlap[ind] = sum(overlaps(seq_coords$x_coord[ind], seq_coords$y_coord[ind], seq_coords, boxes, ind))
  }
})