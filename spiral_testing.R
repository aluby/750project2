library(wordcloud)
wordcloud(names(weights), weights)

to_polar = function(x, y){
  r = sqrt(x^2 + y^2)
  t = atan(y/x)
  return(c(r,t))
}

spiral_points = function(x,y,change=.1){
  pol = to_polar(x,y)
  t = seq(pol[2], 10 * 2 * pi, by=change)
  r = t/max(t)
  return(list(x_spiral = r*cos(t), y_spiral = r* sin(t)))
}

theta = seq(0, 30 * 2 * pi, by = 2 * pi/72)
x = cos(theta)
y = sin(theta)
R = theta/max(theta)
plot.new()
plot.window(xlim = c(-1, 1), ylim = c(-1,1), asp = 1)
lines(x * R, y * R)
points(x*R, y*R)
