
rm(list = ls())

# note: to make the final gif files, this script calls system function:
# `convert` (http://imagemagick.org/script/index.php)

gradient_maker <- function(start=NA, stop=NA, cols=c("darkorange", "white", "darkcyan"), vis=FALSE, n=1000){
    if(is.na(start) | is.na(stop)) stop("need to specify start and stop points on a numerical scale")
    colfunc <- colorRampPalette(cols)
    color.list <- colfunc(n)
    color.locations <- seq(start, stop, length=n)
    names(color.locations) <- color.list
    if(vis==TRUE) plot(color.locations, rep(1, n), col=color.list, pch="|", ylim=c(0.9, 1.1), cex=5)
    return(color.locations)
}

data_gradient <- function(data, colors=c("darkorange", "white", "darkcyan"), my.start=NA, my.stop=NA){
    if(is.na(my.start)) my.start <- min(data, na.rm=TRUE)
    if(is.na(my.stop)) my.stop <- max(data, na.rm=TRUE)
    my.gradient <- gradient_maker(start=my.start, stop=my.stop, cols=colors)
    if(any(data > max(my.gradient), na.rm=T) | any(data < min(my.gradient), na.rm=T)) warning("data is not within gradient range")
    data.colors <- rep(NA, length(data))
    for(i in 1:length(data)){
        if(!is.na(data[i])) data.colors[i] <- names(my.gradient)[which.min(abs(data[i]-my.gradient))]
    }
    data.colors
}

col_alpha <- function (acol, alpha = 0.2){
    acol <- col2rgb(acol)
    acol.red <- acol["red",]/255
    acol.green <- acol["green",]/255
    acol.blue <- acol["blue",]/255
    acol <- mapply(function(red, green, blue, alphas) rgb(red, green, blue, alphas), acol.red, acol.green, acol.blue, alpha)
    return(as.character(acol))
}

dir_init <- function(path, verbose=FALSE, overwrite=TRUE){
  if(substr(path, 1, 2)!='./') stop('path argument must be formatted
    with "./" at beginning')
  contents <- dir(path, recursive=TRUE)
  if(dir.exists(path)){
    if(overwrite){
      if(verbose){
        if(length(contents)==0) print(paste('folder ', path, ' created.', sep=""))
        if(length(contents)>0) print(paste('folder ', path, ' wiped of ', length(contents), ' files/folders.', sep=""))
      }
      if(dir.exists(path)) unlink(path, recursive=TRUE)
      dir.create(path)
    }
  } else {
    if(verbose){
      print(paste('folder ', path, ' created.', sep=""))
    }
    dir.create(path)
  }
}

################

calc_right_line <- function(a, b, lambda, height = 1) {
  x <- seq(a, a * lambda^height, length.out = 20)
  y <- b + (log(a) - log(x)) / log(lambda)
  out <- list(x = x, y = y)
  return(out)
}

calc_left_line <- function(a, b, lambda, height = 1) {
  x <- seq(a, a * (1 / lambda)^height, length.out = 20)
  y <- b + (log(a) - log(x)) / log(1 / lambda)
  out <- list(x = x, y = y)
  return(out)
}

draw_triangles <- function(a = 1, b, lambda, height = 1,
  col = "gray", offset = 0, ...) {
  if (length(a) == 1) {
    left_line <- calc_left_line(a, b, lambda, height)
    right_line <- calc_right_line(a, b, lambda, height)
    triangle_shape <- list(x = c(left_line$x, rev(right_line$x)) + offset,
      y = c(left_line$y, rev(right_line$y)))
    polygon(triangle_shape, col = col, ...)
  }
  if (length(a) > 1) {
    # for drawing a whole row of triangles (all with the same b)
    for (i in 1:length(a)) {
      if (length(col) == 1) my_col <- col
      if (length(col) > 1) my_col <- col[i]
      draw_triangles(a[i], b, lambda, height, col = my_col, ...)
    }
  }
}

power_maker <- function(i) {
  out <- seq(-i, i, by = 2)
  return(out)
}

# seperate the simulation from the display?

sim_balls <- function(n_balls, start_times, n_gates, a, lambda) {
  n_keyframes <- (n_balls + (n_gates + 1) + 3)
  # 'gate' n_gates + 1 is simply the exit hole; add 3 frames for falling
  keyframes <- list()
  for (i in 1:n_keyframes) {
    # calculate the position of each ball at each keyframe
    if (i == 1) {
      balls <- data.frame(ball = 1:n_balls,
        net_change = 0,
        x = a,
        start_time = start_times,
        gates_remaining = n_gates)
    } else {
      balls <- keyframes[[i - 1]]
    }
    # update locations in balls
    on_board <- which(balls$gates_remaining > 0 & balls$start_time <= i)
    if (length(on_board) > 0) {
      next_move <- sample(c(-1, 1), length(on_board), replace = TRUE)
      balls$net_change[on_board] <- balls$net_change[on_board] + next_move
      balls$x[on_board] <- balls$x[on_board] * lambda^next_move
      balls$gates_remaining[on_board] <- balls$gates_remaining[on_board] - 1
    }
    if (i %% 100 == 0) print(i)
    keyframes[[i]] <- balls
  }
  # slooooow
  for (i in 1:n_keyframes) {
    keyframes[[i]]$time <- i
    if (i == 1) {
      locations <- keyframes[[i]]
    } else {
      locations <- rbind(locations, keyframes[[i]])
    }
  }
  return(locations)
}

#### run simulation #####

a <- 1
b <- 10
n_gates <- 10
lambda <- 2.65^(1/n_gates)
n_balls <- 300
text_size <- 0.8
xlog <- FALSE

start_times <- 1:n_balls # each ball starts one after the other

set.seed(1001)
keyframe_locations <- sim_balls(n_balls, start_times, n_gates, a, lambda)

n_keyframes <- length(unique(keyframe_locations$time))

# assign colors to each ball
keyframe_locations$ball_col <- "black"

# calculate y position in keyframes
keyframe_locations$y <- NA
waiting <- which(keyframe_locations$start_time > (keyframe_locations$time + 1))
keyframe_locations$y[waiting] <- b + 2
on_board <- which(keyframe_locations$start_time <= (keyframe_locations$time + 1))
keyframe_locations$y[on_board] <- b - n_gates + keyframe_locations$gates_remaining[on_board]


#######

ticks <- seq(1, n_keyframes, by = (1/2))
n_ticks <- length(ticks)

locations <- keyframe_locations

for (i in 1:length(ticks)) {
  # assuming keyframes are the integers
  if (ticks[i] %% 1 != 0) {
    # for each tick, find the keyframes you are *between* 
    upper <- keyframe_locations[keyframe_locations$time == ceiling(ticks[i]), ]
    lower <- keyframe_locations[keyframe_locations$time == floor(ticks[i]), ]
    add <- lower
    add$time <- ticks[i]
    on_board <- which(lower$gates_remaining > 0 & lower$start_time <= (ticks[i] + 1))
    if (length(on_board) > 0) {
      current_power <- ifelse(upper$x[on_board] < lower$x[on_board], (-1), 1)
      add$y[on_board] <- lower$y[on_board] - (ticks[i] %% 1)^1.3
      add$x[on_board] <- lower$x[on_board] * lambda^(current_power *
        (lower$y[on_board] - add$y[on_board]))
    }
    locations <- rbind(locations, add)
  }
}

o <- order(locations$time, locations$ball)
locations <- locations[o, ]

# calculate final position histogram

final <- locations[locations$time == max(locations$time), ]
final$x <- round(final$x, 3)
xs <- sort(unique(final$x))
final$stack_position <- NA
for (j in 1:length(xs)) {
  final$stack_position[final$x == xs[j]] <- order(final$start_time[final$x == xs[j]])
}

counts <- table(final$x)
hist_scale <- n_gates * 0.9 / max(counts)

final$y <- b - 2 * n_gates + (hist_scale * final$stack_position)

# now revise location information to incorporate falling and final positions!

time_falling <- locations$time - (locations$start_time + n_gates)
cleared_board <- which(time_falling >= 0)
locations$y[cleared_board] <- (b - n_gates) - 1.2 * time_falling[cleared_board]^2

link <- match(locations$ball, final$ball)
locations$y <- pmax(locations$y, final$y[link])

## draw each frame

dir_init("./frames")

ticks <- sort(unique(locations$time))

for (i in 1:length(ticks)) {

  # accumulated heatmap of triangle colors
  d <- locations[which(locations$start_time <= (locations$time + 1) &
    locations$time <= ticks[i] & locations$time %% 1 == 0), ]
  hit_colors <- list()
  for (j in 1:n_gates) {
    gate_locations <- power_maker(j - 1)
    gate_counts <- sapply(gate_locations, function(z)
      sum(d$net_change[d$gates_remaining == (n_gates + 1 - j)] == z))
    gate_counts <- gate_counts * length(gate_locations)^(1/2)
    hit_colors[[j]] <- data_gradient(gate_counts,
      colors = c("gray", "red", "yellow"), my.start = 0, my.stop = n_balls * 1.0)
  }

  # isolate current frame and plot
  current_frame <- which(locations$time == ticks[i])
  current_x <- locations$x[current_frame]
  current_y <- locations$y[current_frame]

  filename <- paste0("./frames/frame", sprintf("%04d", i), ".png")
  png(filename, height = 600, width = 600, units = "px", pointsize = 30)
  global_mar <- c(4.1, 0, 0, 0)
  par(mar = global_mar)

  plot(1, 1, type = "n", ylim = c(b - (2 * n_gates), b),
    xlim = c(a * lambda^(-n_gates), a * lambda^(n_gates)),
    frame.plot = FALSE, axes =  FALSE, ann = FALSE)
  abline(v = a, lty = 2, col = col_alpha("gray", 0.2))
  for (j in 1:n_gates) {
    powers <- power_maker(j - 1)
    draw_triangles(a * lambda^powers, b - j + 1, lambda, border = NA,
      height = 0.8, col = hit_colors[[j]])
  }
  final_bins <- a * lambda^power_maker(n_gates)
  axis(1, at = final_bins, labels = power_maker(n_gates), cex.axis = text_size)
  final_gates <- a * lambda^power_maker(n_gates - 1)
  axis(1, at = final_gates, labels = NA, tck = -0.015, cex.axis = text_size)
  mtext("logarithmic units", 1, line = 2.25, cex = text_size)

  points(current_x, current_y, pch = 16, cex = 0.5 * (20/n_gates),
    col = col_alpha(locations$ball_col[current_frame], 1))

  # add a diagram explaining the geometry

  diag_off <- 1 * a
  diag_height <- 3

  # text(a + 0.5 * diag_off, b - 2, paste0("c = ", sprintf("%1.2f", lambda)),
  #   cex = text_size)

  draw_triangles(a, b, lambda, col = NA, height = 3, offset = diag_off)
  lines(c(a, a) + diag_off, c(b, b - diag_height), lty = 2)

  text(a + diag_off, (b - diag_height), pos = 1,
    labels = "a", cex = text_size)
  text(a * lambda^diag_height + diag_off, (b - diag_height),
    pos = 1, labels = "ac", cex = text_size)
  text(a * lambda^(-diag_height) + diag_off, (b - diag_height),
    pos = 1, labels = "a/c", cex = text_size)

  dev.off()

  if (i %% 100 == 0) print(i)

}


system("convert -loop 1 -delay 4 ./frames/frame* ./multiplicative-sim.gif")

beep(4)



