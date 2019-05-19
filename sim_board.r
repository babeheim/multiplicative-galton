
rm(list = ls())

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

draw_triangles <- function(a = 1, b, lambda, height = 1, ...) {
  if (length(a) == 1) {
    left_line <- calc_left_line(a, b, lambda, height)
    right_line <- calc_right_line(a, b, lambda, height)
    triangle_shape <- list(x = c(left_line$x, rev(right_line$x)),
      y = c(left_line$y, rev(right_line$y)))
    polygon(triangle_shape, ...)
  }
  if (length(a) > 1) {
    # for drawing a whole row of triangles (all with the same b)
    for (i in 1:length(a)) draw_triangles(a[i], b, lambda, height, ...)
  }
}

power_maker <- function(i) {
  out <- seq(-i, i, by = 2)
  return(out)
}

plot_board <- function(a = 1, b = 10, lambda = 1.15, n_gates = 10,
  xlog = FALSE, height = 1) {
  plot(1, 1, type = "n", ylim = c(b - (2 * n_gates), b),
    xlim = c(a * lambda^(-n_gates), a * lambda^(n_gates)),
    frame.plot = FALSE, axes =  FALSE, ann = FALSE, xlog = xlog)
  abline(v = a, lty = 2, col = col_alpha("gray", 0.2))
  for (i in 1:n_gates) {
    powers <- power_maker(i - 1)
    draw_triangles(a * lambda^powers, b - i + 1, lambda, border = NA,
      height = height, col = gray(0.7))
  }
  final_bins <- a * lambda^power_maker(n_gates)
  axis(1, at = final_bins, labels = power_maker(n_gates))
  final_gates <- a * lambda^power_maker(n_gates - 1)
  axis(1, at = final_gates, labels = NA, tck = -0.01)
  mtext("logarithmic units", 1, line = 2.25)
}

plot_board(height = 0.8)

# seperate the simulation from the display?

sim_balls <- function(n_balls, start_times, n_gates, a, lambda) {
  
  n_keyframes <- (n_balls + (n_gates + 1))

  # 'gate' n_gates + 1 is simply the exit hole

  keyframes <- list()

  for (i in 1:n_keyframes) {
    # calculate the position of each ball at each keyframe
    if (i == 1) {
      balls <- data.frame(ball = 1:n_balls,
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
      balls$x[on_board] <- balls$x[on_board] * lambda^next_move
      balls$gates_remaining[on_board] <- balls$gates_remaining[on_board] - 1
    }

    falling <- which(balls$gates_remaining == 0)
    if (length(falling) > 0) {
      # how to do this in a non-hacky way?
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





##########

a <- 1
b <- 10
n_gates <- 10
lambda <- 2.65^(1/n_gates)
n_balls <- 100

start_times <- 1:n_balls # each ball starts one after the other

set.seed(1001)
keyframe_locations <- sim_balls(n_balls, start_times, n_gates, a, lambda)

n_keyframes <- length(unique(keyframe_locations$time))

# assign colors to each ball at its current position at time t
all_bins <- sort(unique(keyframe_locations$x))
all_bins_cols <- data_gradient(all_bins)
# keyframe_locations$ball_col <- all_bins_cols[match(round(keyframe_locations$x, 3), round(all_bins, 3))]
keyframe_locations$ball_col <- "black"

keyframe_locations$y <- b - n_gates + keyframe_locations$gates_remaining

# keyframe animation test

# dir_init("./keyframes")

# for (i in 1:n_keyframes) {

#   current_frame <- which(keyframe_locations$time == i)

#   current_x <- keyframe_locations$x[current_frame]
#   current_y <- keyframe_locations$y[current_frame]

#   filename <- paste0("./keyframes/keyframe", sprintf("%04d", i), ".png")
#   png(filename, res = 300, height = 4, width = 5, units = "in")
#   global_mar <- c(4.1, 0, 0, 0)
#   par(mar = global_mar)
#   plot_board(a = a, b = b, lambda = lambda,
#     n_gates = n_gates, xlog = FALSE, height = 0.8)
#   points(current_x, current_y, pch = 16, cex = 0.5 * (20/n_gates),
#     col = col_alpha(keyframe_locations$ball_col[current_frame], 1))
#   final_bins <- a * lambda^power_maker(n_gates)
#   axis(1, at = final_bins, labels = power_maker(n_gates))
#   mtext("logarithmic units", 1, line = 2.25)
#   dev.off()

#   if (i %% 100 == 0) print(i)

# }


#######

# how to smoothly animate between keyframes?

ticks <- seq(1, n_keyframes, by = 1)
n_ticks <- length(ticks)

# for each tick, find the keyframes you are *between* 
# and calculate intermediate values
# for both x and y, the ball color, and store them

locations <- keyframe_locations

for (i in 1:length(ticks)) {
  # assuming keyframes are the integers
  if (ticks[i] %% 1 != 0) {
    upper <- keyframe_locations[keyframe_locations$time == ceiling(ticks[i]), ]
    lower <- keyframe_locations[keyframe_locations$time == floor(ticks[i]), ]
    add <- lower
    add$time <- ticks[i]

    on_board <- which(lower$gates_remaining > 0 & lower$start_time <= ticks[i])
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

locations[locations$ball == 1, ]

final <- locations[locations$time == max(locations$time), ]
final$x <- round(final$x, 3)
xs <- sort(unique(final$x))
final$stack_position <- NA
for (j in 1:length(xs)) {
  final$stack_position[final$x == xs[j]] <- order(final$start_time[final$x == xs[j]])
}

counts <- table(final$x)
hist_scale <- 0.9 / max(counts)

hist_scale * final$stack_position



dir_init("./frames")

ticks <- sort(unique(locations$time))

for (i in 1:length(ticks)) {

  current_frame <- which(locations$time == ticks[i])
  current_x <- locations$x[current_frame]
  current_y <- locations$y[current_frame]

  filename <- paste0("./frames/frame", sprintf("%04d", i), ".png")
  png(filename, res = 300, height = 4, width = 5, units = "in")
  global_mar <- c(4.1, 0, 0, 0)
  par(mar = global_mar)
  plot_board(a = a, b = b, lambda = lambda,
    n_gates = n_gates, xlog = FALSE, height = 0.8)
  points(current_x, current_y, pch = 16, cex = 0.5 * (20/n_gates),
    col = col_alpha(locations$ball_col[current_frame], 1))
  dev.off()

  if (i %% 100 == 0) print(i)

}

library(beepr)

beep(4)

# ok, but what about the histogram??











# sim_balls <- function(n_balls, start_time, n_gates, a, b, lambda) {

#   final_bins <- a * lambda^power_maker(n_gates)
#   final_bins_counts <- rep(0, length(final_bins))
#   hist_scale <- 0.06
#   hist_baseline <- b - 2 * n_gates
  
#   n_keyframes <- (n_balls + n_gates + 2)

#   keyframes <- list()

#   for (i in 1:n_keyframes) {
#     if (i == 1) {
#       balls <- data.frame(ball = 1:n_balls,
#         x = a,
#         y = b + 3,
#         start_time = start_time,
#         active_time = 0,
#         state = "inactive")
#       balls$state <- as.character(balls$state)
#     } else {
#       balls <- keyframes[[i - 1]]
#     }
    
#     # update locations in balls

#     active <- which(balls$start_time <= i)
#     if (length(active) > 0) {
#       balls$active_time[active] <- balls$active_time[active] + 1
#       balls$state[active] <- "active"
#     }

#     starting <- which(balls$start_time == i)
#     if (length(starting) > 0) {
#       balls$y[starting] <- b
#       balls$state[starting] <- "starting"
#     }

#     on_board <- which(balls$start_time < i & balls$active_time <= (n_gates + 1))
#     if (length(on_board) > 0) {
#       balls$y[on_board] <- balls$y[on_board] - 1
#       next_move <- sample(c(-1, 1), length(on_board), replace = TRUE)
#       balls$x[on_board] <- balls$x[on_board] * c^next_move
#       balls$state[on_board] <- "on_board"
#     }

#     falling <- which(balls$active_time == (n_gates + 2))
#     if (length(falling) > 0) {
#       balls$y[falling] <- b - (n_gates * 1.2)
#       balls$state[falling] <- "falling"
#     }

#     landing <- which(balls$active_time == (n_gates + 3))
#     if (length(landing) > 0) {
#       for (j in 1:length(landing)) {
#         my_bin <- which(round(final_bins, 3) == round(balls$x[landing[j]], 3))
#         balls$y[landing[j]] <- hist_baseline + (final_bins_counts[my_bin] + 1) * hist_scale
#         final_bins_counts[my_bin] <- final_bins_counts[my_bin] + 1
#       }
#       balls$state[landing] <- "landing"
#     }

#     resting <- which(balls$active_time >= (n_gates + 4))
#     if (length(resting) > 0) {
#       balls$state[resting] <- "resting"
#     }

#     keyframes[[i]] <- balls
#   }

#   for (i in 1:n_keyframes) {
#     keyframes[[i]]$time <- i
#     if (i == 1) {
#       locations <- keyframes[[i]]
#     } else {
#       locations <- rbind(locations, keyframes[[i]])
#     }
#   }

#   return(locations)

# }





dir_init("./keyframes")

for (i in 1:n_keyframes) {

  current_frame <- which(locations$time == i & locations$active_time <= i)

  current_x <- locations$x[current_frame]
  current_y <- locations$y[current_frame]

  filename <- paste0("./keyframes/keyframe", sprintf("%04d", i), ".png")
  png(filename, res = 300, height = 4, width = 5, units = "in")
  global_mar <- c(4.1, 0, 0, 0)
  par(mar = global_mar)
  plot_board(a = a, b = b, c = c, n_gates = n_gates, xlog = FALSE, frac = 0.8)
  points(current_x, current_y, pch = 16, cex = 0.5 * (20/n_gates),
    col = col_alpha(locations$ball_col[current_frame], 1))
  final_bins <- a * c^power_maker(n_gates)
  axis(1, at = final_bins, labels = power_maker(n_gates))
  mtext("logarithmic units", 1, line = 2.25)
  dev.off()

  # # draw intermediate frames
  if (i < 1 * n_keyframes) {

    next_frame_subset <- which(locations$time == (i + 1) &
      locations$ball %in% locations$ball[current_frame])

    next_x <- locations$x[next_frame_subset]

    current_power <- ifelse(next_x < current_x, (-1), 1)
    intermediate_y <- current_y
    tar <- which(locations$state[next_frame_subset] %in% c("starting", "on_board"))
    if (length(tar) > 0) intermediate_y[tar] <- current_y[tar] - 0.3
    tar <- which(locations$state[next_frame_subset] %in% c("falling"))
    if (length(tar) > 0) intermediate_y[tar] <- b - (n_gates * 1.1)
    tar <- which(locations$state[next_frame_subset] %in% c("landing"))
    if (length(tar) > 0) intermediate_y[tar] <- b - (n_gates * 1.6)

    intermediate_x <- current_x
    tar <- which(locations$state[next_frame_subset] %in% c("starting", "on_board"))
    intermediate_x[tar] <- c^(current_power[tar] * (current_y[tar] - intermediate_y[tar])) * current_x[tar]

    filename <- paste0("./keyframes/keyframe", sprintf("%04d", i), "zzz.png")
    png(filename, res = 300, height = 4, width = 5, units = "in")
    global_mar <- c(4.1, 0, 0, 0)
    par(mar = global_mar)
    plot_board(a = a, b = b, c = c, n_gates = n_gates, xlog = FALSE, frac = 0.8)
    points(intermediate_x, intermediate_y, pch = 16, cex = 0.5 * (20/n_gates),
      col = col_alpha(locations$ball_col[current_frame], 1))
    final_bins <- a * c^power_maker(n_gates)
    axis(1, at = final_bins, labels = power_maker(n_gates))
    mtext("logarithmic units", 1, line = 2.25)
    dev.off()

  }

  if (i %% 100 == 0) print(i)

}


library(beepr)


system("convert -loop 1 -delay 7 ./keyframes/keyframe* ./test_14.gif")

beep(4)


