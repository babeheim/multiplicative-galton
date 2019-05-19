

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

# we need to be able to truncate the base to allow the balls to fall within!

right_line <- function(a, b, c, frac = 1) {
  x <- seq(a, a * c^frac, length.out = 20)
  y <- b + log(a)/log(c) - log(x)/log(c)
  out <- list(x = x, y = y)
  return(out)
}

left_line <- function(a, b, c, frac = 1) {
  x <- seq(a, a*(1/c)^frac, length.out = 20)
  y <- b + log(a)/log(1/c) - log(x)/log(1/c)
  out <- list(x = x, y = y)
  return(out)
}

draw_triangle <- function(a, b, c, frac, ...) {
  if (length(a) == 1) {
    left <- left_line(a, b, c, frac)
    right <- right_line(a, b, c, frac)
    bottom <- list(x = seq(a * (1/c)^frac, a * c^frac, length.out = 20),
     y = rep(b - frac * 1, 20))
    shape <- list(x = c(left$x, bottom$x, rev(right$x)),
      y = c(left$y, bottom$y, rev(right$y)))
    polygon(shape, ...)
  }
  if (length(a) > 1) {
    for (i in 1:length(a)) draw_triangle(a[i], b, c, frac, ...)
  }
}

power_maker <- function(i) {
  if (i %% 2 != 0) {
    pos <- seq(1, i, by = 2)
    out <- c(rev(pos) * (-1), pos)
  }
  if (i %% 2 == 0) {
    pos <- seq(2, i, by = 2)
    out <- c(rev(pos) * (-1), 0, pos)
  }
  return(out)
}

plot_board <- function(a = 1, b = 10, c = 2.65, n_levels = 10, xlog = FALSE, frac = 1) {

  plot(1, 1, type = "n", ylim = c(b - 2 * n_levels, b),
    xlim = c(c^(-n_levels), c^(n_levels)), frame.plot = FALSE,
    axes =  FALSE, ann = FALSE, xlog = xlog)

  draw_triangle(a, b, c, border = NA, frac = frac, col = col_alpha("blue", 0.2))

  for (i in 1:(n_levels - 1)) {
    powers <- power_maker(i)
    draw_triangle(a * c^powers, b - i, c, border = NA, frac = frac,
      col = col_alpha("blue", 0.2))
  }

}


sim_balls <- function(n_balls, start_time, n_levels, a, b, c) {

  final_bins <- a * c^power_maker(n_levels)
  final_bins_counts <- rep(0, length(final_bins))
  hist_scale <- 0.2
  hist_baseline <- b - 2 * n_levels
  
  n_keyframes <- (n_balls + n_levels + 2)

  keyframes <- list()

  for (i in 1:n_keyframes) {
    if (i == 1) {
      balls <- data.frame(ball = 1:n_balls,
        x = a,
        y = b + 3,
        start_time = start_time,
        active_time = 0,
        state = "inactive")
      balls$state <- as.character(balls$state)
    } else {
      balls <- keyframes[[i - 1]]
    }
    
    # update locations in balls

    active <- which(balls$start_time <= i)
    if (length(active) > 0) {
      balls$active_time[active] <- balls$active_time[active] + 1
      balls$state[active] <- "active"
    }

    starting <- which(balls$start_time == i)
    if (length(starting) > 0) {
      balls$y[starting] <- b
      balls$state[starting] <- "starting"
    }

    on_board <- which(balls$start_time < i & balls$active_time <= (n_levels + 1))
    if (length(on_board) > 0) {
      balls$y[on_board] <- balls$y[on_board] - 1
      next_move <- sample(c(-1, 1), length(on_board), replace = TRUE)
      balls$x[on_board] <- balls$x[on_board] * c^next_move
      balls$state[on_board] <- "on_board"
    }

    falling <- which(balls$active_time == (n_levels + 2))
    if (length(falling) > 0) {
      balls$y[falling] <- b - (n_levels * 1.2)
      balls$state[falling] <- "falling"
    }

    landing <- which(balls$active_time == (n_levels + 3))
    if (length(landing) > 0) {
      for (j in 1:length(landing)) {
        my_bin <- which(round(final_bins, 3) == round(balls$x[landing[j]], 3))
        balls$y[landing[j]] <- hist_baseline + (final_bins_counts[my_bin] + 1) * hist_scale
        final_bins_counts[my_bin] <- final_bins_counts[my_bin] + 1
      }
      balls$state[landing] <- "landing"
    }

    resting <- which(balls$active_time >= (n_levels + 4))
    if (length(resting) > 0) {
      balls$state[resting] <- "resting"
    }

    keyframes[[i]] <- balls
  }


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


##########

rose_to_cyan <- c(
  "#FF254D",
  "#EC0071",
  "#C74D86",
  "#886B9C",
  "#2F9390"
)


a <- 1
b <- 10
n_levels <- 10
c <- 2.65^(1/n_levels)

# plot_board(a = a, b = b, c = c, n_levels = n_levels, xlog = FALSE, frac = 1)

# simulate balls falling through

n_balls <- 180
start_time <- 1:n_balls # each ball starts one after the other

set.seed(1001)
locations <- sim_balls(n_balls, start_time, n_levels, a, b, c)

n_keyframes <- (n_balls + n_levels + 2)

all_bins <- sort(unique(locations$x))
all_bins_cols <- data_gradient(all_bins, colors = rev(rose_to_cyan))

locations$ball_col <- all_bins_cols[match(round(locations$x, 3), round(all_bins, 3))]

# offset to look real
tar <- which(locations$state %in% c("starting", "on_board"))
locations$y[tar] <- locations$y[tar] + 0.09


#######



dir_init("./keyframes")

for (i in 1:n_keyframes) {

  current_frame <- which(locations$time == i & locations$active_time <= i)

  current_x <- locations$x[current_frame]
  current_y <- locations$y[current_frame]

  filename <- paste0("./keyframes/keyframe", sprintf("%04d", i), ".png")
  png(filename, res = 300, height = 4, width = 5, units = "in")
  global_mar <- c(4.1, 0, 0, 0)
  par(mar = global_mar)
  plot_board(a = a, b = b, c = c, n_levels = n_levels, xlog = FALSE, frac = 0.8)
  points(current_x, current_y, pch = 16, cex = 0.5 * (20/n_levels),
    col = col_alpha(locations$ball_col[current_frame], 1))
  final_bins <- a * c^power_maker(n_levels)
  axis(1, at = final_bins, labels = power_maker(n_levels))
  mtext("logarithmic units", 1, line = 2.25)
  dev.off()

  # # draw intermediate frames
  if (i < n_keyframes) {

    next_frame_subset <- which(locations$time == (i + 1) &
      locations$ball %in% locations$ball[current_frame])

    next_x <- locations$x[next_frame_subset]

    current_power <- ifelse(next_x < current_x, (-1), 1)
    intermediate_y <- current_y
    tar <- which(locations$state[next_frame_subset] %in% c("starting", "on_board"))
    if (length(tar) > 0) intermediate_y[tar] <- current_y[tar] - 0.3
    tar <- which(locations$state[next_frame_subset] %in% c("falling"))
    if (length(tar) > 0) intermediate_y[tar] <- b - (n_levels * 1.1)
    tar <- which(locations$state[next_frame_subset] %in% c("landing"))
    if (length(tar) > 0) intermediate_y[tar] <- b - (n_levels * 1.6)

    intermediate_x <- current_x
    tar <- which(locations$state[next_frame_subset] %in% c("starting", "on_board"))
    intermediate_x[tar] <- c^(current_power[tar] * (current_y[tar] - intermediate_y[tar])) * current_x[tar]

    filename <- paste0("./keyframes/keyframe", sprintf("%04d", i), "zzz.png")
    png(filename, res = 300, height = 4, width = 5, units = "in")
    global_mar <- c(4.1, 0, 0, 0)
    par(mar = global_mar)
    plot_board(a = a, b = b, c = c, n_levels = n_levels, xlog = FALSE, frac = 0.8)
    points(intermediate_x, intermediate_y, pch = 16, cex = 0.5 * (20/n_levels),
      col = col_alpha(locations$ball_col[current_frame], 1))
    final_bins <- a * c^power_maker(n_levels)
    axis(1, at = final_bins, labels = power_maker(n_levels))
    mtext("logarithmic units", 1, line = 2.25)
    dev.off()

  }

  if (i %% 100 == 0) print(i)

}

system("convert -loop 1 -delay 7 ./keyframes/keyframe* ./test.gif")

