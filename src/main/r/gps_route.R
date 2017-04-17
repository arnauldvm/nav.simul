#!/usr/bin/env Rscript

##################################
# GPS route navigation simulation
##################################

# Note: we ignore difference between orthodromic route and loxodromic route

# Define parameters
# -----------------

TIME_INTERVAL = "1m"
DURATION = "12h"

# distance unit in nmi
SRC_POS = list(x=0, y=0)
TGT_POS = list(x=0, y=100)

# speed unit is in kn = nmi/h
BOAT_SPEED = 10
TIDE_SPEED_X = 5
TIDE_SLOTS_DURATION_RATIO = 10/12 # For square tide only
# TIDE_TYPE = "SIN"
# TIDE_TYPE = "COS2"
TIDE_TYPE = "SQUARE"

IMG_WIDTH = 512
IMG_HEIGHT = 512

tgt.base.dir.relpath = "../../../generated/img"

# Import functions
# ----------------

if (sys.nframe()>0) { # Started with source(...)
  script.path = sys.frame(1)$ofile
} else { # Started by Rscript
  script.path = gsub("--file=", "", grep("--file=", commandArgs(trailingOnly=F), value=TRUE))
}
script.dir <- dirname(script.path)

source(file=file.path(script.dir, "parseLines.R"))
source(file=file.path(script.dir, "parseDurations.R"))
source(file=file.path(script.dir, "vectors.R"))

rowToVector = function(row) {
  as.matrix(c(row$x, row$y))
}

# Preparation
# -----------

tgt.dir = file.path(script.dir, tgt.base.dir.relpath, TIDE_TYPE)
dir.create(tgt.dir, recursive=T)

HOUR_MS = parseDurationsToMillis("1h")

time_interval_ms = parseDurationsToMillis(TIME_INTERVAL)
time_interval_h = time_interval_ms/HOUR_MS
duration_ms = parseDurationsToMillis(DURATION)
n_points = 1 + duration_ms/time_interval_ms

DF = data.frame(seq=seq(0, n_points-1))
DF$time_ms = DF$seq * time_interval_ms
DF$tide_speed_x = switch(TIDE_TYPE,
       SIN = function() {
         TIDE_SPEED_X * sin(2*pi*(DF$seq-1)/(n_points-1))
       },
       COS2 = function() {
         TIDE_SPEED_X * cos(pi*(DF$seq-1)/(n_points-1))
       },
       SQUARE = function() {
         d = rep(0, times=n_points)
         d[DF$time_ms<TIDE_SLOTS_DURATION_RATIO/2*duration_ms] = +TIDE_SPEED_X
         d[DF$time_ms>(1-TIDE_SLOTS_DURATION_RATIO/2)*duration_ms] = -TIDE_SPEED_X
         d
       })()

DF.gps = data.frame(seq=seq(0, n_points-1))
DF.gps$x = NA
DF.gps$y = NA
DF.gps$brg = NA
DF.gps[1, c('x','y')] = SRC_POS

DF.trad = DF.gps

# Start simulation
# ----------------

speed_increment = BOAT_SPEED*time_interval_h
simulate = function(i, df, bearing_FUN) {
  last.pos = df[i, c('x','y')]
  delta.vector = rowToVector(TGT_POS)-rowToVector(last.pos)
  BTW = angleOf(delta.vector) # Bearing To Waypoint
  remaining.distance = norm(delta.vector, "f")
  
  if (remaining.distance<speed_increment) {
    # Drop anchor
    bearing = NA
    next.pos = last.pos
  } else {
    bearing = bearing_FUN(BTW)
    next.pos = last.pos + speed_increment*c(cos(bearing), sin(bearing))
    # Add tide effect:
    next.pos[1] = next.pos[1] + DF$tide_speed_x[i+1]*time_interval_h
  }

  df[i, 'brg'] = bearing
  df[i+1, c('x','y')] = next.pos
  df
}

CONSTANT_BEARING = switch(TIDE_TYPE,
  SIN = pi/2+0.047, # optimal value obtained by trial and error!
  COS2 = pi/2+0.095, # optimal value obtained by trial and error!
  SQUARE = pi/2+0.095 # optimal value obtained by trial and error!
)
for (i in seq(1, n_points-1)) {
  # TODO: Should add a third strategy: the skipper trying to follow the calculated route
  DF.gps = simulate(i, DF.gps, bearing_FUN=function(BTW) { BTW })
  DF.trad = simulate(i, DF.trad, bearing_FUN=function(BTW) { CONSTANT_BEARING })
}

arrival.gps = min(which(is.na(DF.gps$brg)))
arrival.trad = min(which(is.na(DF.trad$brg)))

min_x = min(DF.gps$x, DF.trad$x, na.rm=T)
max_x = max(DF.gps$x, DF.trad$x, na.rm=T)
min_y = min(DF.gps$y, DF.trad$y, na.rm=T)
max_y = max(DF.gps$y, DF.trad$y, na.rm=T)
min_brg = min(DF.gps$brg, DF.trad$brg, na.rm=T)
max_brg = max(DF.gps$brg, DF.trad$brg, na.rm=T)

# Plotting

png(filename=file.path(tgt.dir, "tide.png"), width=IMG_WIDTH, height=IMG_HEIGHT)
plot(DF$tide_speed_x, DF$time_ms/HOUR_MS, type="l", ylab="time(h)", main="Tide(kn)")
dev.off()

png(filename=file.path(tgt.dir, "progression_y.png"), width=IMG_WIDTH, height=IMG_HEIGHT)
plot(DF$time_ms/HOUR_MS, DF.gps$y, type="l", col="red", xlab="time(h)", main="Progression(y)", ylim=c(min_y, max_y))
lines(DF$time_ms/HOUR_MS, DF.trad$y, type="l", col="blue")
points(DF$time_ms[1]/HOUR_MS, DF.gps$y[1], type="p")
points(DF$time_ms[arrival.gps]/HOUR_MS, DF.gps$y[arrival.gps], type="p", col="red")
points(DF$time_ms[arrival.trad]/HOUR_MS, DF.trad$y[arrival.trad], type="p", col="blue")
legend("topleft", legend=sprintf("Delay = %.0f min", (DF$time_ms[arrival.gps]-DF$time_ms[arrival.trad])/1000/60))
dev.off()

png(filename=file.path(tgt.dir, "progression_x.png"), width=IMG_WIDTH, height=IMG_HEIGHT)
plot(DF.gps$x, DF$time_ms/HOUR_MS, type="l", col="red", ylab="time(h)", main="Progression(x)", xlim=c(min_x, max_x))
lines(DF.trad$x, DF$time_ms/HOUR_MS, type="l", col="blue")
points(DF.gps$x[1], DF$time_ms[1]/HOUR_MS, type="p")
points(DF.gps$x[arrival.gps], DF$time_ms[arrival.gps]/HOUR_MS, type="p", col="red")
points(DF.trad$x[arrival.trad], DF$time_ms[arrival.trad]/HOUR_MS, type="p", col="blue")
dev.off()

png(filename=file.path(tgt.dir, "bearing.png"), width=IMG_WIDTH, height=IMG_HEIGHT)
plot(DF$time_ms/HOUR_MS, DF.gps$brg/pi*180, type="l", col="red", xlab="time(h)", ylab="degrees", main="Bearing", ylim=c(min_brg, max_brg)/pi*180)
lines(DF$time_ms/HOUR_MS, DF.trad$brg/pi*180, type="l", col="blue")
points(DF$time_ms[c(1, arrival.gps-1)]/HOUR_MS, DF.gps$brg[c(1, arrival.gps-1)]/pi*180, type="p", col="red")
points(DF$time_ms[c(1, arrival.trad-1)]/HOUR_MS, DF.trad$brg[c(1, arrival.trad-1)]/pi*180, type="p", col="blue")
dev.off()

png(filename=file.path(tgt.dir, "course.png"), width=IMG_WIDTH, height=IMG_HEIGHT)
plot(DF.gps$x, DF.gps$y, type="l", col="red", main="Course", xlim=c(min_x, max_x), ylim=c(min_y, max_y))
lines(DF.trad$x, DF.trad$y, type="l", col="blue")
points(c(SRC_POS$x, TGT_POS$x), c(SRC_POS$y, TGT_POS$y), type="p")
legend("left", legend=c("Traditional", "GPS"), lty=c(1,1), col=c("blue", "red"))
dev.off()
