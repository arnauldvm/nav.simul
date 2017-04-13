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

HOUR_MS = parseDurationsToMillis("1h")

time_interval_ms = parseDurationsToMillis(TIME_INTERVAL)
time_interval_h = time_interval_ms/HOUR_MS
duration_ms = parseDurationsToMillis(DURATION)
n_points = 1 + duration_ms/time_interval_ms

DF = data.frame(seq=seq(0, n_points-1))
DF$time_ms = DF$seq * time_interval_ms
DF$tide_speed_x = TIDE_SPEED_X * sin(2*pi*(DF$seq-1)/(n_points-1))

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
  remaining.distance = norm(delta.vector, "f")
  
  if (remaining.distance<speed_increment) {
    # Drop anchor
    bearing = NA
    next.pos = last.pos
  } else {
    bearing = bearing_FUN(delta.vector)
    next.pos = last.pos + speed_increment*c(cos(bearing), sin(bearing))
    # Add tide effect:
    next.pos[1] = next.pos[1] + DF$tide_speed_x[i+1]*time_interval_h
  }

  df[i, 'brg'] = bearing
  df[i+1, c('x','y')] = next.pos
  df
}

CONSTANT_BEARING = pi/2+0.047 # optimal value obtained by trial and error!
for (i in seq(1, n_points-1)) {
  DF.gps = simulate(i, DF.gps, bearing_FUN=function(delta.vector) { angleOf(delta.vector) })
  DF.trad = simulate(i, DF.trad, bearing_FUN=function(delta.vector) { CONSTANT_BEARING })
}

arrival.gps = min(which(is.na(DF.gps$brg)))
arrival.trad = min(which(is.na(DF.trad$brg)))

# Plotting

plot(DF$tide_speed_x, DF$time_ms/HOUR_MS, type="l", ylab="time(h)", main="Tide(kn)")

plot(DF$time_ms/HOUR_MS, DF.gps$y, type="l", col="red", xlab="time(h)", main="Progression(y)",
     ylim=c(min(DF.gps$y, DF.trad$y), max(DF.gps$y, DF.trad$y))
)
lines(DF$time_ms/HOUR_MS, DF.trad$y, type="l", col="blue")
points(DF$time_ms[1]/HOUR_MS, DF.gps$y[1], type="p")
points(DF$time_ms[arrival.gps]/HOUR_MS, DF.gps$y[arrival.gps], type="p", col="red")
points(DF$time_ms[arrival.trad]/HOUR_MS, DF.trad$y[arrival.trad], type="p", col="blue")

plot(DF.gps$x, DF$time_ms/HOUR_MS, type="l", col="red", ylab="time(h)", main="Progression(x)",
     xlim=c(min(DF.gps$x, DF.trad$x), max(DF.gps$x, DF.trad$x))
)
lines(DF.trad$x, DF$time_ms/HOUR_MS, type="l", col="blue")
points(DF.gps$x[1], DF$time_ms[1]/HOUR_MS, type="p")
points(DF.gps$x[arrival.gps], DF$time_ms[arrival.gps]/HOUR_MS, type="p", col="red")
points(DF.trad$x[arrival.trad], DF$time_ms[arrival.trad]/HOUR_MS, type="p", col="blue")

plot(DF$time_ms/HOUR_MS, DF.gps$brg/pi*180, type="l", col="red", xlab="time(h)", ylab="degrees", main="Bearing")
lines(DF$time_ms/HOUR_MS, DF.trad$brg/pi*180, type="l", col="blue")
points(DF$time_ms[c(1, arrival.gps-1)]/HOUR_MS, DF.gps$brg[c(1, arrival.gps-1)]/pi*180, type="p", col="red")
points(DF$time_ms[c(1, arrival.trad-1)]/HOUR_MS, DF.trad$brg[c(1, arrival.trad-1)]/pi*180, type="p", col="blue")

plot(DF.gps$x, DF.gps$y, type="l", col="red", main="Course",
     xlim=c(min(DF.gps$x, DF.trad$x), max(DF.gps$x, DF.trad$x)),
     ylim=c(min(DF.gps$y, DF.trad$y), max(DF.gps$y, DF.trad$y))
)
lines(DF.trad$x, DF.trad$y, type="l", col="blue")
points(c(SRC_POS$x, TGT_POS$x), c(SRC_POS$y, TGT_POS$y), type="p")
