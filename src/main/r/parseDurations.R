parseDurationsToMillis = function(string) { # e.g. "1w 2d5h15m 08.24s", "0.000045s"
  opt_sep_re = "\\h*"
  re = paste0("^",
              "(?:(?<weeks>\\d+)w)?", opt_sep_re,
              "(?:(?<days>\\d+)d)?", opt_sep_re,
              "(?:(?<hours>\\d+)h)?", opt_sep_re,
              "(?:(?<minutes>\\d+)m)?", opt_sep_re,
              "(?:(?<seconds>\\d+)(?:\\.(?<milliseconds>\\d*))?s)?",
              "$")
  data = parseLines(re, string)
  l=nchar(as.character(data$milliseconds))
  if (l<3) data$milliseconds = paste0(data$milliseconds, paste(rep("0", 3-l), collapse=""))
  if (l>3) data$milliseconds = sub("(\\d{3})(\\d+)", "\\1.\\2", data$milliseconds)
  data = data.frame(t(apply(data, MARGIN=1, FUN=function(x) ifelse(x=='', 0, as.numeric(x)))))
  
  1000*(60*(60*(24*(7*data$weeks+data$days)+data$hours)+data$minutes)+data$seconds)+data$milliseconds
}
