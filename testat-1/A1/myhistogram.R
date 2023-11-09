myhistogram = function(x, n = length(x)-1, min = base::min(x), max = base::max(x)) {

  delta_b = (max-min) / n  
  borders = seq(from = min, to = max, by = delta_b)
  counts = replicate(length(borders)-1, 0)

  ignored_vals = x[x < min | x >= max]
  if (length(ignored_vals) > 0) {
    warn_msg = paste("Folgende Werte befinden sich außerhalb des vorgegebenen Bereichs: ", paste(unlist(sort(ignored_vals)), collapse = ", "))
    warning(warn_msg)
  }

  for (ele in x) {
    for (i in 1:n) {
      if (ele >= borders[i] && ele < borders[i+1]) {
        counts[i] = counts[i] + 1
        next
      }
    }
  }

  res = list(borders = borders, counts = counts)
  return(res)
}
