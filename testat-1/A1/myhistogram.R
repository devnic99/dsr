myhistogram = function(x, n=length(x)-1, min=base::min(x), max=base::max(x)) {

  outside = x[x < min | x >= max]
  if(length(outside) > 0){
    warning(paste('Zahl(en) außerhalb Intervallgrenzen: ', paste(unlist(sort(outside)), collapse = ', ')))
  }

  filtered = x[x >= min & x <= max] # removes all values which are not in the interval defined by min and max
  sorted = sort(filtered) # sorts vector ascending

  interval_size = (max-min) / n
  borders = seq(min, max, by=interval_size) # creates the list of borders of size n+1
  counts = integer(n)

  for(i in 1:n){
    counts[i] = length(sorted[borders[i] <= sorted & sorted < borders[i+1]])
  }
  
  return(list(
    borders = borders,
    counts = counts
  ))
}
