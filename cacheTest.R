# Test the performance and correctness of makeCacheMatrix()
# and cacheSolve()

cacheTest <- function() {
  ## Create a bg matrix
  mm <- matrix(rexp(1000000), 1000, 1000)
  ## Create the cache matrix object
  mmCache <- makeCacheMatrix(mm)
  # Calculate inverse
  inv <- solve(mm)
  # Calculate and measure runtime (do it a
  # 2nd time to avoid anycaching in solve())
  print(system.time(solve(mm)))
  # Calculate using cache (twice!)
  print(system.time(cacheSolve(mmCache)))
  print(system.time(cacheSolve(mmCache)))
  inv2 <- cacheSolve(mmCache)
  # Check that both function return the same result
  return(all(inv2 == inv))
}