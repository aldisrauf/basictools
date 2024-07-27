#' Calculate Power Allocation
#'
#' This function allocates resources (e.g., sample sizes) to different groups or strata based on a target total, using a power allocation method. Optional minimum allocations and auxiliary information can be used to guide the allocation.
#'
#' @param target Numeric. The total allocation target.
#' @param V1 Numeric vector. Primary allocation vector.
#' @param power Numeric. The power to which each element of `V1` is raised, default is 0.5.
#' @param V2 Numeric vector. Optional secondary allocation vector. Must have the same length as `V1` if provided.
#' @param minAlloc Numeric. Optional minimum allocation for each element of `V1`.
#'
#' @return A numeric vector of allocated values.
#' @examples
#' powerAlloc(target = 100, V1 = c(30, 20, 50), power = 0.5, V2 = c(35, 25, 40), minAlloc = 10)
#' @export
powerAlloc <- function(target = NULL, V1 = NULL, power = 0.5, V2 = NULL, minAlloc = NULL)
{
  alloc3 = 0
  if (sum(V1) <= target) {
    warning("target <= populasi")
    return(V1)
  }
  if (!is.null(V2) & length(V1) != length(V2)) {
    warning("length V1 != length V2")
    return(NULL)
  }
  if (!is.null(minAlloc)) {
    if (minAlloc * length(V1) > target) {
      warning("alokasi minAlloc > target")
      return(NULL)
    }
  }
  if (!is.null(minAlloc)) {
    alloc3 = rep(minAlloc, length(V1))
    if (is.null(V2)) {
      for (i in 1:length(V1)) {
        alloc3[i] = min(V1[i], minAlloc)
      }
    } else {
      for (i in 1:length(V1)) {
        alloc3[i] = min(V2[i], minAlloc)
      }
    }
    target = target - sum(alloc3)
    V1 = V1 - alloc3
  }
  V1_sqrt = (V1^power)
  V1_sqrtT = round_preserve_sum(V1_sqrt/sum(V1_sqrt) * target)
  alloc1 = V1
  if (is.null(V2)) {
    for (i in 1:length(V1)) {
      alloc1[i] = min(V1[i], V1_sqrtT[i])
    }
  } else {
    for (i in 1:length(V1)) {
      alloc1[i] = min(V2[i], V1_sqrtT[i])
    }
  }
  targetSisa = target - sum(alloc1)
  if (is.null(V2)) {
    allocSisa = V1 - alloc1
  } else {
    allocSisa = V2 - alloc1
  }
  if (sum(allocSisa) > 0) {
    allocSisaSqrt = (allocSisa)
    alloc2 = round_preserve_sum(allocSisaSqrt/sum(allocSisaSqrt) * targetSisa)
  } else {
    alloc2 = 0
  }
  allocFinal = alloc1 + alloc2 + alloc3
  return(allocFinal)
}
