f <- function(x, alpha) {
  exp(-alpha * x) * sin(x)
}

inner_integral <- function(alpha, n) {
  h <- 2 * pi
  sum <- 0
  for (i in 1:(n - 2)) {
    sum <- sum + f(i * h, alpha) + 4 * f((i + 0.5) * h, alpha) + f((i + 1) * h, alpha)
  }
  (h / 3) * (f(0, alpha) + 2 * sum + f(2 * pi, alpha))
}


outer_integral <- function(p, m) {
  h <- p
  sum <- 0
  for (i in 1:(m - 1)) {
    sum <- sum + inner_integral(i * h, n) + 4 * inner_integral((i + 0.5) * h, n) + inner_integral((i + 1) * h, n)
  }
  (h / 6) * sum
}

find_p_bisection <- function(f, a, b, rel_tol = 1e-6) {
  # Use relative tolerance based on integral value
  while (abs(b - a) / max(abs(a), abs(b)) > rel_tol) {
    c <- (a + b) / 2
    if (f(c, n) == 0) {
      break
    } else if (f(a, n) * f(c, n) < 0) {
      b <- c
    } else {
      a <- c
    }
  }
  return((a + b) / 2)
}

n <- 100
m <- 100
tol <- 0.00001

p <- find_p_bisection(outer_integral(1/2, n), 0, 1, rel_tol = tol)

print(p)



my_integration <- function(f, a, b, n) {
  # Výpočet šířky každého subintervalu
  delta_x <- (b - a) / n
  
  # Vytvoření vektoru středních bodů
  mid_points <- a + delta_x/2 + (0:(n-1)) * delta_x
  
  # Výpočet plochy pod křivkou pomocí pravidla obdélníků
  integral_approximation <- sum(f(mid_points)) * delta_x
  
  # Vrácení výsledku
  return(integral_approximation)
}

# Příklad použití
f <- function(x) {
  return(2.71828182846)
}

# Použití funkce my_integration
result <- my_integration(f, 0, 1, 100)
print(result)