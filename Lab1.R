# Функция для генерации AR(1) процесса
ar <- function(n, theta) {
  # Инициализация вектора для хранения значений
  x <- numeric(n)
  
  # Генерация случайной ошибки (шум)
  epsilon <- rnorm(n, mean = 0, sd = 1)
  
  # Начальное значение
  x[1] <- epsilon[1]  # или любое другое случайное значение
  
  # Генерация AR(1) процесса
  for (k in 2:n) {
    x[k] <- theta * x[k - 1] + epsilon[k]
  }
  
  return(x)
}

# Параметры
n <- 100  # Объем выборки

# Значения параметра theta для различных случаев
theta_values <- c(0.5, 1, 1.5)  # |theta| < 1, |theta| = 1, |theta| > 1

# Построение графиков
par(mfrow=c(3, 1))  # Разделение окна на 3 строки и 1 столбец

for (theta in theta_values) {
  x <- ar(n, theta)
  
  plot(x, type='l', main=paste("AR(1) процесс при ?? =", theta), 
       ylab="Значения", xlab="Наблюдения", col="blue")
}
