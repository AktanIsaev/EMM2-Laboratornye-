#Задание 1
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

#Задание 2
# Функция для оценки параметра ?? методом наименьших квадратов с ограничением
estimate_theta_mnk <- function(x) {
  n <- length(x)
  
  # Числитель и знаменатель для оценки ??
  numerator <- sum(x[2:n] * x[1:(n-1)])
  denominator <- sum(x[1:(n-1)]^2)
  
  # Оценка ??
  estimated_theta <- numerator / denominator
  
  # Применяем ограничение |??| ??? 1
  estimated_theta <- ifelse(abs(estimated_theta) > 1, sign(estimated_theta), estimated_theta)
  
  return(estimated_theta)
}

# Оценка параметра ?? для каждого процесса и вывод результата
cat("\nОценка параметра ??:\n")
for (theta in theta_values) {
  x <- ar(n, theta)
  
  # Оценка параметра ??
  estimated_theta <- estimate_theta_mnk(x)
  
  # Вывод результата оценки ?? только если начальное значение ?? ??? 1
  if (abs(theta) <= 1) {
    cat("Оцененное значение ?? для начального ?? =", theta, ":", estimated_theta, "\n")
  }
}
