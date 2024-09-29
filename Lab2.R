# Проверка наличия библиотеки tseries
if (!require(tseries)) {
  install.packages("tseries")  # Установка пакета, если он не установлен
}
library(tseries)

# set.seed(123) # На случай если нужно получать одни и те же случ числа
                # Напрмер что бы сравнивать методы оценки мнк и garch 

# Задание 1 ----

# Функция для моделирования процесса garch(1,0)
garch_process <- function(a0, a1, num_obs, initial_volatility = 1, initial_process_value = 1) {
  if (a0 <= 0) stop("a0 должно быть положительным")  # Проверка, что a0 положительно
  if (a1 <= 0 || a1 >= 1) stop("a1 должно быть в диапазоне (0, 1)")  # Проверка диапазона a1
  
  process_values <- numeric(num_obs)  # Инициализация вектора значений процесса
  volatility <- numeric(num_obs)       # Инициализация вектора волатильности
  
  errors <- rnorm(num_obs, mean = 0, sd = 1)  # Генерация случайных ошибок
  volatility[1] <- initial_volatility          # Установка начальной волатильности
  process_values[1] <- initial_process_value   # Установка начального значения процесса
  
  # Основной цикл для вычисления значений процесса и волатильности
  for (t in 2:num_obs) {
    volatility[t] <- a0 + a1 * (process_values[t - 1])^2  # Вычисление волатильности
    process_values[t] <- sqrt(volatility[t]) * errors[t]    # Вычисление значений процесса
  }
  
  par(mfrow = c(2, 1))  # Разделение графиков на 2 строки
  plot(process_values, type = 'l', col = 'seagreen',
       main = 'Стационарный процесс {h_n} garch(1,0)', 
       ylab = 'h_n', xlab = 'Время (n)')  # График значений процесса
  
  plot(volatility, type = 'l', col = 'blue',
       main = 'Волатильность {σ_n} garch(1,0)', 
       ylab = 'σ_n', xlab = 'Время (n)')  # График волатильности
  
  return(process_values)  # Возврат значений процесса
}

# Генерация garch(1,0) с параметрами 
n <- 1000  # Количество наблюдений
a0 <- 0.8  # Значение a0
a1 <- 0.4  # Значение a1

result <- garch_process(a0, a1, n)  # Моделирование процесса garch(1,0)

# Задание 2 ----

# Оценка параметров a0 и a1 методом наименьших квадратов

# Функция для оценки a1
estimate_a1 <- function(h) {
  n <- length(h)  # Длина вектора h
  h_sq <- h^2     # Квадраты значений процесса
  
  # Вычисление необходимых сумм
  sum_h_sq <- sum(h_sq)                  # Сумма h^2
  sum_h_sq_lag <- sum(h_sq[-1])          # Сумма h^2 с лагом
  sum_h_sq_lag_sq <- sum(h_sq[-n]^2)     # Сумма (h^2)^2 с лагом
  sum_h_sq_product <- sum(h_sq[-1] * h_sq[-n])  # Сумма произведений
  
  # Вычисление коэффициента a1
  numerator <- (sum_h_sq_product / (n - 1)) - (sum_h_sq * sum_h_sq_lag) / (n^2)
  denominator <- (sum_h_sq_lag_sq / (n - 1)) - (sum_h_sq_lag / (n - 1))^2
  
  return(numerator / denominator)  # Возврат оценки a1
}

# Функция для оценки a0
estimate_a0 <- function(h, a1) {
  h_sq <- h^2  # Квадраты значений процесса
  return(mean(h_sq) - a1 * mean(h[-1]^2))  # Среднее значение h^2 минус влияние a1
}

# Вычисление оценок
est_a1 <- estimate_a1(result)   # Оценка a1
est_a0 <- estimate_a0(result, est_a1)   # Оценка a0

# Печать результатов оценки методом наименьших квадратов
cat("Оценка a0 методом наименьших квадратов:", est_a0, "\n")   # Вывод оценки a0
cat("Оценка a1 методом наименьших квадратов:", est_a1, "\n")   # Вывод оценки a1

# Задание 3 ----

# Оценка параметров a0 и a1 с использованием функции garch() из пакета tseries

# Формируем выборку h_n для оценки параметров
h_n <- result   # Используем результат моделирования как выборку

# Оценка параметров с помощью функции garch()
garch_fit <- garch(h_n, order = c(0, 1), trace = FALSE)   # Используем порядок (0, 1) для garch(1,0)

# Извлечение оценок параметров
est_a0_garch <- coef(garch_fit)[1]   # Оценка a0
est_a1_garch <- coef(garch_fit)[2]   # Оценка a1

# Печать результатов оценки методом garch()
cat("Оценка a0 методом garch():", est_a0_garch, "\n")   # Вывод оценки a0
cat("Оценка a1 методом garch():", est_a1_garch, "\n")   # Вывод оценки a1

# Выводы о сравнении оценок параметров
cat("\nСравнение оценок параметров:\n")
cat("Метод наименьших квадратов:\n")
cat("a0:", est_a0, ", a1:", est_a1, "\n")
cat("Метод garch():\n")
cat("a0:", est_a0_garch, ", a1:", est_a1_garch, "\n")

