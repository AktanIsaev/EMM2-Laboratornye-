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

# Задание 4 ----

# Функция для моделирования процесса GARCH(3,0)
garch_process_3_0 <- function(a0, a1, a2, a3, num_obs, initial_volatility = 1, initial_process_value = 1) {
  # Проверка, что параметр a0 положительный
  if (a0 <= 0) stop("a0 должно быть положительным")
  # Проверка, что параметры a1, a2, a3 положительные и сумма (a1 + a2 + a3) меньше 1
  if (a1 <= 0 || a2 <= 0 || a3 <= 0 || (a1 + a2 + a3) >= 1) stop("Параметры должны удовлетворять условию 0 < a1 + a2 + a3 < 1")
  
  # Инициализация векторов для значений процесса и волатильности
  process_values <- numeric(num_obs)
  volatility <- numeric(num_obs)
  
  # Генерация случайных ошибок из нормального распределения
  errors <- rnorm(num_obs, mean = 0, sd = 1)
  
  # Установка начальных условий
  volatility[1] <- initial_volatility
  process_values[1] <- initial_process_value
  
  # Основной цикл для вычисления значений процесса GARCH(3,0)
  for (t in 4:num_obs) {
    # Вычисление волатильности на текущем шаге
    volatility[t] <- a0 + a1 * (process_values[t - 1])^2 + a2 * (process_values[t - 2])^2 + a3 * (process_values[t - 3])^2
    # Вычисление значений процесса на текущем шаге
    process_values[t] <- sqrt(volatility[t]) * errors[t]
  }
  
  # Разделение графиков на 2 строки
  par(mfrow = c(2, 1))
  # График значений процесса
  plot(process_values, type = 'l', col = 'seagreen', main = 'Стационарный процесс {h_n} GARCH(3,0)', ylab = 'h_n', xlab = 'Время (n)')
  # График волатильности
  plot(volatility, type = 'l', col = 'blue', main = 'Волатильность {σ_n} GARCH(3,0)', ylab = 'σ_n', xlab = 'Время (n)')
  
  # Возврат значений процесса и волатильности в виде списка
  return(list(process_values = process_values, volatility = volatility))
}

# Генерация процесса GARCH(3,0)
n <- 1100 # Количество наблюдений
a0 <- 0.2 # Значение параметра a0
a1 <- 0.3 # Значение параметра a1
a2 <- 0.2 # Значение параметра a2
a3 <- 0.1 # Значение параметра a3

# Вызов функции для генерации данных GARCH(3,0)
garch_data <- garch_process_3_0(a0, a1, a2, a3, n)
# Извлечение значений процесса
process_values <- garch_data$process_values

# Разделение на обучающую и тестовую выборки в соотношении 10:1
train_size <- floor(n / 11 * 10) # Размер обучающей выборки (10/11 от всех данных)
test_size <- n - train_size # Размер тестовой выборки (1/11 от всех данных)

# Создание обучающей выборки
train_data <- process_values[1:train_size]
# Создание тестовой выборки
test_data <- process_values[(train_size + 1):n]

# Оценка параметров (a0, a1, a2, a3) на обучающей выборке с помощью функции garch
garch_fit_3_0 <- garch(train_data, order = c(0, 3), trace = FALSE) # Оценка GARCH(3,0)
est_params <- coef(garch_fit_3_0) # Извлечение оцененных параметров (a0, a1, a2, a3)

# Прогнозирование на 1 шаг вперед для тестовой выборки
h_pred <- numeric(test_size) # Вектор для хранения прогнозов
# Цикл по каждому элементу тестовой выборки
for (t in 1:test_size) {
  idx <- train_size + t # Текущая позиция в тестовой выборке
  # Формула прогноза: h_n+1|n = a0 + a1*h_n^2 + a2*h_{n-1}^2 + a3*h_{n-2}^2
  h_pred[t] <- est_params[1] +
    est_params[2] * process_values[idx - 1]^2 + # Вклад a1
    est_params[3] * process_values[idx - 2]^2 + # Вклад a2
    est_params[4] * process_values[idx - 3]^2 # Вклад a3
}

# Наложение прогнозов на график тестовой выборки
plot(test_data, type = 'l', col = 'seagreen', main = 'Тестовая выборка и прогнозы GARCH(3,0)', ylab = 'h_n', xlab = 'Время (n)')
# График прогнозов (красный)
lines(sqrt(h_pred), col = 'red', lwd = 2)

# Вывод оцененных параметров
cat("Оцененные параметры:\n")
cat("a0:", est_params[1], "\n") # Вывод оценки a0
cat("a1:", est_params[2], "\n") # Вывод оценки a1
cat("a2:", est_params[3], "\n") # Вывод оценки a2
cat("a3:", est_params[4], "\n") # Вывод оценки a3

