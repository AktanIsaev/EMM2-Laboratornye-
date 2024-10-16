# Задание 1 ----

# Устанавливаем параметры и начальные значения
# set.seed(123)  #Если нужно то фиксируем случайные значения для повторяемости эксперимента
n <- 2100  # Количество наблюдений
theta <- c(-0.3, 0.4)  # Параметры AR(2)
A <- c(1, 0.2, 0.1, 0.2)  # Параметры ARCH(3)

# Генерируем шум для модели - независимые нормальные случайные величины
epsilon <- rnorm(n)

# Создаем векторы для хранения значений
x <- rep(0, n)
sigma2 <- rep(0, n)

# Инициализируем начальные значения для sigma^2 и x
# Первый раз просто берем дисперсию шума для начальных значений
sigma2[1:3] <- var(epsilon)
x[1:3] <- rnorm(3)  # Случайно инициализируем первые значения x

# Генерация процесса AR(2)ARCH(3)
for (i in 4:n) {
  # Помним что sigma^2 зависит от прошлых значений x (ARCH(3) часть)
  sigma2[i] <- A[1] + A[2] * x[i-1]^2 + A[3] * x[i-2]^2 + A[4] * x[i-3]^2
  # Теперь генерируем x с учетом модели AR(2) и шума с дисперсией sigma^2
  x[i] <- theta[1] * x[i-1] + theta[2] * x[i-2] + sqrt(sigma2[i]) * epsilon[i]
}

# Построим график полученного ряда
plot(x, type = "l", col = "blue", main = "AR(2)ARCH(3) процесс", xlab = "Время", ylab = "x_n")


# Задание 2 ----

# Количество наблюдений в обучающей выборке (в отношении 20:1)
train_size <- floor(20/21 * n)  # Рассчитываем размер обучающей выборки
test_size <- n - train_size  # Размер тестовой выборки

# Разделение на обучающую и тестовую выборки
train_data <- x[1:train_size]  # Первые 2000 наблюдений (примерно 20:1)
test_data <- x[(train_size + 1):n]  # Остальные наблюдения

# Вывод размеров выборок для проверки
cat("Размер обучающей выборки:", length(train_data), "\n")
cat("Размер тестовой выборки:", length(test_data), "\n")

# Можно визуально посмотреть, как разделение повлияло на данные
plot(train_data, type = "l", col = "green", main = "Обучающая выборка", xlab = "Время", ylab = "x_train")
plot(test_data, type = "l", col = "red", main = "Тестовая выборка", xlab = "Время", ylab = "x_test")


# Задание 3 ----

# Шаг 3a: Оценка параметров theta с помощью функции arima()
# Используем встроенную функцию arima(), чтобы не писать всё вручную
ar_model <- arima(train_data, order = c(2, 0, 0))  # AR(2) модель
theta_hat <- coef(ar_model)[1:2]  # Оценка параметров theta

# Выводим оцененные параметры
cat("Оценка параметров theta через arima():", theta_hat, "\n")

# Проверим невязки, чтобы убедиться, что модель правильно настроена
residuals <- residuals(ar_model)
plot(residuals, type = "l", col = "purple", main = "Остатки AR(2) модели", xlab = "Время", ylab = "Невязки")


# Шаг 3b: Оценка параметров A с помощью функции garch()

# Используем невязки для оценки параметров ARCH(3)
# Здесь мы применяем функцию garch() к ошибкам модели AR(2)
library(tseries)  # Подгружаем пакет для использования функции garch
garch_model <- garch(residuals, order = c(0, 3))  # ARCH(3) модель
A_hat <- coef(garch_model)  # Оценка параметров A

# Выводим оцененные параметры A
cat("Оценка параметров A через garch():", A_hat, "\n")

# Построим график оцененной дисперсии
sigma_hat <- fitted(garch_model)[,1]^2  # Оцененные значения дисперсии
plot(sigma_hat, type = "l", col = "orange", main = "Оцененная дисперсия ARCH(3)", xlab = "Время", ylab = "Sigma^2")


# Задание 4 ----

# Создаем векторы для хранения прогнозов и оценок волатильности
x_forecast <- rep(NA, test_size)
sigma_forecast <- rep(NA, test_size)

# Прогнозируем на один шаг вперед по формуле
for (i in 1:test_size) {
  idx <- train_size + i  # Индекс текущего шага для тестовой выборки
  
  # Прогноз среднего значения (используем оцененные параметры theta_hat)
  Xn <- c(x[idx-1], x[idx-2])  # Вектор значений Xn для AR(2)
  x_forecast[i] <- sum(theta_hat * Xn)  # Прогноз x_n+1|n
  
  # Прогноз дисперсии (используем оцененные параметры A_hat)
  sigma_forecast[i] <- sqrt(A_hat[1] + A_hat[2] * x[idx-1]^2 + A_hat[3] * x[idx-2]^2 + A_hat[4] * x[idx-3]^2)
}
# Границы прогноза: верхняя и нижняя границы по формуле
upper_bound <- x_forecast + sigma_forecast
lower_bound <- x_forecast - sigma_forecast

# Построим график фактических значений процесса (сплошная бирюзовая линия)
plot(test_data, type = "l", col = "turquoise", main = "Прогноз на 1 шаг вперед", 
     xlab = "Наблюдения", ylab = "Значение процесса", ylim = range(c(lower_bound, upper_bound, test_data)))

# Наносим прогнозы (черные пустые окружности)
points(x_forecast, col = "black", pch = 1)

# Наносим прогнозные границы волатильности (красные штрих-пунктирные линии)
lines(upper_bound, col = "red", lty = 2)
lines(lower_bound, col = "red", lty = 2)

# Легенда для графика
legend("topleft", legend = c("Реальные значения", "Прогнозы", "Границы волатильности"),
       col = c("turquoise", "black", "red"), lty = c(1, NA, 2), pch = c(NA, 1, NA))

# Задания 5-7 ----

# Взял котировку акции Apple с 01.01.2000 по 15.10.2024
# В связи с проблемами преобразования формата даты из файла с расширением txt, 
# нашел другой способ через расширение csv и метода read.csv2 

# Загрузка данных из файла CSV
data <- read.csv2("C:\\Users\\Aktan\\Desktop\\AAPL_200101_241015.csv", header = TRUE, stringsAsFactors = FALSE)

# Проверка структуры данных
str(data)

# Преобразование формата даты 
data$X.DATE. <- as.Date(paste0(substr(data$X.DATE., 1, 4), "-", substr(data$X.DATE., 5, 6), "-01"), format = "%Y-%m-%d")

# Проверка на наличие NA в данных
sum_na_high <- sum(is.na(data$X.HIGH.))
cat("Количество NA в наивысших ценах (HIGH):", sum_na_high, "\n")

# Построение графика для динамики актива
plot(data$X.HIGH., type="l", col="blue", main = "Динамика актива",
     xlab = "День", ylab = "Цена")

# Задание 8 ----

# Преобразуем столбец X.HIGH. в числовой формат
data$X.HIGH. <- as.numeric(data$X.HIGH.)

# Приведение данных к стационарному виду через логарифмическую доходность (z_k = ln(P_k / P_k-1))
log_returns <- log(data$X.HIGH.[-1] / data$X.HIGH.[-nrow(data)])

# Вывод первых нескольких значений логарифмической доходности
cat("Логарифмическая доходность (первые 5 значений):", head(log_returns, 5), "\n")

# Задание 9 ----
# Построение графика доходностей {z_k} финансового актива
plot(log_returns, type = "l", col = "green", main = "График логарифмической доходности",
     xlab = "Наблюдения", ylab = "Доходность z_k")

# Задание 10 ----

# Применим преобразования для последовательности логарифмических доходностей {z_n}

# Шаг 2: Разделение на обучающую и тестовую выборки для {z_n}
train_size_z <- floor(20/21 * length(log_returns))  # Рассчитываем размер обучающей выборки
test_size_z <- length(log_returns) - train_size_z  # Размер тестовой выборки

# Разделение на обучающую и тестовую выборки для логарифмических доходностей
train_data_z <- log_returns[1:train_size_z]  # Обучающая выборка
test_data_z <- log_returns[(train_size_z + 1):length(log_returns)]  # Тестовая выборка

# Вывод размеров выборок для проверки
cat("Размер обучающей выборки для z_n:", length(train_data_z), "\n")
cat("Размер тестовой выборки для z_n:", length(test_data_z), "\n")

# Шаг 3a: Оценка параметров AR(2) модели для {z_n}
ar_model_z <- arima(train_data_z, order = c(2, 0, 0))  # AR(2) модель для доходностей
theta_hat_z <- coef(ar_model_z)[1:2]  # Оценка параметров theta

# Выводим оцененные параметры для {z_n}
cat("Оценка параметров theta для {z_n} через arima():", theta_hat_z, "\n")

# Проверим невязки модели AR(2) для {z_n}
residuals_z <- residuals(ar_model_z)
plot(residuals_z, type = "l", col = "purple", main = "Остатки AR(2) модели для {z_n}", xlab = "Время", ylab = "Невязки")

# Шаг 3b: Оценка параметров ARCH(3) модели для {z_n}
garch_model_z <- garch(residuals_z, order = c(0, 3))  # ARCH(3) модель для невязок
A_hat_z <- coef(garch_model_z)  # Оценка параметров A

# Выводим оцененные параметры ARCH(3) для {z_n}
cat("Оценка параметров A для {z_n} через garch():", A_hat_z, "\n")

# Построим график оцененной дисперсии для {z_n}
sigma_hat_z <- fitted(garch_model_z)[,1]^2  # Оцененные значения дисперсии
plot(sigma_hat_z, type = "l", col = "orange", main = "Оцененная дисперсия ARCH(3) для {z_n}", xlab = "Время", ylab = "Sigma^2")

# Шаг 4: Прогнозирование на один шаг вперед для {z_n}
x_forecast_z <- rep(NA, test_size_z)
sigma_forecast_z <- rep(NA, test_size_z)

for (i in 1:test_size_z) {
  idx_z <- train_size_z + i  # Индекс текущего шага для тестовой выборки {z_n}
  
  # Прогноз среднего значения (используем оцененные параметры theta_hat_z)
  Xn_z <- c(log_returns[idx_z-1], log_returns[idx_z-2])  # Вектор значений Xn для AR(2)
  x_forecast_z[i] <- sum(theta_hat_z * Xn_z)  # Прогноз z_n+1|n
  
  # Прогноз дисперсии (используем оцененные параметры A_hat_z)
  sigma_forecast_z[i] <- sqrt(A_hat_z[1] + A_hat_z[2] * log_returns[idx_z-1]^2 + A_hat_z[3] * log_returns[idx_z-2]^2 + A_hat_z[4] * log_returns[idx_z-3]^2)
}

# Границы прогноза для {z_n}: верхняя и нижняя границы
upper_bound_z <- x_forecast_z + sigma_forecast_z
lower_bound_z <- x_forecast_z - sigma_forecast_z

# Построим график фактических значений процесса {z_n}
plot(test_data_z, type = "l", col = "turquoise", main = "Прогноз на 1 шаг вперед для {z_n}", 
     xlab = "Наблюдения", ylab = "Доходность", ylim = range(c(lower_bound_z, upper_bound_z, test_data_z)))

# Наносим прогнозы для {z_n}
points(x_forecast_z, col = "black", pch = 1)

# Наносим прогнозные границы волатильности (красные штрих-пунктирные линии) для {z_n}
lines(upper_bound_z, col = "red", lty = 2)
lines(lower_bound_z, col = "red", lty = 2)

# Легенда для графика {z_n}
legend("topleft", legend = c("Реальные значения", "Прогнозы", "Границы волатильности"),
       col = c("turquoise", "black", "red"), lty = c(1, NA, 2), pch = c(NA, 1, NA))

