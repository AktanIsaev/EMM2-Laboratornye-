set.seed(123) # Фиксируем, для воспроизводимости

# Задание 1 ----

t <- 50 # Задаем время
lambda <- 2 # Параметр распределения
n <- 1000 # Количество реализаций

N_vals <- numeric(n) # Вектор для хранения значений N_t

for (j in 1:n) {
  time <- 0 # Общее время для текущей реализации
  count <- 0 # Счетчик страховых случаев
  
  while (time < t) { 
    time <- time + rexp(1, rate = lambda) # Добавляем интервал
    if (time < t) count <- count + 1 # Увеличиваем счетчик
  }
  
  N_vals[j] <- count # Записываем результат
}

# Задание 2 ----

# Построение гистограммы
hist(N_vals, breaks = 20, probability = TRUE, main = "Распределение N_t и функция вероятности", 
     xlab = "N_t", col = "lightblue", ylim = c(0, 0.05))

# Добавление графика функции вероятности пуассоновского распределения
curve(((lambda * t)^x / factorial(x)) * exp(-lambda * t), add = TRUE, col = "red", lwd = 2)


