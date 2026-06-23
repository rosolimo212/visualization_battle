library(tidyverse)

df <- read.csv('https://raw.githubusercontent.com/rosolimo212/visualization_battle/refs/heads/main/visual_challenge_ver_0.csv')

# Построить подневную динамику выручки одной книжки. 

summary(df)
art_id_current <- 243463

str(df)
create_chart_by_art_id <- function(df, art_id_current, 
  color_line = 'black', color_point = 'black') {
  df |> 
  filter(art_id == art_id_current) |> 
  mutate(calendar_date = as.Date(calendar_date)) |> 
  ggplot(aes(calendar_date, revenue, group = 1))+
  geom_point(color = color_point)+
  geom_line(color = color_line)+
  theme_bw()+
  scale_x_date(name = 'Дата')+
  scale_y_continuous(name = 'Выручка')+
  ggtitle(glue::glue('Динамика продаж art_id = {art_id_current}'))+
  theme(plot.title = element_text(hjust = 0.5))
}

create_chart_by_art_id(df, art_id_current = 86630365, 
  color_line = 'blue')

# Показать взаимное влияние числа покупок и среднего чека в динамике.  
art_id_current <- 86630365
df |> 
  filter(art_id == art_id_current) |> 
  mutate(calendar_date = as.Date(calendar_date)) |> 
  mutate(aov = revenue / sales_qty) |> 
  select(calendar_date, sales_qty, aov) |> 
  pivot_longer(cols = c(sales_qty, aov)) |> 
  ggplot(aes(calendar_date, value, color = name))+
  geom_line()+
  # scale_color_manual(color = RColorBrewer::brewer.pal(n = 2, name = 'Blues')[1:2])
  hrbrthemes::theme_ipsum()

df |> 
  filter(art_id == art_id_current) |> 
  mutate(calendar_date = as.Date(calendar_date)) |> 
  mutate(aov = revenue / sales_qty) |> 
  select(calendar_date, sales_qty, aov) |> 
  pivot_longer(cols = c(sales_qty, aov)) |> 
  ggplot(aes(calendar_date, value))+
  geom_line()+
  facet_wrap(~name, nrow = 2, scales = 'free_y')+
  scale_y_continuous(name = '')+
  scale_x_date(name = '')+
  ggtitle('Средний чек и количество покупок')+
  # scale_y_continuous(limits = c(0, ))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
  # theme(plot.caption = element_blank())

# Максимально компактно показать 3 параметра книжки на одном таймлайне: трафик, цену и выручку. 

# user_visits
# revenue
# avg_sales_catalog_price
str(df |> 
  filter(art_id == art_id_current))

df |> 
  filter(art_id == art_id_current) |> 
  mutate(calendar_date = as.Date(calendar_date)) |> 
  select(calendar_date, user_visits, revenue, avg_sales_catalogue_price) |> 
  pivot_longer(cols = c(user_visits, revenue, avg_sales_catalogue_price)) |> 
  ggplot(aes(calendar_date, value, color = name))+
  geom_line()+
  ggtitle(glue::glue('Ключевые метрики для art_id = {art_id_current}'))+
  scale_color_discrete(name = 'Метрики',
    labels = c(
      user_visits                = "Посещения",
      revenue                    = "Выручка",
      avg_sales_catalogue_price  = "Средняя цена арта в каталоге"
    ))+
  scale_y_continuous(name = '')+
  scale_x_date(name = '')+
  theme_classic()+
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        legend.position = 'right')



