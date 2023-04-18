library(tidyverse)
library(readxl)
library(broom)
library(umap)
library(ggrepel)
library(ggplot2)
library(purrr)
library(umap)

stores_tbl <- read_excel(path = "Additional Data/breakfast_at_the_frat.xlsx",
                         sheet = "dh Store Lookup",
                         skip = 1)

products_tbl <- read_excel(path = "Additional Data/breakfast_at_the_frat.xlsx",
                         sheet = "dh Products Lookup",
                         skip = 1)

transaction_tbl <- read_excel(path = "Additional Data/breakfast_at_the_frat.xlsx",
                         sheet = "dh Transaction Data",
                         skip = 1)

orderlines_tbl <- transaction_tbl %>%
  left_join(products_tbl) %>%
  left_join(stores_tbl, by = c("STORE_NUM" = "STORE_ID"))

glimpse(orderlines_tbl)

customer_trends_tbl <- orderlines_tbl %>%
  
  mutate(BRANDED = ifelse(MANUFACTURER == "PRIVATE LABEL", "no", "yes")) %>%
  select(STORE_NAME, PRICE, UPC, DESCRIPTION, CATEGORY, SUB_CATEGORY, BRANDED, UNITS) %>%
  
  group_by(STORE_NAME, PRICE, UPC, DESCRIPTION, CATEGORY, SUB_CATEGORY, BRANDED) %>%
  summarise(QUANTITY_PURCHASED = sum(UNITS)) %>%
  ungroup() %>%
  
  group_by(STORE_NAME) %>%
  mutate(PROP_OF_TOTAL = QUANTITY_PURCHASED/sum(QUANTITY_PURCHASED)) %>%
  ungroup()

customer_product_tbl <- customer_trends_tbl %>%
  select(STORE_NAME, UPC, PROP_OF_TOTAL) %>%
  pivot_wider(names_from = UPC, values_from = PROP_OF_TOTAL, values_fill = 0) %>%
  ungroup()
glimpse(customer_product_tbl)

#modelling
kmeans_obj <- customer_product_tbl %>%
  select(-STORE_NAME) %>%
  kmeans(centers = 3, nstart = 100)

kmeans_obj$cluster

broom::tidy(kmeans_obj) %>% glimpse()
broom::glance(kmeans_obj)
broom::augment(kmeans_obj, customer_product_tbl) %>%
  select(STORE_NAME, .cluster)

kmeans_mapper <- function(centers = 3) {
  customer_product_tbl %>%
    select(-STORE_NAME) %>%
    kmeans(centers = centers, nstart = 100)
}
3%>% kmeans_mapper() %>% glance()

kmeans_mapped_tbl <- tibble(centers = 1:15) %>%
  mutate(k_means = centers %>% map(kmeans_mapper)) %>%
  mutate(glance = k_means %>% map(glance))

kmeans_mapped_tbl 

kmeans_mapped_tbl %>%
  unnest(glance) %>%
  select(centers, tot.withinss) %>%
  
  ggplot(aes(centers, tot.withinss)) +
  geom_point(color = "#2dc6d6", size = 4) +
  geom_line(color = "#2dc6d6", size = 1) +
  ggrepel::geom_label_repel(aes(label = centers), color = "#2dc6d6") +
  labs(title = "Skree Plot",
       subtitle = "Measures the distance each of the customer are from the closes K-Means center",
       caption = "Conclusion: Based on the Scree Plot, we select 3 clusters to segment the customer base.")
  
umap_obj <- customer_product_tbl %>%
  select(-STORE_NAME) %>%
  umap()
View(umap_obj)

umap_results_tbl <- umap_obj$layout %>%
  as_tibble(.name_repair = "unique") %>%
  set_names(c("x","y")) %>%
  bind_cols(
    customer_product_tbl %>% select(STORE_NAME)
  )

umap_results_tbl %>%
  ggplot(aes(x, y)) +
  geom_point() +
  geom_label_repel(aes(label = STORE_NAME), size = 3)

kmeans_3_obj <- kmeans_mapped_tbl %>%
  pull(k_means) %>%
  pluck(3)

kmeans_3_clusters_tbl <- kmeans_3_obj %>%
  augment(customer_product_tbl) %>%
  select(STORE_NAME, .cluster)

umap_kmeans_3_results_tbl <- umap_results_tbl %>%
  left_join(kmeans_3_clusters_tbl)

umap_kmeans_3_results_tbl %>%
  mutate(label_text = str_glue("Customer: {STORE_NAME}
                                 Cluster: {.cluster}")) %>%
  
  ggplot(aes(x, y, color = .cluster)) +
  
  # Geometries
  geom_point() +
  geom_label_repel(aes(label = label_text), size = 2, fill = "#282A36") +
  
  # Formatting
  scale_color_manual(values=c("#2d72d6", "#2dc6d6", "#2dd692")) +
  labs(title = "Customer Segmentation: 2D Projection",
       subtitle = "UMAP 2D Projection with K-Means Cluster Assignment",
       caption = "Conclusion: 3 Customer Segments identified using 2 algorithms") +
  theme(legend.position = "none")






