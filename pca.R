###### CREATE PC2 SERIES #########
countries <- c("Austria","Belgium","Finland","France","Greece","Ireland", 
               "Italy", "Netherlands","Portugal","Spain") %>% as_tibble()

# FULL SAMPLE
dates_FS <- spread %>% filter(Dates >= '2001-01-31') %>% 
  filter(Dates <= '2020-01-31') %>% select(Dates)

spreads_FS <- spread %>% filter(Dates >= '2001-01-31') %>% 
  filter(Dates <= '2020-01-31') %>% select(-Dates) %>% as.matrix()

pca_FS <- prcomp(spreads_FS, scale = TRUE)
eigenvalues_FS <- pca_FS$sdev^2 %>% as.numeric() %>% t()

pca_FS <- pca_FS$rotation %>% as_tibble() %>% select(PC1, PC2) %>% 
  cbind(countries) %>% select(country = value, PC1, PC2) %>%
  mutate(PC1 = -PC1)
pca_FS[4,3] <- -pca_FS[4,3] # for normalization

pca_FS <- tibble(Dates = dates_FS,
                 PC1_FS = spreads_FS %*% pca_FS$PC1,
                 PC2_FS = spreads_FS %*% pca_FS$PC2)


# SAMPLE 1 - PRE CRISIS
dates_S1 <- spread %>% filter(Dates >= '2001-01-31') %>% 
  filter(Dates <= '2007-08-31') %>% select(Dates)

spreads_S1 <- spread %>% filter(Dates >= '2001-01-31') %>% 
  filter(Dates <= '2007-08-31') %>% select(-Dates) %>% as.matrix()

pca_S1 <- prcomp(spreads_S1, scale = TRUE)
eigenvalues_S1 <- pca_S1$sdev^2 %>% as.numeric() %>% t()

pca_S1 <- pca_S1$rotation %>% as_tibble() %>% select(PC1, PC2) %>% 
  cbind(countries) %>% select(country = value, PC1, PC2) %>%
  mutate(PC1 = -PC1)

pca_S1[1,3] <- -pca_S1[1,3] # for normalization
pca_S1[2,3] <- -pca_S1[2,3] # for normalization
pca_S1[3,3] <- -pca_S1[3,3] # for normalization
pca_S1[5,3] <- -pca_S1[5,3] # for normalization
pca_S1[7,3] <- -pca_S1[7,3] # for normalization
pca_S1[8,3] <- -pca_S1[8,3] # for normalization
pca_S1[10,3] <- -pca_S1[10,3] # for normalization

pca_S1 <- tibble(Dates = dates_S1,
                 PC1_S1 = spreads_S1 %*% pca_S1$PC1,
                 PC2_S1 = spreads_S1 %*% pca_S1$PC2)


# SAMPLE 2 - CRISIS
dates_S2 <- spread %>%filter(Dates <= '2013-01-31') %>% select(Dates)

spreads_S2 <- spread %>% filter(Dates <= '2013-01-31') %>% 
  select(-Dates) %>% as.matrix()

pca_S2 <- prcomp(spreads_S2, scale = TRUE)
eigenvalues_S2 <- pca_S2$sdev^2 %>% as.numeric() %>% t()

pca_S2 <- pca_S2$rotation %>% as_tibble() %>% select(PC1, PC2) %>% 
  cbind(countries) %>% select(country = value, PC1, PC2) %>%
  mutate(PC2 = -PC2)

pca_S2[1,3] <- -pca_S2[1,3] # for normalization
pca_S2[2,3] <- -pca_S2[2,3] # for normalization
pca_S2[3,3] <- -pca_S2[3,3] # for normalization
pca_S2[5,3] <- -pca_S2[5,3] # for normalization
pca_S2[6,3] <- -pca_S2[6,3] # for normalization
pca_S2[7,3] <- -pca_S2[7,3] # for normalization
pca_S2[8,3] <- -pca_S2[8,3] # for normalization
pca_S2[9,3] <- -pca_S2[9,3] # for normalization
pca_S2[10,3] <- -pca_S2[10,3] # for normalization

pca_S2 <- tibble(Dates = dates_S2,
                 PC1_S2 = spreads_S2 %*% pca_S2$PC1,
                 PC2_S2 = spreads_S2 %*% pca_S2$PC2)

# SAMPLE 3 - POST CRISIS
dates_S3 <- spread %>% filter(Dates >= '2013-01-31') %>% 
  filter(Dates <= '2020-01-31') %>% select(Dates)

spreads_S3 <- spread %>% filter(Dates >= '2013-01-31') %>% 
  filter(Dates <= '2020-01-31') %>% select(-Dates) %>% as.matrix()

pca_S3 <- prcomp(spreads_S3, scale = TRUE)
eigenvalues_S3 <- pca_S3$sdev^2 %>% as.numeric() %>% t()

pca_S3 <- pca_S3$rotation %>% as_tibble() %>% select(PC1, PC2) %>% 
  cbind(countries) %>% select(country = value, PC1, PC2) %>%
  mutate(PC1 = -PC1, PC2 = -PC2)

pca_S3[7,3] <- -pca_S3[7,3] # for normalization
pca_S3[8,3] <- -pca_S3[8,3] # for normalization

pca_S3 <- tibble(Dates = dates_S3,
                 PC1_S3 = spreads_S3 %*% pca_S3$PC1,
                 PC2_S3 = spreads_S3 %*% pca_S3$PC2)

rm(dates_FS, dates_S1, dates_S2, dates_S3)

####### JOIN SERIES #########
PCA <- full_join(pca_FS %>% select(-PC1_FS), pca_S1 %>% select(-PC1_S1), by = c("Dates")) 
PCA <- full_join(PCA, pca_S2 %>% select(-PC1_S2), by = c("Dates"))
PCA <- full_join(PCA, pca_S3 %>% select(-PC1_S3), by = c("Dates")) %>% 
  as_tibble()
