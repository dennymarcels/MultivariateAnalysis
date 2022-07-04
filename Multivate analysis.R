source('Auxiliary functions.R')
library('plot3D')

# 1 DATASET PEQUENO ----

# 1.1 Definição ----

# Variáveis
set.seed(1)

h = c(sample(30:37, 10, replace = T), sample(43:50, 10, replace = T))
m = sort(sample(30:50, 20, replace = T))
casais = data.frame(h = h, m = m)
casais = casais[sample(20, 20), ]
rownames(casais) <- 1:20
h = casais[, 'h']
m = casais[, 'm']
casais_var = casais

# Grupos
casais$cidade = factor(sapply(h, function(x) ifelse((x > 35)&(x < 45), 'A', 'B')))
casais_gro = casais$cidade

# Variáveis correlatas
casais$poupança = 1000 * (h + m)
casais_cor = casais[, 'poupança']

# Cores para os plots
casais_col = rainbow(nrow(casais))
casais_gro_col = c('black', 'gray')[casais_gro]

# Legenda para os plots
legend_gro = data.frame('legend' = as.character(unique(casais_gro)), 'col' = as.character(unique(casais_gro_col)), stringsAsFactors = F)
legend_gro = legend_gro[order(legend_gro[, 'legend']), ]

# 1.2 Exploração ----

# 1.2.1 Visualização ----

# Cada variável individualmente

png('P - h[1].png', width = 1000, height = 300, res = 200)
par(mfrow = c(1, 1), mar = c(2.5, 1, 2.5, 1))
t = table(h)
yh = numeric()
for(name in names(t)){
    n = t[name]
    n = if(n == 1) {0} else if(n == 2) {c(-0.2, 0.2)} else if(n == 3){c(-0.4, 0, 0.4)}
    yh[which(h == as.numeric(name))] = n  
}
plot(h, yh, xlim = c(30, 50), ylim = c(-1, 1), pch = 24, yaxt = 'n', xlab = '', ylab = '', main = paste0('Idade homem\nvar = ', round(var(h),2)))
points(mean(h), 0, pch = 4, col = 'gray')
dev.off()

png('P - m[1].png', width = 1000, height = 300, res = 200)
par(mfrow = c(1, 1), mar = c(2.5, 1, 2.5, 1))
t = table(m)
ym = numeric()
for(name in names(t)){
    n = t[name]
    n = if(n == 1) {0} else if(n == 2) {c(-0.2, 0.2)} else if(n == 3){c(-0.4, 0, 0.4)}
    ym[which(m == as.numeric(name))] = n  
}
plot(m, ym, xlim = c(30, 50), ylim = c(-1, 1), pch = 25, yaxt = 'n', xlab = '', ylab = '', main = paste0('Idade mulher\nvar = ', round(var(m),2)))
points(mean(m), 0, pch = 4, col = 'gray')
dev.off()

# Identificando as cidades de origem

png('P - h[2].png', width = 1000, height = 400, res = 200)
par(mfrow = c(1, 1), mar = c(2.5, 1, 2.5, 1))
plot(h, yh, xlim = c(30, 50), ylim = c(-2, 2), pch = 24, col = casais_gro_col, lwd = 2, yaxt = 'n', xlab = '', ylab = '', main = paste0('Idade homem\nvar = ', round(var(h),2)))
legend('bottomright', cex = 0.5, title = 'Cidade', legend = legend_gro[, 'legend'], pch = 24, col = legend_gro[, 'col'], horiz = T)
dev.off()

png('P - m[2].png', width = 1000, height = 400, res = 200)
par(mfrow = c(1, 1), mar = c(2.5, 1, 2.5, 1))
plot(m, ym, xlim = c(30, 50), ylim = c(-2, 2), pch = 25, col = as.character(casais_gro_col), lwd = 2, yaxt = 'n', xlab = '', ylab = '', main = paste0('Idade mulher\nvar = ', round(var(m),2)))
legend('bottomright', cex = 0.5, title = 'Cidade', legend = legend_gro[, 'legend'], pch = 25, col = legend_gro[, 'col'], horiz = T)
dev.off()

# Identificando os casais

png('P - h[3].png', width = 1000, height = 400, res = 200)
par(mfrow = c(1, 1), mar = c(2.5, 1, 2.5, 1))
plot(h, yh, xlim = c(30, 50), ylim = c(-2, 2), pch = 24, col = casais_gro_col, bg = casais_col, lwd = 1.5, yaxt = 'n', xlab = '', ylab = '', main = paste0('Idade homem\nvar = ', round(var(h),2)))
legend('bottomright', cex = 0.5, title = 'Cidade', legend = legend_gro[, 'legend'], pch = 24, col = legend_gro[, 'col'], horiz = T)
dev.off()

png('P - m[3].png', width = 1000, height = 400, res = 200)
par(mfrow = c(1, 1), mar = c(2.5, 1, 2.5, 1))
plot(m, ym, xlim = c(30, 50), ylim = c(-2, 2), pch = 25, col = casais_gro_col, bg = casais_col, lwd = 1.5, yaxt = 'n', xlab = '', ylab = '', main = paste0('Idade mulher\nvar = ', round(var(m),2)))
legend('bottomright', cex = 0.5, title = 'Cidade', legend = legend_gro[, 'legend'], pch = 25, col = legend_gro[, 'col'], horiz = T)
dev.off()

# Ambas as variáveis ao mesmo tempo

png('P - h vs m.png', width = 1000, height = 1000, res = 200)
par(mfrow = c(1, 1), mar = c(4, 4, 2.5, 2.5))
plot(h, m, xlim = c(30, 50), ylim = c(30, 50), cex = 1.5, pch = 23, col = casais_gro_col, bg = casais_col, lwd = 1.5, main = 'Idade homem vs Idade mulher', xlab = paste0(c('Idade homem :: var = ', round(var(h), 2)), collapse = ''), ylab = paste0(c('Idade mulher :: var = ', round(var(m), 2)), collapse = ''))
legend('bottomright', cex = 0.5, title = 'Cidade', legend = legend_gro[, 'legend'], pch = 23, col = legend_gro[, 'col'], horiz = T)
dev.off()

# Sem identificar subgrupos

png('P - h vs m [b&w].png', width = 1000, height = 1000, res = 200)
par(mfrow = c(1, 1), mar = c(2.5, 2.5, 2.5, 2.5))
plot(h, m, xlim = c(30, 50), ylim = c(30, 50), cex = 1.5, pch = 23, lwd = 1.5, main = 'Idade homem vs Idade mulher', xlab = 'Idade homem', ylab = 'Idade mulher')
dev.off()

# 1.2.2 K-Means & PAM ----

casais_dist = dist(casais_var)

# K-Means

K = 1:10
temp <- numeric()
for (k in K){
    temp <-c(temp, sum(kmeans(casais_var, k, nstart = 100)$withinss))}
png('P - kmeans.png', width = 1500, height = 1000, res = 200)
par(mfrow = c(1, 1), mar = c(5, 5, 2, 1))
plot(K, temp, xaxt = 'n', cex = 2, cex.lab = 1, cex.axis = 1, type="o", xlab = "Número de k", ylab = "Soma de Quadrados")
axis(1, at = K, labels = K, cex.axis = 1)
dev.off()

# PAM

K = 2:10
png('P - PAM.png', width = 1500, height = 1000, res = 200)
par(mar = c(4, 4, 1.5, 1), mfrow = c(ceiling(sqrt(diff(range(K))+1)), ceiling((diff(range(K))+1)/ceiling(sqrt(diff(range(K))+1)))))
for (k in K){
    pam = cluster::pam(casais_dist, k)
    silinfo = pam$silinfo$widths
    cluster = silinfo[, 1]
    col = rainbow(k)[cluster]
    plot(silinfo[, 3], ylim = c(0, 1), type = 'h', main = paste0('Score da silhueta para k = ', k), ylab = 'Score', col = col)
    abline(h = pam$silinfo$avg.width, lty = 2)
}
dev.off()

pam = cluster::pam(casais_dist, 2)
casais_pam = as.factor(pam$clustering)
casais$pam = casais_pam
casais_pam_col = rainbow(2)[casais_pam]

# Ambas as variáveis de novo

png('P - h vs m (PAM).png', width = 1000, height = 1000, res = 200)
par(mfrow = c(1, 1), mar = c(2.5, 2.5, 2.5, 2.5))
plot(h, m, xlim = c(30, 50), ylim = c(30, 50), cex = 1.5, pch = 23, col = casais_gro_col, bg = casais_pam_col, lwd = 1.5, main = 'Idade homem vs Idade mulher', xlab = 'Idade homem', ylab = 'Idade mulher')
legend('bottomright', cex = 0.75, title = 'Cidade', pch = 23, legend = legend_gro[, 'legend'], col = legend_gro[, 'col'], horiz = T)
legend_pam = data.frame('legend' = as.character(unique(casais_pam)), 'col' = as.character(unique(casais_pam_col)), stringsAsFactors = F)
legend_pam = legend_pam[order(legend_pam[, 'legend']), ]
legend('topleft', cex = 0.75, title = 'PAM', legend = legend_pam[, 'legend'], fill = legend_pam[, 'col'], horiz = T)
dev.off()

png('P - heatmap (Cidade x PAM).png', width = 1000, height = 900, res = 200)
par(mar = c(0, 0, 0, 0))
table = table(casais_gro, casais_pam)/rowSums(table(casais_gro, casais_pam))
col = colorRampPalette(c('white', 'black'))(100)[(min(table)*100):(max(table)*100)]
heatmap(table, mar = c(4, 8), cexRow = 3.6, cexCol = 4, col = col, Colv = NA, Rowv = NA, reorderfun = NA, scale = 'none')
legend('bottomright', title = 'Proporção', cex = 1.5, fill = c('white', 'black'), legend = c('0%', '100%'))
dev.off()

# 1.2.3 Hierarchical clustering ----

casais_clust = hclust(casais_dist, method = 'average')

colsidecolors = as.matrix(data.frame('cidade' = casais_gro_col, 'PAM' = casais_pam_col))

png('P - hierarchical clustering.png', width = 1000, height = 750, res = 200)
par(mar = c(0, 0, 0, 0), mfrow = c(1, 1))
col = colorRampPalette(c('blue', 'red'))(20)[(min(casais_var)-30):(max(casais_var)-30)]
heatmapplus(t(as.matrix(casais_var)), mar = c(2, 10), cexRow = 2, Colv = as.dendrogram(casais_clust), ColSideColors = colsidecolors, distfun = stats::dist, col = col, scale = 'none')
legend('topright', title = 'Cidade | PAM', cex = 0.75, ncol = 2, fill = c(legend_gro[, 'col'], legend_pam[, 'col']), legend = c(legend_gro[, 'legend'], legend_pam[, 'legend']))
legend('bottomright', title = 'Idade', cex = 0.75, fill = c('blue', 'red'), legend = c(30, 50))
dev.off()

# 1.2.4 PCA ----

png('P - h vs m, orthogonal h.png', width = 1000, height = 1000, res = 200)
par(mfrow = c(1, 1), mar = c(4, 4, 2.5, 2.5))
plot(h, m, xlim = c(30, 50), ylim = c(30, 50), cex = 1.5, pch = 23, col = casais_gro_col, bg = casais_col, lwd = 1.5, main = 'Idade homem vs Idade mulher', xlab = paste0(c('Idade homem :: var = ', round(var(h), 2)), collapse = ''), ylab = paste0(c('Idade mulher :: var = ', round(var(m), 2)), collapse = ''))
legend('bottomright', cex = 0.5, title = 'Cidade', pch = 23, legend = legend_gro[, 'legend'], col = legend_gro[, 'col'], horiz = T)
olm_coefs <- olm_two_axis(h, m)
lines_two_axis(olm_coefs, T, F, lty = 2)
dev.off()

png('P - h vs m, orthogonal h & m.png', width = 1000, height = 1000, res = 200)
par(mfrow = c(1, 1), mar = c(4, 4, 2.5, 2.5))
plot(h, m, xlim = c(30, 50), ylim = c(30, 50), cex = 1.5, pch = 23, col = casais_gro_col, bg = casais_col, lwd = 1.5, main = 'Idade homem vs Idade mulher', xlab = paste0(c('Idade homem :: var = ', round(var(h), 2)), collapse = ''), ylab = paste0(c('Idade mulher :: var = ', round(var(m), 2)), collapse = ''))
legend('bottomright', cex = 0.5, title = 'Cidade', pch = 23, legend = legend_gro[, 'legend'], col = legend_gro[, 'col'], horiz = T)
lines_two_axis(olm_coefs, lty = 2)
dev.off()

pca = prcomp(casais_var)
rot = pca$rotation
pca_var = pca$sdev^2
pca = pca$x
pca_var_perc = round(pca_var/sum(pca_var)*100, 2)
pca_var_perc_acc = cumsum(pca_var_perc)

png('P - PCA.png', width = 1000, height = 1000, res = 200)
par(mfrow = c(1, 1), mar = c(4, 4, 2.5, 2.5))
lims = c(min(pca[, 1], pca[, 2]), max(pca[, 1], pca[, 2]))
plot(pca[, 1], pca[, 2], xlim = lims, ylim = lims, pch = 23, bg = casais_col, cex = 1.5, main = 'PCA', xlab = paste0('PC1 | ', pca_var_perc[1], '% | ', pca_var_perc_acc[1], '%'), ylab = paste0('PC2 | ', pca_var_perc[2], '% | ', pca_var_perc_acc[2], '%'))

olm_coefs <- olm_two_axis(pca[, 1], pca[, 2])
lines_two_axis(olm_coefs, lty = 2)

#text(rev(lims)[1], rev(lims)[2], cex = 1, labels = paste0(capture.output(rot), collapse = '\n'), adj = c(1, 0), family = 'mono')
dev.off()

png('P - PCA, PC1.png', width = 1000, height = 300, res = 200)
par(mfrow = c(1, 1), mar = c(4, 2.5, 2.5, 2.5))
plot(pca[, 1], rep(0, length(pca[, 1])), pch = 23, bg = adjustcolor(casais_col, alpha.f = 0.8), yaxt = 'n', cex = 1.5, main = 'PC1', xlab = paste0('PC1 | ', pca_var_perc[1], '% | ', pca_var_perc_acc[1], '%'), ylab = NA)
abline(h = mean(pca[, 2]), lty = 2)
dev.off()

# 1.3 Discriminação ----

# 1.3.1 CDA ----

lm = lm(as.matrix(casais_var) ~ cidade, casais)
cda <- candisc::candisc(lm, data = casais)

png('P - CDA, cidade [1].png', width = 1000, height = 1000, res = 200)
par(mfrow = c(1, 1), mar = c(4, 4, 2.5, 2.5))
plot(h, m, xlim = c(30, 50), ylim = c(30, 50), pch = 23, bg = casais_gro_col, cex = 1.5, main = 'h vs m', xlab = 'h', ylab = 'm')
legend('bottomright', cex = 0.75, title = 'Cidade', legend = legend_gro[, 'legend'], fill = legend_gro[, 'col'], horiz = T)
dev.off()

png('P - CDA, cidade [2].png', width = 1000, height = 1000, res = 200)
par(mfrow = c(1, 1), mar = c(4, 4, 2.5, 2.5))

plot(cda$scores$Can1, rep(0, length(cda$scores$Can1)), yaxt = 'n', pch = c(22:23)[casais$cidade], bg = adjustcolor(casais_gro_col, alpha.f = 0.5), xlab = 'CAN1 (100%)', ylab = NA, cex = 1.5, main = 'CDA')
mult = max(abs(cda$scores$Can1))/max(abs(cda$structure))
arrows(0, 0, cda$structure * mult, 0, length = 0.1, lwd = 1.5, col = 'purple')
text(cda$structure * mult, 0, pos = 3, labels = dimnames(cda$structure)[[1]], col = 'purple')
legend('bottomright', title = paste0(cda$term, '\n'), bty = 'n', legend = levels(cda$scores[, cda$term]), fill = legend_gro[, 'col'], x.intersp = 0.25, y.intersp = 0.7)
dev.off()

lm = lm(as.matrix(casais_var) ~ pam, casais)
cda <- candisc::candisc(lm, data = casais)

png('P - CDA, pam [1].png', width = 1000, height = 1000, res = 200)
par(mfrow = c(1, 1), mar = c(4, 4, 2.5, 2.5))
plot(h, m, xlim = c(30, 50), ylim = c(30, 50), pch = 23, bg = casais_pam_col, cex = 1.5, main = 'h vs m', xlab = 'h', ylab = 'm')
legend('bottomright', cex = 0.75, title = 'PAM', legend = legend_pam[, 'legend'], fill = legend_pam[, 'col'], horiz = T)
dev.off()

png('P - CDA, pam [2].png', width = 1000, height = 1000, res = 200)
par(mfrow = c(1, 1), mar = c(4, 4, 2.5, 2.5))
plot(cda$scores$Can1, rep(0, length(cda$scores$Can1)), yaxt = 'n', pch = 23, bg = adjustcolor(casais_pam_col, alpha.f = 0.5), xlab = 'CAN1 (100%)', ylab = NA, cex = 1.5, main = 'CDA')
mult = max(abs(cda$scores$Can1))/max(abs(cda$structure))
arrows(0, 0, cda$structure * mult, 0, length = 0.1, lwd = 1.5, col = 'purple')
text(cda$structure * mult, 0, pos = 3, labels = dimnames(cda$structure)[[1]], col = 'purple')
legend('bottomright', title = paste0(cda$term, '\n'), bty = 'n', legend = levels(cda$scores[, cda$term]), fill = legend_pam[, 'col'], x.intersp = 0.25, y.intersp = 0.7)
dev.off()

# 1.3.2 Árvores de decisão ----

tree = tree::tree(cidade ~ h+m, casais)

png('P - arvore de decisao, cidade.png', width = 1000, height = 1000, res = 200)
par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))
plot(tree)
text(tree, cex = 2)
dev.off()

tree = tree::tree(pam ~ h+m, casais)

png('P - arvore de decisao, pam.png', width = 1000, height = 1000, res = 200)
par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))
plot(tree)
text(tree, cex = 2)
dev.off()

# 1.4 Interpretação ----

# 1.4.1 RDA ----

rda = vegan::rda(formula = casais_var ~ cidade + poupança, data = casais)

for(scaling in 1:2){
    plot_info = plot(rda, scaling = scaling)
    summ = summary(rda)
    
# Plotting RDA
    
    species = plot_info$species
    sites = plot_info$sites
    centroids = plot_info$centroids
    biplot = plot_info$biplot * attr(plot_info$biplot, 'arrow.mul')
    
    comp = round(c(summ$cont$importance[2], summ$cont$importance[5])*100, 1)
    comp_acc = cumsum(comp)
    cons = round(c(summ$concont$importance[2], summ$concont$importance[5])*100, 1)
    cons_acc = cumsum(cons)
    
    xlim = c(min(species[, 1], sites[, 1], centroids[, 1], biplot[, 1]), max(species[, 1], sites[, 1], centroids[, 1], biplot[, 1])) * 1.1
    ylim = c(min(species[, 2], sites[, 2], centroids[, 2], biplot[, 2]), max(species[, 2], sites[, 2], centroids[, 2], biplot[, 2])) * 1.1
    
    png(paste0('P - RDA, scaling ', scaling, '.png'), width = 1000, height = 1000, res = 200)
    par(mfrow = c(1, 1), mar = c(4, 4, 2.5, 2.5))
    plot(sites[, 1], sites[, 2], xlim = xlim, ylim = ylim, pch = 23, cex = 2, bg = casais_col,
         xlab = paste0('RDA1 (', comp[1], '% | ', comp_acc[1], '% || ', cons[1], '% | ', cons_acc[1], '%)'), 
         ylab = paste0('RDA2 (', comp[2], '% | ', comp_acc[2], '% || ', cons[2], '% | ', cons_acc[2], '%)'))
    abline(h = 0, v = 0, lty = 2)
    points(species[, 1], species[, 2], cex = 2, pch = 4, lwd = 2, col = 'gray')
    text(species[, 1], species[, 2], cex = 1, pos = 3, offset = 1.5, labels = rownames(species), col = 'gray')
    points(centroids[, 1], centroids[, 2], cex = 2, pch = 4, lwd = 2, col = 'black')
    text(centroids[, 1], centroids[, 2], cex = 1, pos = 3, offset = 1.5, labels = rownames(centroids))
    arrows(0, 0, biplot[, 1], biplot[, 2], lwd = 1.5, length = 0.1)
    text(biplot[, 1], biplot[, 2], cex = 1, pos = 3, offset = 1.5, labels = rownames(biplot))
    dev.off()
}

# Plotting PCA

sites = summ$sites[, 3]
comp = round(summ$cont$importance[8]*100, 1)
comp_acc = round(summ$cont$importance[9]*100, 1)

png('P - RDA (PC1).png', width = 1000, height = 300, res = 200)
par(mfrow = c(1, 1), mar = c(4, 2.5, 2.5, 2.5))
plot(sites, rep(0, length(sites)), pch = 23, cex = 2, bg = casais_col, yaxt = 'n', main = 'PCA',
     xlab = paste0('PC1 (', comp, '% | ', comp_acc, '%)'), 
     ylab = NA)
abline(h = 0, lty = 2)
dev.off()

# 2 DATASET MÉDIO: Bolas ----

# 2.1 Definição ----

# Variáveis
set.seed(1)

# A
A_vermelho = 1-rexp(50, 10)
A_verde = runif(50, 0, 0.5)
A_azul = rnorm(50, 0.30, 0.1)

# B
B_vermelho = 1-rexp(50, 10)
B_verde = runif(50, 0, 0.5)
B_azul = rnorm(50, 0.30, 0.1)

# C
C_vermelho = 0.9-rexp(50, 8)
C_verde = runif(50, 0.40, 0.80)
C_azul = rnorm(50, 0.85, 0.01)

# D
D_vermelho = 0.85-rexp(50, 8)
D_verde = runif(50, 0, 0.60)
D_azul = rnorm(50, 0.85, 0.05)

# E
E_vermelho = 0.4-rexp(50, 15)
E_verde = runif(50, 0.55, 1)
E_azul = rnorm(50, 0.30, 0.1)

# F
F_vermelho = 0.4-rexp(50, 25)
F_verde = runif(50, 0.55, 0.95)
F_azul = rnorm(50, 0.30, 0.1)

# Grupos
lotes = c(rep('A', 50), rep('B', 50), rep('C', 50), rep('D', 50), rep('E', 50), rep('F', 50))

bolas = data.frame(
    'lote' = lotes,
    'vermelho' = c(A_vermelho, B_vermelho, C_vermelho, D_vermelho, E_vermelho, F_vermelho),
    'verde' = c(A_verde, B_verde, C_verde, D_verde, E_verde, F_verde),
    'azul' = c(A_azul, B_azul, C_azul, D_azul, E_azul, F_azul))

# Variáveis correlatas
bolas$fabricante = c(rep('X', 100), rep('Y', 100), rep('Z', 100))
bolas$preço = 1*bolas[, 'verde'] + 0.5*bolas[, 'azul'] + 0.5*bolas[, 'vermelho']
bolas$preferência = 0.6 * bolas[, 'azul'] + 0.3 * bolas[, 'vermelho'] + 0.1 * bolas[, 'vermelho'] * bolas[, 'azul']

# Separando os três tipos de dados
bolas_var = bolas[, c('vermelho', 'verde', 'azul')]
bolas_gro = bolas$lote
bolas_cor = bolas[, c('fabricante', 'preço', 'preferência')]

# Cores para os plots
bolas_col = rgb(bolas[, 'vermelho'], bolas[, 'verde'], bolas[, 'azul'])
bolas_gro_col = rainbow(length(levels(bolas_gro)))[bolas_gro]

# Legenda para os plots
legend_gro = data.frame('legend' = as.character(unique(bolas_gro)), 'col' = as.character(unique(bolas_gro_col)), stringsAsFactors = F)
legend_gro = legend_gro[order(legend_gro[, 'legend']), ]

# 2.2 Exploração ----

# 2.2.1 Visualização ----

# Cada variável separada por lote

png('M - canais de cor por lote.png', width = 1200, height = 1200, res = 200)
par(mfrow = c(3, 1), mar = c(2, 2, 1, 1))
plot(rep(1, 50), A_vermelho, xaxt = 'n', cex = 2, ylim = c(0, 1), xlim = c(0, 7), pch = 21, bg = adjustcolor(rgb0(A_vermelho, 0, 0), alpha.f = 0.5))
axis(1, at = 1:6, labels = LETTERS[1:6])
abline(h = 0.5, lty = 2, col = 'gray')
points(rep(2, 50), B_vermelho, cex = 2, ylim = c(0, 1), pch = 21, bg = adjustcolor(rgb0(B_vermelho, 0, 0), alpha.f = 0.5))
points(rep(3, 50), C_vermelho, cex = 2, ylim = c(0, 1), pch = 21, bg = adjustcolor(rgb0(C_vermelho, 0, 0), alpha.f = 0.5))
points(rep(4, 50), D_vermelho, cex = 2, ylim = c(0, 1), pch = 21, bg = adjustcolor(rgb0(D_vermelho, 0, 0), alpha.f = 0.5))
points(rep(5, 50), E_vermelho, cex = 2, ylim = c(0, 1), pch = 21, bg = adjustcolor(rgb0(E_vermelho, 0, 0), alpha.f = 0.5))
points(rep(6, 50), F_vermelho, cex = 2, ylim = c(0, 1), pch = 21, bg = adjustcolor(rgb0(F_vermelho, 0, 0), alpha.f = 0.5))

plot(rep(1, 50), A_verde, xaxt = 'n', cex = 2, ylim = c(0, 1), xlim = c(0, 7), pch = 21, bg = adjustcolor(rgb0(0, A_verde, 0), alpha.f = 0.5))
axis(1, at = 1:6, labels = LETTERS[1:6])
abline(h = 0.5, lty = 2, col = 'gray')
points(rep(2, 50), B_verde, cex = 2, ylim = c(0, 1), pch = 21, bg = adjustcolor(rgb0(0, B_verde, 0), alpha.f = 0.5))
points(rep(3, 50), C_verde, cex = 2, ylim = c(0, 1), pch = 21, bg = adjustcolor(rgb0(0, C_verde, 0), alpha.f = 0.5))
points(rep(4, 50), D_verde, cex = 2, ylim = c(0, 1), pch = 21, bg = adjustcolor(rgb0(0, D_verde, 0), alpha.f = 0.5))
points(rep(5, 50), E_verde, cex = 2, ylim = c(0, 1), pch = 21, bg = adjustcolor(rgb0(0, E_verde, 0), alpha.f = 0.5))
points(rep(6, 50), F_verde, cex = 2, ylim = c(0, 1), pch = 21, bg = adjustcolor(rgb0(0, F_verde, 0), alpha.f = 0.5))

plot(rep(1, 50), A_azul, xaxt = 'n', cex = 2, ylim = c(0, 1), xlim = c(0, 7), pch = 21, bg = adjustcolor(rgb0(0, 0, A_azul), alpha.f = 0.5))
axis(1, at = 1:6, labels = LETTERS[1:6])
abline(h = 0.5, lty = 2, col = 'gray')
points(rep(2, 50), B_azul, cex = 2, ylim = c(0, 1), pch = 21, bg = adjustcolor(rgb0(0, 0, B_azul), alpha.f = 0.5))
points(rep(3, 50), C_azul, cex = 2, ylim = c(0, 1), pch = 21, bg = adjustcolor(rgb0(0, 0, C_azul), alpha.f = 0.5))
points(rep(4, 50), D_azul, cex = 2, ylim = c(0, 1), pch = 21, bg = adjustcolor(rgb0(0, 0, D_azul), alpha.f = 0.5))
points(rep(5, 50), E_azul, cex = 2, ylim = c(0, 1), pch = 21, bg = adjustcolor(rgb0(0, 0, E_azul), alpha.f = 0.5))
points(rep(6, 50), F_azul, cex = 2, ylim = c(0, 1), pch = 21, bg = adjustcolor(rgb0(0, 0, F_azul), alpha.f = 0.5))

dev.off()

# Cada variável sem separar por lote

png('M - canais de cor.png', width = 1200, height = 1200, res = 200)
par(mfrow = c(3, 1), mar = c(2, 2, 2.5, 1))
plot(bolas[, 'vermelho'], rep(0, 300), main = 'Todas as amostras, espectro VERMELHO', pch = 21, cex = 2, xlim = c(0, 1), bg = adjustcolor(bolas_col, alpha.f = 0.5))
plot(bolas[, 'verde'], rep(0, 300), main = 'Todas as amostras, espectro VERDE', pch = 21, cex = 2, xlim = c(0, 1), bg = adjustcolor(bolas_col, alpha.f = 0.5))
plot(bolas[, 'azul'], rep(0, 300), main = 'Todas as amostras, espectro AZUL', pch = 21, cex = 2, xlim = c(0, 1), bg = adjustcolor(bolas_col, alpha.f = 0.5))
dev.off()

# Histogramas

png('M - histograma (vermelho).png', width = 500, height = 500, res = 200)
par(mfrow = c(1, 1), mar = c(2, 2, 1, 1))
hist(bolas[, 'vermelho'], xlab = NULL, ylab = NULL, main = NULL, cex.axis = 0.75)
dev.off()

png('M - histograma (verde).png', width = 500, height = 500, res = 200)
par(mfrow = c(1, 1), mar = c(2, 2, 1, 1))
hist(bolas[, 'verde'], xlab = NULL, ylab = NULL, main = NULL, cex.axis = 0.75)
dev.off()

png('M - histograma (azul).png', width = 500, height = 500, res = 200)
par(mfrow = c(1, 1), mar = c(2, 2, 1, 1))
hist(bolas[, 'azul'], xlab = NULL, ylab = NULL, main = NULL, cex.axis = 0.75)
dev.off()

# Variáveis comparadas dois a dois

png('M - canais de cor dois a dois.png', width = 3000, height = 1000, res = 200)
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))
plot(bolas[, 'vermelho'], bolas[, 'verde'], pch = 21, cex = 2, bg = adjustcolor(bolas_col, alpha.f = 0.5), xlim = c(0, 1), ylim = c(0, 1), xlab = 'vermelho', ylab = 'verde')
plot(bolas[, 'vermelho'], bolas[, 'azul'], pch = 21, cex = 2, bg = adjustcolor(bolas_col, alpha.f = 0.5), xlim = c(0, 1), ylim = c(0, 1), xlab = 'vermelho', ylab = 'azul')
plot(bolas[, 'verde'], bolas[, 'azul'], pch = 21, cex = 2, bg = adjustcolor(bolas_col, alpha.f = 0.5), xlim = c(0, 1), ylim = c(0, 1), xlab = 'verde', ylab = 'azul')
dev.off()

# Plot 3D
png('M - 3D.png', width = 1200, height = 1200, res = 200)
par(mfrow = c(1, 1))
scatter3D_fancy(x = bolas[, 'vermelho'], y = bolas[, 'verde'], z = bolas[, 'azul'], xlab = 'vermelho', ylab = 'verde', zlab = 'azul', xlim = c(0, 1), ylim = c(0, 1), zlim = c(0, 1), pch = 21, bg = adjustcolor(bolas_col, alpha.f = 0.8), col = 'black', ticktype = 'detailed', phi = 0, theta = 45)
dev.off()

# 2.2.2 K-Means & PAM ----

bolas_dist = dist(bolas_var)

# K-Means

K = 1:20
temp <- numeric()
for (k in K){
    temp <-c(temp, sum(kmeans(bolas_var, k, nstart = 100)$withinss))}
png('M - kmeans.png', width = 1500, height = 1000, res = 200)
par(mfrow = c(1, 1), mar = c(5, 5, 2, 1))
plot(K, temp, xaxt = 'n', cex = 2, cex.lab = 1, cex.axis = 1, type="o", xlab = "Número de k", ylab = "Soma de Quadrados")
axis(1, at = K, labels = K, cex.axis = 1)
dev.off()

# PAM

K = 2:10
png('M - PAM.png', width = 1500, height = 1000, res = 200)
par(mar = c(4, 4, 1.5, 1), mfrow = c(ceiling(sqrt(diff(range(K))+1)), ceiling((diff(range(K))+1)/ceiling(sqrt(diff(range(K))+1)))))
for (k in K){
    pam = cluster::pam(bolas_dist, k)
    silinfo = pam$silinfo$widths
    cluster = silinfo[, 1]
    col = rainbow(k)[cluster]
    plot(silinfo[, 3], ylim = c(0, 1), type = 'h', main = paste0('Score da silhueta para k = ', k), ylab = 'Score', col = col)
    abline(h = pam$silinfo$avg.width, lty = 2)
}
dev.off()

pam = cluster::pam(bolas_dist, 3)
bolas_pam = as.factor(pam$clustering)
bolas$pam = bolas_pam
bolas_pam_col = rainbow(3)[bolas_pam]

png('M - heatmap (Lote x PAM).png', width = 1000, height = 900, res = 200)
par(mar = c(0, 0, 0, 0))
table = table(bolas_gro, bolas_pam)/rowSums(table(bolas_gro, bolas_pam))
col = colorRampPalette(c('white', 'black'))(100)[(min(table)*100):(max(table)*100)]
heatmap(table, mar = c(4, 10), cexRow = 3.6, cexCol = 4, col = col, Colv = NA, Rowv = NA, reorderfun = NA, scale = 'none')
legend('bottomright', title = 'Proporção', cex = 1.5, fill = c('white', 'black'), legend = c('0%', '100%'))
dev.off()

legend_pam = data.frame('legend' = as.character(unique(bolas_pam)), 'col' = as.character(unique(bolas_pam_col)), stringsAsFactors = F)
legend_pam = legend_pam[order(legend_pam[, 'legend']), ]

# 2.2.3 Hierarchical clustering ----

bolas_clust = hclust(bolas_dist, method = 'average')

colsidecolors = as.matrix(data.frame('Lote' = bolas_gro_col, 'PAM' = bolas_pam_col))

png('M - hierarchical clustering.png', width = 1000, height = 750, res = 200)
par(mar = c(0, 0, 0, 0), mfrow = c(1, 1))
col = colorRampPalette(c('blue', 'red'))(100)[(as.integer(min(bolas_var)*100)):(as.integer(max(bolas_var)*100))]
heatmapplus(t(as.matrix(bolas_var)), mar = c(0, 11), cexRow = 2, Colv = as.dendrogram(bolas_clust), ColSideColors = colsidecolors, distfun = stats::dist, col = col, scale = 'none')
legend('topright', title = 'Lote | PAM', cex = 0.75, ncol = 3, fill = c(legend_gro[, 'col'], legend_pam[, 'col']), legend = c(legend_gro[, 'legend'], legend_pam[, 'legend']))
legend('bottomright', title = 'Intensidade', cex = 0.75, fill = c('blue', 'red'), legend = c('0%', '100%'))
dev.off()

# 2.2.4 PCA ----

pca = prcomp(bolas_var)
rot = pca$rotation
pca_var = pca$sdev^2
pca = pca$x
pca_var_perc = round(pca_var/sum(pca_var)*100, 2)
pca_var_perc_acc = cumsum(pca_var_perc)

par(mfrow = c(1, 2), mar = c(5, 5, 4, 1))

# Sem discriminar as amostras

png('M - PCA, sem_discriminar.png', width = 2000, height = 1000, res = 200)
par(mfrow = c(1, 2), mar = c(4, 4, 2.5, 2.5))

lims = c(min(pca[, 1], pca[, 2]), max(pca[, 1], pca[, 2]))
plot(pca[, 1], pca[, 2], xlim = lims, ylim = lims, pch = 21, cex = 1.5, main = 'PCA', xlab = paste0('PC1 | ', pca_var_perc[1], '% | ', pca_var_perc_acc[1], '%'), ylab = paste0('PC2 | ', pca_var_perc[2], '% | ', pca_var_perc_acc[2], '%'))

olm_coefs <- olm_two_axis(pca[, 1], pca[, 2])
lines_two_axis(olm_coefs, lty = 2)

lims = c(min(pca[, 2], pca[, 3]), max(pca[, 2], pca[, 3]))
plot(pca[, 2], pca[, 3], xlim = lims, ylim = lims, pch = 21, cex = 1.5, main = 'PCA', xlab = paste0('PC2 | ', pca_var_perc[2], '% | ', pca_var_perc_acc[2], '%'), ylab = paste0('PC3 | ', pca_var_perc[3], '% | ', pca_var_perc_acc[3], '%'))

olm_coefs <- olm_two_axis(pca[, 2], pca[, 3])
lines_two_axis(olm_coefs, lty = 2)

text(rev(lims)[1], rev(lims)[2], cex = 0.75, labels = paste0(capture.output(rot), collapse = '\n'), adj = c(1, 0), family = 'mono')

dev.off()

# Cada amostra tem sua cor final

png('M - PCA, cor_final.png', width = 2000, height = 1000, res = 200)
par(mfrow = c(1, 2), mar = c(4, 4, 2.5, 2.5))

lims = c(min(pca[, 1], pca[, 2]), max(pca[, 1], pca[, 2]))
plot(pca[, 1], pca[, 2], xlim = lims, ylim = lims, pch = 21, bg = adjustcolor(bolas_col, alpha.f = 0.5), cex = 1.5, main = 'PCA', xlab = paste0('PC1 | ', pca_var_perc[1], '% | ', pca_var_perc_acc[1], '%'), ylab = paste0('PC2 | ', pca_var_perc[2], '% | ', pca_var_perc_acc[2], '%'))
olm_coefs <- olm_two_axis(pca[, 1], pca[, 2])
lines_two_axis(olm_coefs, lty = 2)

lims = c(min(pca[, 2], pca[, 3]), max(pca[, 2], pca[, 3]))
plot(pca[, 2], pca[, 3], xlim = lims, ylim = lims, pch = 21, bg = adjustcolor(bolas_col, alpha.f = 0.5), cex = 1.5, main = 'PCA', xlab = paste0('PC2 | ', pca_var_perc[2], '% | ', pca_var_perc_acc[2], '%'), ylab = paste0('PC3 | ', pca_var_perc[3], '% | ', pca_var_perc_acc[3], '%'))
olm_coefs <- olm_two_axis(pca[, 2], pca[, 3])
lines_two_axis(olm_coefs, lty = 2)

dev.off()

# Cada lote é identificado

png('M - PCA, por lote.png', width = 2000, height = 1000, res = 200)
par(mfrow = c(1, 2), mar = c(4, 4, 2.5, 2.5))

lims = c(min(pca[, 1], pca[, 2]), max(pca[, 1], pca[, 2]))
plot(pca[, 1], pca[, 2], xlim = lims, ylim = lims, pch = c(21:25, 21)[bolas_gro], col = c(rep('black', 5), 'gray')[bolas_gro], bg = adjustcolor(bolas_col, alpha.f = 0.5), cex = 1.5, main = 'PCA', xlab = paste0('PC1 | ', pca_var_perc[1], '% | ', pca_var_perc_acc[1], '%'), ylab = paste0('PC2 | ', pca_var_perc[2], '% | ', pca_var_perc_acc[2], '%'))
legend('bottomleft', title = 'Lotes', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:25, 21), col = c(rep('black', 5), 'gray'), cex = 1, pt.cex = 1.5, legend = levels(bolas_gro), bty = 'n')
olm_coefs <- olm_two_axis(pca[, 1], pca[, 2])
lines_two_axis(olm_coefs, lty = 2)

lims = c(min(pca[, 2], pca[, 3]), max(pca[, 2], pca[, 3]))
plot(pca[, 2], pca[, 3], xlim = lims, ylim = lims, pch = c(21:25, 21)[bolas_gro], col = c(rep('black', 5), 'gray')[bolas_gro], bg = adjustcolor(bolas_col, alpha.f = 0.5), cex = 1.5, main = 'PCA', xlab = paste0('PC2 | ', pca_var_perc[2], '% | ', pca_var_perc_acc[2], '%'), ylab = paste0('PC3 | ', pca_var_perc[3], '% | ', pca_var_perc_acc[3], '%'))
legend('bottomleft', title = 'Lotes', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:25, 21), col = c(rep('black', 5), 'gray'), cex = 1, pt.cex = 1.5, legend = levels(bolas_gro), bty = 'n')
olm_coefs <- olm_two_axis(pca[, 2], pca[, 3])
lines_two_axis(olm_coefs, lty = 2)

dev.off()

# Cada grupo PAM é identificado

png('M - PCA, por PAM.png', width = 2000, height = 1000, res = 200)
par(mfrow = c(1, 2), mar = c(4, 4, 2.5, 2.5))

lims = c(min(pca[, 1], pca[, 2]), max(pca[, 1], pca[, 2]))
plot(pca[, 1], pca[, 2], xlim = lims, ylim = lims, pch = c(21:23)[bolas_pam], bg = adjustcolor(bolas_col, alpha.f = 0.5), cex = 1.5, main = 'PCA', xlab = paste0('PC1 | ', pca_var_perc[1], '% | ', pca_var_perc_acc[1], '%'), ylab = paste0('PC2 | ', pca_var_perc[2], '% | ', pca_var_perc_acc[2], '%'))
legend('bottomleft', title = 'PAM', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:23), cex = 1, pt.cex = 1.5, legend = levels(bolas_pam), bty = 'n')
olm_coefs <- olm_two_axis(pca[, 1], pca[, 2])
lines_two_axis(olm_coefs, lty = 2)

lims = c(min(pca[, 2], pca[, 3]), max(pca[, 2], pca[, 3]))
plot(pca[, 2], pca[, 3], xlim = lims, ylim = lims, pch = c(21:23)[bolas_pam], bg = adjustcolor(bolas_col, alpha.f = 0.5), cex = 1.5, main = 'PCA', xlab = paste0('PC2 | ', pca_var_perc[2], '% | ', pca_var_perc_acc[2], '%'), ylab = paste0('PC3 | ', pca_var_perc[3], '% | ', pca_var_perc_acc[3], '%'))
legend('bottomleft', title = 'PAM', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:23), cex = 1, pt.cex = 1.5, legend = levels(bolas_pam), bty = 'n')
olm_coefs <- olm_two_axis(pca[, 2], pca[, 3])
lines_two_axis(olm_coefs, lty = 2)

dev.off()

# 2.2.5 PCOA (distância euclidiana)----

pcoa = ape::pcoa(bolas_dist)
pcoa_var_perc_acc = round(pcoa$values$Cumul_eig*100, 2)
pcoa = pcoa$vectors
pcoa_var_perc = pcoa_var_perc_acc[1]
for(dim in 3:2){
    pcoa_var_perc[dim] = pcoa_var_perc_acc[dim] - pcoa_var_perc_acc[dim-1]     
}

# 2.2.6 NMDS (distância euclidiana)----

nmds = vegan::metaMDS(comm = bolas_dist, k = 3)
nmds = postMDS2(nmds, bolas_dist)
nmds_var = attr(nmds$points, 'sdev')^2
nmds_var_perc = round(nmds_var/sum(nmds_var)*100, 2)
nmds_var_perc_acc = cumsum(nmds_var_perc)
nmds = nmds$points

# Plotando PCA, PCOA & NMDS (distância euclidiana)

png('M - PCA, PCOA & NMDS (distância euclidiana).png', width = 3000, height = 2000, res = 200)
par(mfrow = c(2, 3), mar = c(4.25, 4.25, 2.5, 1))

lims = c(min(pca[, 1], pca[, 2]), max(pca[, 1], pca[, 2]))
plot(pca[, 1], pca[, 2], xlim = lims, ylim = lims, pch = c(21:25, 21)[bolas_gro], col = c(rep('black', 5), 'gray')[bolas_gro], bg = adjustcolor(bolas_col, alpha.f = 0.5), cex = 2, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, main = 'PCA', xlab = paste0('PC1 | ', pca_var_perc[1], '% | ', pca_var_perc_acc[1], '%'), ylab = paste0('PC2 | ', pca_var_perc[2], '% | ', pca_var_perc_acc[2], '%'))
legend('bottomleft', title = 'Lotes', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:25, 21), col = c(rep('black', 5), 'gray'), cex = 1.25, pt.cex = 2.25, legend = levels(bolas_gro), bty = 'n')
olm_coefs <- olm_two_axis(pca[, 1], pca[, 2])
lines_two_axis(olm_coefs, lty = 2)

lims = c(min(pcoa[, 1], pcoa[, 2]), max(pcoa[, 1], pcoa[, 2]))
plot(pcoa[, 1], pcoa[, 2], xlim = lims, ylim = lims, pch = c(21:25, 21)[bolas_gro], col = c(rep('black', 5), 'gray')[bolas_gro], bg = adjustcolor(bolas_col, alpha.f = 0.5), cex = 2, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, main = 'PCOA', xlab = paste0('PCO1 | ', pcoa_var_perc[1], '% | ', pcoa_var_perc_acc[1], '%'), ylab = paste0('PCO2 | ', pcoa_var_perc[2], '% | ', pcoa_var_perc_acc[2], '%'))
legend('bottomleft', title = 'Lotes', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:25, 21), col = c(rep('black', 5), 'gray'), cex = 1.25, pt.cex = 2.25, legend = levels(bolas_gro), bty = 'n')
olm_coefs <- olm_two_axis(pcoa[, 1], pcoa[, 2])
lines_two_axis(olm_coefs, lty = 2)

lims = c(min(nmds[, 1], nmds[, 2]), max(nmds[, 1], nmds[, 2]))
plot(nmds[, 1], nmds[, 2], xlim = lims, ylim = lims, pch = c(21:25, 21)[bolas_gro], col = c(rep('black', 5), 'gray')[bolas_gro], bg = adjustcolor(bolas_col, alpha.f = 0.5), cex = 2, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, main = 'NMDS', xlab = paste0('NMDS1 | ', nmds_var_perc[1], '% | ', nmds_var_perc_acc[1], '%'), ylab = paste0('NMDS2 | ', nmds_var_perc[2], '% | ', nmds_var_perc_acc[2], '%'))
legend('bottomleft', title = 'Lotes', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:25, 21), col = c(rep('black', 5), 'gray'), cex = 1.25, pt.cex = 2.25, legend = levels(bolas_gro), bty = 'n')
olm_coefs <- olm_two_axis(nmds[, 1], nmds[, 2])
lines_two_axis(olm_coefs, lty = 2)

lims = c(min(pca[, 2], pca[, 3]), max(pca[, 2], pca[, 3]))
plot(pca[, 2], pca[, 3], xlim = lims, ylim = lims, pch = c(21:25, 21)[bolas_gro], col = c(rep('black', 5), 'gray')[bolas_gro], bg = adjustcolor(bolas_col, alpha.f = 0.5), cex = 2, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, main = 'PCA', xlab = paste0('PC2 | ', pca_var_perc[2], '% | ', pca_var_perc_acc[2], '%'), ylab = paste0('PC3 | ', pca_var_perc[3], '% | ', pca_var_perc_acc[3], '%'))
legend('bottomleft', title = 'Lotes', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:25, 21), col = c(rep('black', 5), 'gray'), cex = 1.25, pt.cex = 2.25, legend = levels(bolas_gro), bty = 'n')
olm_coefs <- olm_two_axis(pca[, 2], pca[, 3])
lines_two_axis(olm_coefs, lty = 2)

lims = c(min(pcoa[, 2], pcoa[, 3]), max(pcoa[, 2], pcoa[, 3]))
plot(pcoa[, 2], pcoa[, 3], xlim = lims, ylim = lims, pch = c(21:25, 21)[bolas_gro], col = c(rep('black', 5), 'gray')[bolas_gro], bg = adjustcolor(bolas_col, alpha.f = 0.5), cex = 2, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, main = 'PCOA', xlab = paste0('PCO2 | ', pcoa_var_perc[2], '% | ', pcoa_var_perc_acc[2], '%'), ylab = paste0('PCO3 | ', pcoa_var_perc[3], '% | ', pcoa_var_perc_acc[3], '%'))
legend('bottomleft', title = 'Lotes', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:25, 21), col = c(rep('black', 5), 'gray'), cex = 1.25, pt.cex = 2.25, legend = levels(bolas_gro), bty = 'n')
olm_coefs <- olm_two_axis(pcoa[, 2], pcoa[, 3])
lines_two_axis(olm_coefs, lty = 2)

lims = c(min(nmds[, 2], nmds[, 3]), max(nmds[, 2], nmds[, 3]))
plot(nmds[, 2], nmds[, 3], xlim = lims, ylim = lims, pch = c(21:25, 21)[bolas_gro], col = c(rep('black', 5), 'gray')[bolas_gro], bg = adjustcolor(bolas_col, alpha.f = 0.5), cex = 2, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, main = 'NMDS', xlab = paste0('NMDS2 | ', nmds_var_perc[2], '% | ', nmds_var_perc_acc[2], '%'), ylab = paste0('NMDS3 | ', nmds_var_perc[3], '% | ', nmds_var_perc_acc[3], '%'))
legend('bottomleft', title = 'Lotes', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:25, 21), col = c(rep('black', 5), 'gray'), cex = 1.25, pt.cex = 2.25, legend = levels(bolas_gro), bty = 'n')
olm_coefs <- olm_two_axis(nmds[, 2], nmds[, 3])
lines_two_axis(olm_coefs, lty = 2)

dev.off()

# 2.2.7 PCOA (distância personalizada)----

bolas_dist = dist_bolas(bolas_var)

pcoa = ape::pcoa(bolas_dist)
pcoa_var_perc_acc = round(pcoa$values$Cumul_eig*100, 2)
pcoa = pcoa$vectors
pcoa_var_perc = pcoa_var_perc_acc[1]
for(dim in 3:2){
    pcoa_var_perc[dim] = pcoa_var_perc_acc[dim] - pcoa_var_perc_acc[dim-1]     
}

# 2.2.6 NMDS (distância personalizada)----

nmds = vegan::metaMDS(comm = bolas_dist, k = 3)
nmds = postMDS2(nmds, bolas_dist)
nmds_var = attr(nmds$points, 'sdev')^2
nmds_var_perc = round(nmds_var/sum(nmds_var)*100, 2)
nmds_var_perc_acc = cumsum(nmds_var_perc)
nmds = nmds$points

# Plotando PCA, PCOA & NMDS

png('M - PCA, PCOA & NMDS (distância personalizada).png', width = 3000, height = 2000, res = 200)
par(mfrow = c(2, 3), mar = c(4.25, 4.25, 2.5, 1))

lims = c(min(pca[, 1], pca[, 2]), max(pca[, 1], pca[, 2]))
plot(pca[, 1], pca[, 2], xlim = lims, ylim = lims, pch = c(21:25, 21)[bolas_gro], col = c(rep('black', 5), 'gray')[bolas_gro], bg = adjustcolor(bolas_col, alpha.f = 0.5), cex = 1.5, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, main = 'PCA', xlab = paste0('PC1 | ', pca_var_perc[1], '% | ', pca_var_perc_acc[1], '%'), ylab = paste0('PC2 | ', pca_var_perc[2], '% | ', pca_var_perc_acc[2], '%'))
legend('bottomleft', title = 'Lotes', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:25, 21), col = c(rep('black', 5), 'gray'), cex = 1.25, pt.cex = 2.25, legend = levels(bolas_gro), bty = 'n')
olm_coefs <- olm_two_axis(pca[, 1], pca[, 2])
lines_two_axis(olm_coefs, lty = 2)

lims = c(min(pcoa[, 1], pcoa[, 2]), max(pcoa[, 1], pcoa[, 2]))
plot(pcoa[, 1], pcoa[, 2], xlim = lims, ylim = lims, pch = c(21:25, 21)[bolas_gro], col = c(rep('black', 5), 'gray')[bolas_gro], bg = adjustcolor(bolas_col, alpha.f = 0.5), cex = 1.5, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, main = 'PCOA', xlab = paste0('PCO1 | ', pcoa_var_perc[1], '% | ', pcoa_var_perc_acc[1], '%'), ylab = paste0('PCO2 | ', pcoa_var_perc[2], '% | ', pcoa_var_perc_acc[2], '%'))
legend('bottomleft', title = 'Lotes', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:25, 21), col = c(rep('black', 5), 'gray'), cex = 1.25, pt.cex = 2.25, legend = levels(bolas_gro), bty = 'n')
olm_coefs <- olm_two_axis(pcoa[, 1], pcoa[, 2])
lines_two_axis(olm_coefs, lty = 2)

lims = c(min(nmds[, 1], nmds[, 2]), max(nmds[, 1], nmds[, 2]))
plot(nmds[, 1], nmds[, 2], xlim = lims, ylim = lims, pch = c(21:25, 21)[bolas_gro], col = c(rep('black', 5), 'gray')[bolas_gro], bg = adjustcolor(bolas_col, alpha.f = 0.5), cex = 1.5, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, main = 'NMDS', xlab = paste0('NMDS1 | ', nmds_var_perc[1], '% | ', nmds_var_perc_acc[1], '%'), ylab = paste0('NMDS2 | ', nmds_var_perc[2], '% | ', nmds_var_perc_acc[2], '%'))
legend('bottomleft', title = 'Lotes', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:25, 21), col = c(rep('black', 5), 'gray'), cex = 1.25, pt.cex = 2.25, legend = levels(bolas_gro), bty = 'n')
olm_coefs <- olm_two_axis(nmds[, 1], nmds[, 2])
lines_two_axis(olm_coefs, lty = 2)

lims = c(min(pca[, 2], pca[, 3]), max(pca[, 2], pca[, 3]))
plot(pca[, 2], pca[, 3], xlim = lims, ylim = lims, pch = c(21:25, 21)[bolas_gro], col = c(rep('black', 5), 'gray')[bolas_gro], bg = adjustcolor(bolas_col, alpha.f = 0.5), cex = 1.5, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, main = 'PCA', xlab = paste0('PC2 | ', pca_var_perc[2], '% | ', pca_var_perc_acc[2], '%'), ylab = paste0('PC3 | ', pca_var_perc[3], '% | ', pca_var_perc_acc[3], '%'))
legend('bottomleft', title = 'Lotes', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:25, 21), col = c(rep('black', 5), 'gray'), cex = 1.25, pt.cex = 2.25, legend = levels(bolas_gro), bty = 'n')
olm_coefs <- olm_two_axis(pca[, 2], pca[, 3])
lines_two_axis(olm_coefs, lty = 2)

lims = c(min(pcoa[, 1], pcoa[, 2]), max(pcoa[, 1], pcoa[, 2]))
plot(pcoa[, 1], pcoa[, 2], xlim = lims, ylim = lims, pch = c(21:25, 21)[bolas_gro], col = c(rep('black', 5), 'gray')[bolas_gro], bg = adjustcolor(rgb(0, bolas_var[, 'verde'], bolas_var[, 'azul']), alpha.f = 0.5), cex = 1.5, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, main = 'PCOA', xlab = paste0('PCO1 | ', pcoa_var_perc[1], '% | ', pcoa_var_perc_acc[1], '%'), ylab = paste0('PCO2 | ', pcoa_var_perc[2], '% | ', pcoa_var_perc_acc[2], '%'))
legend('bottomleft', title = 'Lotes', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:25, 21), col = c(rep('black', 5), 'gray'), cex = 1.25, pt.cex = 2.25, legend = levels(bolas_gro), bty = 'n')
olm_coefs <- olm_two_axis(pcoa[, 1], pcoa[, 2])
lines_two_axis(olm_coefs, lty = 2)

lims = c(min(nmds[, 2], nmds[, 3]), max(nmds[, 2], nmds[, 3]))
plot(nmds[, 2], nmds[, 3], xlim = lims, ylim = lims, pch = c(21:25, 21)[bolas_gro], col = c(rep('black', 5), 'gray')[bolas_gro], bg = adjustcolor(bolas_col, alpha.f = 0.5), cex = 1.5, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, main = 'NMDS', xlab = paste0('NMDS2 | ', nmds_var_perc[2], '% | ', nmds_var_perc_acc[2], '%'), ylab = paste0('NMDS3 | ', nmds_var_perc[3], '% | ', nmds_var_perc_acc[3], '%'))
legend('bottomleft', title = 'Lotes', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:25, 21), col = c(rep('black', 5), 'gray'), cex = 1.25, pt.cex = 2.25, legend = levels(bolas_gro), bty = 'n')
olm_coefs <- olm_two_axis(nmds[, 2], nmds[, 3])
lines_two_axis(olm_coefs, lty = 2)

dev.off()

# 2.3 Discriminação ----

# 2.3.1 CDA ----

lm <- lm(as.matrix(bolas_var) ~ lote, bolas)
cda <- candisc::candisc(lm, data = bolas)

png('M - CDA (lotes).png', width = 1000, height = 1000, res = 200)
par(mfrow = c(1, 1), mar = c(4, 4, 2.5, 2.5))
plot(cda, main = 'CDA', pch = 1:6, col = rainbow(6), var.col = 'purple', scale = 6, conf = 0, cex = 1.5, prefix = "CAN")
dev.off()

lm <- lm(as.matrix(bolas_var) ~ pam, bolas)
cda <- candisc::candisc(lm, data = bolas)

png('M - CDA (PAM).png', width = 1000, height = 1000, res = 200)
par(mfrow = c(1, 1), mar = c(4, 4, 2.5, 2.5))
plot(cda, main = 'CDA', pch = 1:3, col = c('red', 'blue', 'green'), var.col = 'purple', scale = 6, conf = 0, cex = 1.5, prefix = "CAN")
dev.off()

# 2.3.2 Árvores de decisão ----

tree = tree::tree(lote ~ vermelho+verde+azul, bolas)

png('M - arvore de decisao, lote.png', width = 1000, height = 1500, res = 200)
par(mfrow = c(1, 1))
plot(tree)
text(tree, cex = 1.5)
dev.off()

tree = tree::tree(pam ~ vermelho+verde+azul, bolas)

png('M - arvore de decisao, PAM.png', width = 1000, height = 1500, res = 200)
par(mfrow = c(1, 1))
plot(tree)
text(tree, cex = 1.5)
dev.off()

# 2.4 Interpretação ----

# 2.4.1 RDA ----

rda = vegan::rda(formula = bolas_var ~ fabricante + preço + preferência, data = bolas)
for(scaling in 1:2){
    for(i in 1:2){
        choices = if(i == 1){c(1, 2)} else {c(3, 2)}
        plot_info = plot(rda, scaling = scaling, choices = choices)
        multiplier = if(i == 1){attr(plot_info$biplot, 'arrow.mul')} else {multiplier}
        summ = summary(rda)
        species = plot_info$species
        sites = plot_info$sites
        centroids = plot_info$centroids
        biplot = plot_info$biplot * multiplier
        
        comp = round(c(summ$cont$importance[2, i], summ$cont$importance[2, i+1])*100, 1)
        comp_acc = round(c(summ$cont$importance[3, i], summ$cont$importance[3, i+1])*100, 1)
        cons = round(c(summ$concont$importance[2, i], summ$concont$importance[2, i+1])*100, 1)
        cons_acc = round(c(summ$concont$importance[3, i], summ$concont$importance[3, i+1])*100, 1)
        
        xlim = c(min(species[, 1], sites[, 1], centroids[, 1], biplot[, 1]), max(species[, 1], sites[, 1], centroids[, 1], biplot[, 1])) * 1.1
        ylim = c(min(species[, 2], sites[, 2], centroids[, 2], biplot[, 2]), max(species[, 2], sites[, 2], centroids[, 2], biplot[, 2])) * 1.1
        
        png(paste0('M - RDA (scaling_', scaling, ') dims_', i, 'vs', i+1, '.png'), width = 1000, height = 1000, res = 200)
        par(mfrow = c(1, 1), mar = c(4, 4, 2.5, 2.5))
        j = ifelse(i == 1, 1, 2)
        k = ifelse(i == 2, 1, 2)
        plot(sites[, 1], sites[, 2], xlim = xlim, ylim = ylim, pch = 21, cex = 2, bg = bolas_col,
             xlab = paste0('RDA', choices[1], ' (', comp[j], '% | ', comp_acc[j], '% || ', cons[j], '% | ', cons_acc[j], '%)'), 
             ylab = paste0('RDA', choices[2], ' (', comp[k], '% | ', comp_acc[k], '% || ', cons[k], '% | ', cons_acc[k], '%)'))
        abline(h = 0, v = 0, lty = 2)
        points(species[, 1], species[, 2], cex = 2, pch = 4, lwd = 2, col = 'gray')
        text(species[, 1], species[, 2], cex = 1, pos = 3, offset = 1.5, labels = rownames(species), col = 'gray')
        points(centroids[, 1], centroids[, 2], cex = 2, pch = 4, lwd = 2, col = 'white')
        text(centroids[, 1], centroids[, 2], cex = 1, pos = 3, offset = 1.5, labels = rownames(centroids))
        arrows(0, 0, biplot[, 1], biplot[, 2], lwd = 1.5, length = 0.1)
        text(biplot[, 1], biplot[, 2], cex = 1, pos = 3, offset = 1.5, labels = rownames(biplot))
        dev.off()
    }
}

# Colorindo pelo gradiente de preço

preço = bolas$preço
preço = preço/max(preço)
preço = preço*100
preço = round(preço)
preço_col = colorRampPalette(c('white', 'black'))(100)[preço]

for(i in 1:2){
    choices = if(i == 1){c(1, 2)} else {c(3, 2)}
    plot_info = plot(rda, scaling = 2, choices = choices)
    multiplier = if(i == 1){attr(plot_info$biplot, 'arrow.mul')} else {multiplier}
    summ = summary(rda)
    species = plot_info$species
    sites = plot_info$sites
    centroids = plot_info$centroids
    biplot = plot_info$biplot * multiplier
    
    comp = round(c(summ$cont$importance[2, i], summ$cont$importance[2, i+1])*100, 1)
    comp_acc = round(c(summ$cont$importance[3, i], summ$cont$importance[3, i+1])*100, 1)
    cons = round(c(summ$concont$importance[2, i], summ$concont$importance[2, i+1])*100, 1)
    cons_acc = round(c(summ$concont$importance[3, i], summ$concont$importance[3, i+1])*100, 1)
    
    xlim = c(min(species[, 1], sites[, 1], centroids[, 1], biplot[, 1]), max(species[, 1], sites[, 1], centroids[, 1], biplot[, 1])) * 1.1
    ylim = c(min(species[, 2], sites[, 2], centroids[, 2], biplot[, 2]), max(species[, 2], sites[, 2], centroids[, 2], biplot[, 2])) * 1.1
    
    png(paste0('M - RDA (gradiente de preço) dims_', i, 'vs', i+1, '.png'), width = 1000, height = 1000, res = 200)
    par(mfrow = c(1, 1), mar = c(4, 4, 2.5, 2.5))
    j = ifelse(i == 1, 1, 2)
    k = ifelse(i == 2, 1, 2)
    plot(sites[, 1], sites[, 2], xlim = xlim, ylim = ylim, pch = 21, cex = 0.75, bg = preço_col,
         xlab = paste0('RDA', choices[1], ' (', comp[j], '% | ', comp_acc[j], '% || ', cons[j], '% | ', cons_acc[j], '%)'), 
         ylab = paste0('RDA', choices[2], ' (', comp[k], '% | ', comp_acc[k], '% || ', cons[k], '% | ', cons_acc[k], '%)'))
    abline(h = 0, v = 0, lty = 2)
    points(species[, 1], species[, 2], cex = 2, pch = 4, lwd = 2, col = 'gray')
    text(species[, 1], species[, 2], cex = 1, pos = 3, offset = 1.5, labels = rownames(species), col = 'gray')
    arrows(0, 0, biplot[, 1], biplot[, 2], lwd = 1.5, length = 0.1)
    text(biplot[, 1], biplot[, 2], cex = 1, pos = 3, offset = 1.5, labels = rownames(biplot))
    dev.off()
}

# Colorindo pelo gradiente de preferência

preferência = bolas$preferência
preferência = preferência/max(preferência)
preferência = preferência*100
preferência = round(preferência)
preferência_col = colorRampPalette(c('white', 'black'))(100)[preferência]

for(i in 1:2){
    choices = if(i == 1){c(1, 2)} else {c(3, 2)}
    plot_info = plot(rda, scaling = 2, choices = choices)
    multiplier = if(i == 1){attr(plot_info$biplot, 'arrow.mul')} else {multiplier}
    summ = summary(rda)
    species = plot_info$species
    sites = plot_info$sites
    centroids = plot_info$centroids
    biplot = plot_info$biplot * multiplier
    
    comp = round(c(summ$cont$importance[2, i], summ$cont$importance[2, i+1])*100, 1)
    comp_acc = round(c(summ$cont$importance[3, i], summ$cont$importance[3, i+1])*100, 1)
    cons = round(c(summ$concont$importance[2, i], summ$concont$importance[2, i+1])*100, 1)
    cons_acc = round(c(summ$concont$importance[3, i], summ$concont$importance[3, i+1])*100, 1)
    
    xlim = c(min(species[, 1], sites[, 1], centroids[, 1], biplot[, 1]), max(species[, 1], sites[, 1], centroids[, 1], biplot[, 1])) * 1.1
    ylim = c(min(species[, 2], sites[, 2], centroids[, 2], biplot[, 2]), max(species[, 2], sites[, 2], centroids[, 2], biplot[, 2])) * 1.1
    
    png(paste0('M - RDA (gradiente de preferência) dims_', i, 'vs', i+1, '.png'), width = 1000, height = 1000, res = 200)
    par(mfrow = c(1, 1), mar = c(4, 4, 2.5, 2.5))
    j = ifelse(i == 1, 1, 2)
    k = ifelse(i == 2, 1, 2)
    plot(sites[, 1], sites[, 2], xlim = xlim, ylim = ylim, pch = 21, cex = 0.75, bg = preferência_col,
         xlab = paste0('RDA', choices[1], ' (', comp[j], '% | ', comp_acc[j], '% || ', cons[j], '% | ', cons_acc[j], '%)'), 
         ylab = paste0('RDA', choices[2], ' (', comp[k], '% | ', comp_acc[k], '% || ', cons[k], '% | ', cons_acc[k], '%)'))
    abline(h = 0, v = 0, lty = 2)
    points(species[, 1], species[, 2], cex = 2, pch = 4, lwd = 2, col = 'gray')
    text(species[, 1], species[, 2], cex = 1, pos = 3, offset = 1.5, labels = rownames(species), col = 'gray')
    arrows(0, 0, biplot[, 1], biplot[, 2], lwd = 1.5, length = 0.1)
    text(biplot[, 1], biplot[, 2], cex = 1, pos = 3, offset = 1.5, labels = rownames(biplot))
    dev.off()
}

# 3 DATASET GRANDE: Futebol ----

# 3.1 Definição ----

# Lendo o arquivo contendo os dados
futebol = read.csv('data.csv')

# Selecionando uma amostra de 500 jogadores, e filtrando para as colunas de interesse com os dados técnicos dos jogadores
set.seed(1)
futebol = futebol[sample(1:nrow(futebol), 500), c(4, 22, 27, 28, 55:83)]

# Removendo jogadores com dados faltantes
apply(futebol, 2, function(i) sum(is.na(i)))
futebol = futebol[!is.na(futebol[, 'Crossing']), ]

# Convertendo altura e peso para medidas do sistema métrico
futebol[, 'Height'] = sapply(as.character(futebol[, 'Height']), function(x){
    x = as.numeric(unlist(strsplit(x, '\'')))
    x = 30.48*x[1] + 2.54*x[2]
    x})
futebol[, 'Weight'] = sapply(as.character(futebol[, 'Height']), function(x){
    x = as.character(unlist(strsplit(x, 'b')))
    x = 0.453*as.numeric(x[1])
    x
})

# Grupos
futebol_gro = futebol[, 'Position']
futebol_gro = droplevels(futebol_gro)

# Variáveis correlatas
futebol_cor = futebol[, c('Position', 'Age', 'Height', 'Weight')]

# Variáveis que definem os jogadores
futebol_var = futebol[, 5:ncol(futebol)]

# Cores para os plots
futebol_gro_col = rainbow(length(levels(futebol_gro)))[futebol_gro]

legend_gro = data.frame('legend' = as.character(unique(futebol_gro)), 'col' = as.character(unique(futebol_gro_col)), stringsAsFactors = F)

# 3.2 Exploração ----

# 3.2.1 Visualização ----

# Boxplot
png('G - boxplot.png', width = 1000, height = 1000, res = 200)
par(mfrow = c(1, 1), mar = c(8, 3, 1, 1))
boxplot(futebol_var, las = 2)
dev.off()

# 3.2.2 K-Means & PAM ----

futebol_dist = dist(futebol_var)

# K-Means

K = 1:20
temp <- numeric()
for (k in K){
    temp <-c(temp, sum(kmeans(futebol_var, k, nstart = 100)$withinss))}
png('G - kmeans.png', width = 1500, height = 1000, res = 200)
par(mfrow = c(1, 1), mar = c(5, 5, 2, 1))
plot(K, temp, xaxt = 'n', cex = 2, cex.lab = 1, cex.axis = 1, type="o", xlab = "Número de k", ylab = "Soma de Quadrados")
axis(1, at = K, labels = K, cex.axis = 1)
dev.off()

# PAM

K = 2:10
png('G - PAM.png', width = 1500, height = 1000, res = 200)
par(mar = c(4, 4, 1.5, 1), mfrow = c(ceiling(sqrt(diff(range(K))+1)), ceiling((diff(range(K))+1)/ceiling(sqrt(diff(range(K))+1)))))
for (k in K){
    pam = cluster::pam(futebol_dist, k)
    silinfo = pam$silinfo$widths
    cluster = silinfo[, 1]
    col = rainbow(k)[cluster]
    plot(silinfo[, 3], ylim = c(0, 1), type = 'h', main = paste0('Score da silhueta para k = ', k), ylab = 'Score', col = col)
    abline(h = pam$silinfo$avg.width, lty = 2)
}
dev.off()

pam = cluster::pam(futebol_dist, 4)
futebol_pam = as.factor(pam$clustering)
futebol$pam = futebol_pam
futebol_pam_col = rainbow(4)[futebol_pam]

png('G - heatmap (Posição x PAM).png', width = 1000, height = 1500, res = 200)
par(mar = c(0, 0, 0, 0))
table = table(futebol_gro, futebol_pam)/rowSums(table(futebol_gro, futebol_pam))
col = colorRampPalette(c('white', 'black'))(100)[(min(table)*100):(max(table)*100)]
heatmap(table, mar = c(4, 10), cexRow = 1.5, cexCol = 4, col = col, Colv = NA, Rowv = NA, reorderfun = NA, scale = 'none')
legend('bottomright', title = 'Proporção', cex = 1.5, fill = c('white', 'black'), legend = c('0%', '100%'))
dev.off()

legend_pam = data.frame('legend' = as.character(unique(futebol_pam)), 'col' = as.character(unique(futebol_pam_col)), stringsAsFactors = F)
legend_pam = legend_pam[order(legend_pam[, 'legend']), ]

# 3.2.3 Hierarchical clustering ----

futebol_clust = hclust(futebol_dist, method = 'average')

colsidecolors = as.matrix(data.frame('Posição' = futebol_gro_col, 'PAM' = futebol_pam_col))

png('G - hierarchical clustering.png', width = 3000, height = 1500, res = 200)
par(mar = c(0, 0, 0, 0), mfrow = c(1, 1))
col = colorRampPalette(c('blue', 'red'))(100)[(as.integer(min(futebol_var))):(as.integer(max(futebol_var)))]
heatmapplus(t(as.matrix(futebol_var)), mar = c(0, 25), cexRow = 1.5, Colv = as.dendrogram(futebol_clust), ColSideColors = colsidecolors, distfun = stats::dist, col = col, scale = 'none')
legend('topright', title = 'Posição | PAM', cex = 0.75, ncol = 5, fill = c(legend_gro[, 'col'], legend_pam[, 'col']), legend = c(legend_gro[, 'legend'], legend_pam[, 'legend']))
legend('bottomright', title = 'Intensidade', cex = 0.75, fill = c('blue', 'red'), legend = c('0%', '100%'))
dev.off()

# 3.2.4 PCA ----

pca = prcomp(futebol_var)
rot = pca$rotation
pca_var = pca$sdev^2
pca = pca$x
pca_var_perc = round(pca_var/sum(pca_var)*100, 2)
pca_var_perc_acc = cumsum(pca_var_perc)

png('G - PCA.png', width = 2000, height = 1000, res = 200)
par(mfrow = c(1, 2), mar = c(4, 4, 2.5, 2.5))

lims = c(min(pca[, 1], pca[, 2]), max(pca[, 1], pca[, 2]))
plot(pca[, 1], pca[, 2], xlim = lims, ylim = lims, pch = c(21:24)[futebol_pam], bg = adjustcolor(futebol_gro_col, alpha.f = 0.5), cex = 1.5, main = 'PCA', xlab = paste0('PC1 | ', pca_var_perc[1], '% | ', pca_var_perc_acc[1], '%'), ylab = paste0('PC2 | ', pca_var_perc[2], '% | ', pca_var_perc_acc[2], '%'))
legend('bottomleft', title = 'PAM', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:24), cex = 1, pt.cex = 1.5, legend = levels(futebol_pam), bty = 'n')

olm_coefs <- olm_two_axis(pca[, 1], pca[, 2])
lines_two_axis(olm_coefs, lty = 2)

lims = c(min(pca[, 2], pca[, 3]), max(pca[, 2], pca[, 3]))
plot(pca[, 2], pca[, 3], xlim = lims, ylim = lims, pch = c(21:24)[futebol_pam], bg = adjustcolor(futebol_gro_col, alpha.f = 0.5), cex = 1.5, main = 'PCA', xlab = paste0('PC2 | ', pca_var_perc[2], '% | ', pca_var_perc_acc[2], '%'), ylab = paste0('PC3 | ', pca_var_perc[3], '% | ', pca_var_perc_acc[3], '%'))
legend('bottomleft', title = 'PAM', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:24), cex = 1, pt.cex = 1.5, legend = levels(futebol_pam), bty = 'n')

olm_coefs <- olm_two_axis(pca[, 2], pca[, 3])
lines_two_axis(olm_coefs, lty = 2)

dev.off()

# 3.2.5 PCOA (distância euclidiana)----

pcoa = ape::pcoa(futebol_dist)
pcoa_var_perc_acc = round(pcoa$values$Cumul_eig*100, 2)
pcoa = pcoa$vectors
pcoa_var_perc = pcoa_var_perc_acc[1]
for(dim in 3:2){
    pcoa_var_perc[dim] = pcoa_var_perc_acc[dim] - pcoa_var_perc_acc[dim-1]     
}

# 3.2.6 NMDS (distância euclidiana)----

nmds = vegan::metaMDS(comm = futebol_dist, k = 3)
nmds = postMDS2(nmds, futebol_dist)
nmds_var = attr(nmds$points, 'sdev')^2
nmds_var_perc = round(nmds_var/sum(nmds_var)*100, 2)
nmds_var_perc_acc = cumsum(nmds_var_perc)
nmds = nmds$points

# Plotando PCA, PCOA & NMDS (distância euclidiana)

###
par(mfrow = c(2, 3), mar = c(4.25, 4.25, 2.5, 1))

lims = c(min(pca[, 1], pca[, 2]), max(pca[, 1], pca[, 2]))
plot(pca[, 1], pca[, 2], xlim = lims, ylim = lims, pch = c(21:25, 21)[bolas_gro], col = c(rep('black', 5), 'gray')[bolas_gro], bg = adjustcolor(bolas_col, alpha.f = 0.5), cex = 2, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, main = 'PCA', xlab = paste0('PC1 | ', pca_var_perc[1], '% | ', pca_var_perc_acc[1], '%'), ylab = paste0('PC2 | ', pca_var_perc[2], '% | ', pca_var_perc_acc[2], '%'))
legend('bottomleft', title = 'Lotes', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:25, 21), col = c(rep('black', 5), 'gray'), cex = 1.25, pt.cex = 2.25, legend = levels(bolas_gro), bty = 'n')
olm_coefs <- olm_two_axis(pca[, 1], pca[, 2])
lines_two_axis(olm_coefs, lty = 2)
###


png('G - PCA, PCOA & NMDS (distância euclidiana).png', width = 3000, height = 2000, res = 200)
par(mfrow = c(2, 3), mar = c(4.25, 4.25, 2.5, 1))

lims = c(min(pca[, 1], pca[, 2]), max(pca[, 1], pca[, 2]))
plot(pca[, 1], pca[, 2], xlim = lims, ylim = lims, pch = c(21:24)[futebol_pam], bg = adjustcolor(futebol_gro_col, alpha.f = 0.5), cex = 2, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, main = 'PCA', xlab = paste0('PC1 | ', pca_var_perc[1], '% | ', pca_var_perc_acc[1], '%'), ylab = paste0('PC2 | ', pca_var_perc[2], '% | ', pca_var_perc_acc[2], '%'))
legend('bottomleft', title = 'PAM', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:24)[futebol_pam], cex = 1.25, pt.cex = 2.25, legend = levels(futebol_pam), bty = 'n')
olm_coefs <- olm_two_axis(pca[, 1], pca[, 2])
lines_two_axis(olm_coefs, lty = 2)

lims = c(min(pcoa[, 1], pcoa[, 2]), max(pcoa[, 1], pcoa[, 2]))
plot(pcoa[, 1], pcoa[, 2], xlim = lims, ylim = lims, pch = c(21:24)[futebol_pam], bg = adjustcolor(futebol_gro_col, alpha.f = 0.5), cex = 2, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, main = 'PCOA', xlab = paste0('PCO1 | ', pcoa_var_perc[1], '% | ', pcoa_var_perc_acc[1], '%'), ylab = paste0('PCO2 | ', pcoa_var_perc[2], '% | ', pcoa_var_perc_acc[2], '%'))
legend('bottomleft', title = 'PAM', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:24)[futebol_pam], cex = 1.25, pt.cex = 2.25, legend = levels(futebol_pam), bty = 'n')
olm_coefs <- olm_two_axis(pcoa[, 1], pcoa[, 2])
lines_two_axis(olm_coefs, lty = 2)

lims = c(min(nmds[, 1], nmds[, 2]), max(nmds[, 1], nmds[, 2]))
plot(nmds[, 1], nmds[, 2], xlim = lims, ylim = lims, pch = c(21:24)[futebol_pam], bg = adjustcolor(futebol_gro_col, alpha.f = 0.5), cex = 2, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, main = 'NMDS', xlab = paste0('NMDS1 | ', nmds_var_perc[1], '% | ', nmds_var_perc_acc[1], '%'), ylab = paste0('NMDS2 | ', nmds_var_perc[2], '% | ', nmds_var_perc_acc[2], '%'))
legend('bottomleft', title = 'PAM', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:24)[futebol_pam], cex = 1.25, pt.cex = 2.25, legend = levels(futebol_pam), bty = 'n')
olm_coefs <- olm_two_axis(nmds[, 1], nmds[, 2])
lines_two_axis(olm_coefs, lty = 2)

lims = c(min(pca[, 2], pca[, 3]), max(pca[, 2], pca[, 3]))
plot(pca[, 2], pca[, 3], xlim = lims, ylim = lims, pch = c(21:24)[futebol_pam], bg = adjustcolor(futebol_gro_col, alpha.f = 0.5), cex = 2, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, main = 'PCA', xlab = paste0('PC2 | ', pca_var_perc[2], '% | ', pca_var_perc_acc[2], '%'), ylab = paste0('PC3 | ', pca_var_perc[3], '% | ', pca_var_perc_acc[3], '%'))
legend('bottomleft', title = 'PAM', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:24)[futebol_pam], cex = 1.25, pt.cex = 2.25, legend = levels(futebol_pam), bty = 'n')
olm_coefs <- olm_two_axis(pca[, 2], pca[, 3])
lines_two_axis(olm_coefs, lty = 2)

lims = c(min(pcoa[, 2], pcoa[, 3]), max(pcoa[, 2], pcoa[, 3]))
plot(pcoa[, 2], pcoa[, 3], xlim = lims, ylim = lims, pch = c(21:24)[futebol_pam], bg = adjustcolor(futebol_gro_col, alpha.f = 0.5), cex = 2, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, main = 'PCOA', xlab = paste0('PCO2 | ', pcoa_var_perc[2], '% | ', pcoa_var_perc_acc[2], '%'), ylab = paste0('PCO3 | ', pcoa_var_perc[3], '% | ', pcoa_var_perc_acc[3], '%'))
legend('bottomleft', title = 'PAM', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:24)[futebol_pam], cex = 1.25, pt.cex = 2.25, legend = levels(futebol_pam), bty = 'n')
olm_coefs <- olm_two_axis(pcoa[, 2], pcoa[, 3])
lines_two_axis(olm_coefs, lty = 2)

lims = c(min(nmds[, 2], nmds[, 3]), max(nmds[, 2], nmds[, 3]))
plot(nmds[, 2], nmds[, 3], xlim = lims, ylim = lims, pch = c(21:24)[futebol_pam], bg = adjustcolor(futebol_gro_col, alpha.f = 0.5), cex = 2, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, main = 'NMDS', xlab = paste0('NMDS2 | ', nmds_var_perc[2], '% | ', nmds_var_perc_acc[2], '%'), ylab = paste0('NMDS3 | ', nmds_var_perc[3], '% | ', nmds_var_perc_acc[3], '%'))
legend('bottomleft', title = 'PAM', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:24)[futebol_pam], cex = 1.25, pt.cex = 2.25, legend = levels(futebol_pam), bty = 'n')
olm_coefs <- olm_two_axis(nmds[, 2], nmds[, 3])
lines_two_axis(olm_coefs, lty = 2)

dev.off()

# 3.2.7 PCOA (distância personalizada)----

futebol_dist = dist_futebol(futebol_var)

pcoa = ape::pcoa(futebol_dist)
pcoa_var_perc_acc = round(pcoa$values$Cumul_eig*100, 2)
pcoa = pcoa$vectors
pcoa_var_perc = pcoa_var_perc_acc[1]
for(dim in 3:2){
    pcoa_var_perc[dim] = pcoa_var_perc_acc[dim] - pcoa_var_perc_acc[dim-1]     
}

# 3.2.6 NMDS (distância personalizada)----

nmds = vegan::metaMDS(comm = futebol_dist, k = 3)
nmds = postMDS2(nmds, futebol_dist)
nmds_var = attr(nmds$points, 'sdev')^2
nmds_var_perc = round(nmds_var/sum(nmds_var)*100, 2)
nmds_var_perc_acc = cumsum(nmds_var_perc)
nmds = nmds$points

# Plotando PCA, PCOA & NMDS

png('G - PCA, PCOA & NMDS (distância personalizada).png', width = 3000, height = 2000, res = 200)
par(mfrow = c(2, 3), mar = c(4.25, 4.25, 2.5, 1))

lims = c(min(pca[, 1], pca[, 2]), max(pca[, 1], pca[, 2]))
plot(pca[, 1], pca[, 2], xlim = lims, ylim = lims, pch = c(21:24)[futebol_pam], bg = adjustcolor(futebol_gro_col, alpha.f = 0.5), cex = 2, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, main = 'PCA', xlab = paste0('PC1 | ', pca_var_perc[1], '% | ', pca_var_perc_acc[1], '%'), ylab = paste0('PC2 | ', pca_var_perc[2], '% | ', pca_var_perc_acc[2], '%'))
legend('bottomleft', title = 'PAM', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:24)[futebol_pam], cex = 1.25, pt.cex = 2.25, legend = levels(futebol_pam), bty = 'n')
olm_coefs <- olm_two_axis(pca[, 1], pca[, 2])
lines_two_axis(olm_coefs, lty = 2)

lims = c(min(pcoa[, 1], pcoa[, 2]), max(pcoa[, 1], pcoa[, 2]))
plot(pcoa[, 1], pcoa[, 2], xlim = lims, ylim = lims, pch = c(21:24)[futebol_pam], bg = adjustcolor(futebol_gro_col, alpha.f = 0.5), cex = 2, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, main = 'PCOA', xlab = paste0('PCO1 | ', pcoa_var_perc[1], '% | ', pcoa_var_perc_acc[1], '%'), ylab = paste0('PCO2 | ', pcoa_var_perc[2], '% | ', pcoa_var_perc_acc[2], '%'))
legend('bottomleft', title = 'PAM', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:24)[futebol_pam], cex = 1.25, pt.cex = 2.25, legend = levels(futebol_pam), bty = 'n')
olm_coefs <- olm_two_axis(pcoa[, 1], pcoa[, 2])
lines_two_axis(olm_coefs, lty = 2)

lims = c(min(nmds[, 1], nmds[, 2]), max(nmds[, 1], nmds[, 2]))
plot(nmds[, 1], nmds[, 2], xlim = lims, ylim = lims, pch = c(21:24)[futebol_pam], bg = adjustcolor(futebol_gro_col, alpha.f = 0.5), cex = 2, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, main = 'NMDS', xlab = paste0('NMDS1 | ', nmds_var_perc[1], '% | ', nmds_var_perc_acc[1], '%'), ylab = paste0('NMDS2 | ', nmds_var_perc[2], '% | ', nmds_var_perc_acc[2], '%'))
legend('bottomleft', title = 'PAM', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:24)[futebol_pam], cex = 1.25, pt.cex = 2.25, legend = levels(futebol_pam), bty = 'n')
olm_coefs <- olm_two_axis(nmds[, 1], nmds[, 2])
lines_two_axis(olm_coefs, lty = 2)

lims = c(min(pca[, 2], pca[, 3]), max(pca[, 2], pca[, 3]))
plot(pca[, 2], pca[, 3], xlim = lims, ylim = lims, pch = c(21:24)[futebol_pam], bg = adjustcolor(futebol_gro_col, alpha.f = 0.5), cex = 2, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, main = 'PCA', xlab = paste0('PC2 | ', pca_var_perc[2], '% | ', pca_var_perc_acc[2], '%'), ylab = paste0('PC3 | ', pca_var_perc[3], '% | ', pca_var_perc_acc[3], '%'))
legend('bottomleft', title = 'PAM', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:24)[futebol_pam], cex = 1.25, pt.cex = 2.25, legend = levels(futebol_pam), bty = 'n')
olm_coefs <- olm_two_axis(pca[, 2], pca[, 3])
lines_two_axis(olm_coefs, lty = 2)

lims = c(min(pcoa[, 2], pcoa[, 3]), max(pcoa[, 2], pcoa[, 3]))
plot(pcoa[, 2], pcoa[, 3], xlim = lims, ylim = lims, pch = c(21:24)[futebol_pam], bg = adjustcolor(futebol_gro_col, alpha.f = 0.5), cex = 2, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, main = 'PCOA', xlab = paste0('PCO2 | ', pcoa_var_perc[2], '% | ', pcoa_var_perc_acc[2], '%'), ylab = paste0('PCO3 | ', pcoa_var_perc[3], '% | ', pcoa_var_perc_acc[3], '%'))
legend('bottomleft', title = 'PAM', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:24)[futebol_pam], cex = 1.25, pt.cex = 2.25, legend = levels(futebol_pam), bty = 'n')
olm_coefs <- olm_two_axis(pcoa[, 2], pcoa[, 3])
lines_two_axis(olm_coefs, lty = 2)

lims = c(min(nmds[, 2], nmds[, 3]), max(nmds[, 2], nmds[, 3]))
plot(nmds[, 2], nmds[, 3], xlim = lims, ylim = lims, pch = c(21:24)[futebol_pam], bg = adjustcolor(futebol_gro_col, alpha.f = 0.5), cex = 2, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, main = 'NMDS', xlab = paste0('NMDS2 | ', nmds_var_perc[2], '% | ', nmds_var_perc_acc[2], '%'), ylab = paste0('NMDS3 | ', nmds_var_perc[3], '% | ', nmds_var_perc_acc[3], '%'))
legend('bottomleft', title = 'PAM', ncol = 2, x.intersp = 0.5, y.intersp = 1, pch = c(21:24)[futebol_pam], cex = 1.25, pt.cex = 2.25, legend = levels(futebol_pam), bty = 'n')
olm_coefs <- olm_two_axis(nmds[, 2], nmds[, 3])
lines_two_axis(olm_coefs, lty = 2)

dev.off()

# 3.3 Discriminação ----

# 3.3.1 CDA ----

lm <- lm(as.matrix(futebol_var) ~ Position, futebol)
cda <- candisc::candisc(lm, data = futebol)

png('G - CDA (posição).png', width = 1000, height = 1000, res = 200)
par(mfrow = c(1, 1), mar = c(4, 4, 2.5, 2.5))
plot(cda, main = 'CDA', pch = 1:6, col = rainbow(24), var.col = 'purple', scale = 6, conf = 0, cex = 1.5, prefix = "CAN")
dev.off()

lm <- lm(as.matrix(futebol_var) ~ pam, futebol)
cda <- candisc::candisc(lm, data = futebol)

png('G - CDA (PAM).png', width = 1000, height = 1000, res = 200)
par(mfrow = c(1, 1), mar = c(4, 4, 2.5, 2.5))
plot(cda, main = 'CDA', pch = 1:3, col = rainbow(4), var.col = 'purple', scale = 6, conf = 0, cex = 1.5, prefix = "CAN")
dev.off()

# 3.3.2 Árvores de decisão ----

tree = tree::tree(Position ~ Crossing + Finishing + HeadingAccuracy + ShortPassing + Volleys + Dribbling + Curve + FKAccuracy + LongPassing + BallControl + Acceleration + SprintSpeed + Agility + Reactions + Balance + ShotPower + Jumping + Stamina + Strength + LongShots + Aggression + Interceptions + Positioning + Vision + Penalties + Composure + Marking + StandingTackle + SlidingTackle, futebol)

png('G - arvore de decisao, posição.png', width = 1000, height = 1500, res = 200)
par(mfrow = c(1, 1))
plot(tree)
text(tree, cex = 1)
dev.off()

tree = tree::tree(pam ~ Crossing + Finishing + HeadingAccuracy + ShortPassing + Volleys + Dribbling + Curve + FKAccuracy + LongPassing + BallControl + Acceleration + SprintSpeed + Agility + Reactions + Balance + ShotPower + Jumping + Stamina + Strength + LongShots + Aggression + Interceptions + Positioning + Vision + Penalties + Composure + Marking + StandingTackle + SlidingTackle, futebol)

png('G - arvore de decisao, PAM.png', width = 1000, height = 1500, res = 200)
par(mfrow = c(1, 1))
plot(tree)
text(tree, cex = 1)
dev.off()

# 3.3.3 Random Forest ----

set.seed(1)
rf <- randomForest::randomForest(futebol_var, futebol[, 'pam'], importance = T, proximity = T, ntree = 5000)

png('G - random forests, PAM.png', width = 2500, height = 1200, res = 200)
par(mfrow = c(1, 2), mar = c(4, 4, 1, 4))
randomForest::varImpPlot(rf, type = 1, pch = 19, col = 1, cex = 1, main = "Accuracy Plot")
randomForest::varImpPlot(rf, type = 2, pch = 19, col = 1, cex = 1, main = "Gini Plot")
dev.off()

# Extracting the 10 most important features (by accuracy)
imp_features = rf$importance[, c('MeanDecreaseAccuracy', 'MeanDecreaseGini')]
imp_features[, 'MeanDecreaseAccuracy'] = imp_features[, 'MeanDecreaseAccuracy']/rf$importanceSD[, 'MeanDecreaseAccuracy']
imp_features = imp_features[order(imp_features[, 'MeanDecreaseAccuracy'], decreasing = T), ]
imp_features = rownames(imp_features)[1:10]

# 3.3.3.1 De volta ao CDA ----

lm <- lm(as.matrix(futebol_var[, imp_features]) ~ pam, futebol)
cda <- candisc::candisc(lm, data = futebol)

png('G - CDA com menos fatores (PAM).png', width = 1000, height = 1000, res = 200)
par(mfrow = c(1, 1), mar = c(4, 4, 2.5, 2.5))
plot(cda, main = 'CDA', pch = 1:3, col = c('red', 'blue', 'green'), var.col = 'purple', scale = 6, conf = 0, cex = 1.5, prefix = "CAN")
dev.off()

# 3.4 Interpretação ----

# 3.4.1 RDA ----

rda = vegan::rda(formula = futebol_var ~ Position + Age + Height + Weight, data = futebol)

for(scaling in 1:2){
    for(i in 1:2){
        choices = if(i == 1){c(1, 2)} else {c(3, 2)}
        plot_info = plot(rda, scaling = scaling, choices = choices)
        multiplier = if(i == 1){attr(plot_info$biplot, 'arrow.mul')} else {multiplier}
        summ = summary(rda)
        species = plot_info$species
        sites = plot_info$sites
        centroids = plot_info$centroids
        biplot = plot_info$biplot * multiplier
        
        comp = round(c(summ$cont$importance[2, i], summ$cont$importance[2, i+1])*100, 1)
        comp_acc = round(c(summ$cont$importance[3, i], summ$cont$importance[3, i+1])*100, 1)
        cons = round(c(summ$concont$importance[2, i], summ$concont$importance[2, i+1])*100, 1)
        cons_acc = round(c(summ$concont$importance[3, i], summ$concont$importance[3, i+1])*100, 1)
        
        xlim = c(min(species[, 1], sites[, 1], centroids[, 1], biplot[, 1]), max(species[, 1], sites[, 1], centroids[, 1], biplot[, 1])) * 1.1
        ylim = c(min(species[, 2], sites[, 2], centroids[, 2], biplot[, 2]), max(species[, 2], sites[, 2], centroids[, 2], biplot[, 2])) * 1.1
        
        png(paste0('G - RDA (scaling_', scaling, ') dims_', i, 'vs', i+1, '.png'), width = 1000, height = 1000, res = 200)
        par(mfrow = c(1, 1), mar = c(4, 4, 2.5, 2.5))
        j = ifelse(i == 1, 1, 2)
        k = ifelse(i == 2, 1, 2)
        plot(sites[, 1], sites[, 2], xlim = xlim, ylim = ylim, pch = 21, cex = 2, bg = futebol_gro_col,
             xlab = paste0('RDA', choices[1], ' (', comp[j], '% | ', comp_acc[j], '% || ', cons[j], '% | ', cons_acc[j], '%)'), 
             ylab = paste0('RDA', choices[2], ' (', comp[k], '% | ', comp_acc[k], '% || ', cons[k], '% | ', cons_acc[k], '%)'))
        abline(h = 0, v = 0, lty = 2)
        points(species[, 1], species[, 2], cex = 2, pch = 4, lwd = 2, col = 'gray')
        text(species[, 1], species[, 2], cex = 1, pos = 3, offset = 1.5, labels = rownames(species), col = 'gray')
        points(centroids[, 1], centroids[, 2], cex = 2, pch = 4, lwd = 2, col = 'white')
        text(centroids[, 1], centroids[, 2], cex = 1, pos = 3, offset = 1.5, labels = rownames(centroids))
        arrows(0, 0, biplot[, 1], biplot[, 2], lwd = 1.5, length = 0.1)
        text(biplot[, 1], biplot[, 2], cex = 1, pos = 3, offset = 1.5, labels = rownames(biplot))
        dev.off()
    }
}

save.image('multivariate_analysis.RData')
