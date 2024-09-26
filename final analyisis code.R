Bikas<-read.csv("E:/Fertility problems in Mung bean/Mung Final analysis Folder/MFA.csv", header = TRUE)
attach(Bikas)
print(Bikas)
options(max.print = 1000)
Bikas$REP=as.factor(REP)
Bikas$VAR=as.factor(VAR)
require(gvlma)
fit=lm(GY.ha ~REP+VAR) 
gvmodelfit=gvlma(fit) 
gvmodelfit
boxplot(GY.ha )
attach(Bikas)
Bikas
anova(lm(GY.ha ~(REP+VAR)))
require(agricolae)
aggregate(GY.ha ,by=list(VAR),mean)
aggregate(GY.ha ,by=list(VAR),sd)
sd_var<-sd(GY.ha )
sample_size<-length(GY.ha )
standard_error<-sd_var/sqrt(sample_size)
standard_error
dmrtcomparison<-with(Bikas,duncan.test(GY.ha , VAR, 222 , 0.016 ))      
dmrtcomparison
#to study the all the PCV, gcv, heritablity and other traits of data
View(Bikas)
library(variability)
gen.var(Bikas[3:39],Bikas$VAR,Bikas$REP)

#to study the PCA OF ALL THE DATA
View(Bikas)
sc1<-scale(Bikas[-1 : -2], center = TRUE)
sc1
getwd()
library(metan)
?network_plot


######for creating the circular corelation plot 
require(ggplot2)
library(reshape2)
Ex1<-all$cor
Ex1<-as.data.frame(Ex1)
class(Ex1)
corr<-data.frame(Ex1)
corr_df<-as.data.frame(corr)
corr_df$variable1<-rownames(corr_df)
melted_corr<-melt(corr_df, id.vars = "NULL", variable.name = "NULL", value.name = "value")
melted_corr
P2<-ggplot(melted_corr, aes(x= variable1, y= variable2, fill=value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "red", mid = "white", high = "green", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  coord_polar() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 9.5),
        axis.text.y = element_text(size = 9))

P2
library(ggplot2)
ggsave(filename = "Figure-6.png", plot = P2,
       width = 10, height = 8, dpi = 800)


require(factoextra)
#for the missing value handeling
col_means<- colMeans(sc1, na.rm = TRUE)
missing_indices <- is.na(sc1)
sc1[missing_indices] <- col_means[missing_indices]
sc1
missing_values<-any(is.na(sc1))
missing_values
missing_values_count <- sum(is.na(sc1))
cat("Number of missing values:", missing_values_count, "\n")
missing_values_matrix<-is.na(sc1)
missing_values_count <- colSums(missing_values_matrix)
print(missing_values_count)
#for the PCA STRACTURAL VALUE DECOMPOSITIONS
PCa1<-princomp(sc1)
PCa1
biplot(PCa1)
PCa1$scores
#for the singular value decompositins
pca2<-prcomp(sc1)
pca2
pca2$x
summary(pca2)
biplot(pca2)
#for the visualization 
fviz_pca(pca2)
fviz_pca_biplot(pca2, repel = TRUE)
#to get the direction of the variables 
fviz_pca_var(pca2, repel = TRUE)
?fviz_pca_var
#alternative code for PCA_BIPLOT
library(factoextra)

PCA2<-fviz_pca_biplot(
  pca2,
  axes = c(1, 2),
  geom = c("point", "text"),
  geom.var = c("arrow", "text"),
  col.ind = "darkgreen",
  fill.ind = "white",
  col.var = "brown",
  fill.var = "black",
  gradient.cols = NULL,
  label = "text",
  label.var = "text",  # Add this line to label variables
  invisible = "none",
  repel = TRUE,
  habillage = "none",
  palette = NULL,
  addEllipses = TRUE,
  title = "PCA - Biplot"
)
PCA2
#alternative code for the PCA variables
fviz_pca_var(pca2,repel = TRUE)
Variable<-fviz_pca_var(pca2, col.var = "contrib",
             repel = TRUE,
             gradient.cols = c("black", "darkgreen", "red"),
             ggtheme = theme_minimal())
Variable

?fviz_pca_var

library(ggplot2)
library(gridExtra)
combined_plot <- grid.arrange(Variable, PCA2, ncol = 2)
ggsave("Figure-12.png", plot = combined_plot, dpi = 800, width = 15, height = 10)
getwd()
####for creating the elipse plot with 4 compartments 
library(factoextra)
library(cluster)  
library(ggplot2)
library(dplyr)
pca_result <- prcomp(sc1, scale. = TRUE) 
pca_result
length(individual_colors)
nrow(pca_result$x)

num_clusters <- 3 
kmeans_result <- kmeans(pca_result$x[, 1:2], centers = num_clusters)
cluster_assignments <- kmeans_result$cluster
cluster_colors <- c("brown","orange", "green") 
individual_colors <- cluster_colors[cluster_assignments]

pca_result <- prcomp(sc1, scale. = TRUE) 
pca_result
length(individual_colors)

biplot <- fviz_pca_biplot(
  pca_result,
  geom = c("point", "text"),
  geom.var = c("arrow", "text"),
  col.ind = individual_colors,
  fill.ind = "white",
  col.var = "black",
  fill.var = "white",
  gradient.cols = NULL,
  label = "var",
  invisible = "none",
  repel = TRUE,
  habillage = "none",
  palette = NULL,
  addEllipses = TRUE,
  title = "PCA - Biplot",
  legend.title = "Cluster Groups",
  label.ind = VAR
)
biplot
getwd()

ggsave(filename = "elipse plot of mungbean.png", plot = biplot,
       width = 12, height = 10, dpi = 800)

biplot <- fviz_pca_biplot(
  pca_result,
  geom = c("point", "text"),
  geom.var = c("arrow", "text"),
  col.ind = individual_colors,
  fill.ind = "white",
  col.var = "black",
  fill.var = "white",
  gradient.cols = NULL,
  label = "var",
  invisible = "none",
  repel = TRUE,
  habillage = "none",
  palette = NULL,
  addEllipses = TRUE,
  title = "PCA - Biplot"
)
biplot
###3333 addition of the Genotypes name in each cluster
genotype_data <- data.frame(Cluster = cluster_assignments, Genotype = VAR)
genotype_data
# Create a PCA biplot with ellipses based on clusters
biplot <- fviz_pca_biplot(
  pca_result,
  geom = c("point", "text"),
  geom.var = c("arrow", "text"),
  col.ind = individual_colors,
  fill.ind = "white",
  col.var = "black",
  fill.var = "white",
  gradient.cols = NULL,
  label = "var",
  invisible = "none",
  repel = TRUE,
  habillage = "none",
  palette = NULL,
  addEllipses = TRUE,
  title = "PCA - Biplot",
  angle.var = 45,  
  legend.title = "Cluster Groups"  
)

biplot
# Add genotype labels to the data points
biplot1 <- biplot + geom_text(data = genotype_data, aes(x = pca_result$x[, 1], y = pca_result$x[, 2], label = Genotype), size = 1.5, vjust = 1)
biplot1
#for hiding the ovelapped text and rotating the Compoennets
biplot2<-biplot + geom_text(data = genotype_data, aes(x = pca_result$x[, 1], y = pca_result$x[, 2], label = Genotype,angle = 45), size = 2, vjust = -1, check_overlap = TRUE)
biplot2
ggsave("Figure-14.png", height = 10, width = 12,dpi = 800)


?fviz_pca_var 
fviz_screeplot(pca2)
# scree plot
fviz_screeplot(pca2)
SP1<-fviz_eig(pca2, addlabels = TRUE, barfill="darkgreen")
SP1
SP2<-fviz_screeplot(pca2, choice="eigenvalue", ncp=10, addlabels = TRUE, barfill="brown")
SP2
library(gridExtra)
getwd()
Combined<-grid.arrange(SP1,SP2, ncol=1, top='Eigen value and % of explain variance')
ggsave("Figure-13.png", plot = Combined, width = 10, height = 8, dpi = 800)
###3contribution of the each principle dimensions
#too see the contribution of Particular contribution by variables
a<-fviz_contrib(pca2, choice = "var",fill = "brown", axes = 1, top = 12)
a
a2<-fviz_contrib(pca2, choice = "var", axes = 2,fill = "darkgreen",top = 9)
a2
a3<-fviz_contrib(pca2, choice = "var", axes = 3,fill = "orange",
                 color = "orange", top = 6)
a3
a4<-fviz_contrib(pca2, choice = "var", axes = 4,fill = "#800080",
                 color = "pink", top = 4)
a4

A4<-grid.arrange(a,a2,a3, a4, ncol=2, top='Contribution of variables to First 2PCs')
getwd()
ggsave("Figure-11.png",plot = A4, width = 10, height = 8, dpi = 800)

require(FactoMineR)
fpca<-PCA(sc1, ncp = 10)
fpca
fpca$eig
fpca$ind$coord
fpca$ind$cos2
fpca$ind$contrib
#scatter plot of pca
fviz_pca_ind(fpca)
fviz_pca_ind(fpca, repel = TRUE, col.ind = "cos2")
fpca$eig
fviz_screeplot(fpca, choice="eigenvalue", ncp=31)
fpca$eig
#for transporting the data into excel sheet we have to mention the data into matrix 
table1<-fpca$eig
class(table1)
table1<-as.data.frame(table1)
class(table1)
library(writexl)
write_xlsx(table1, "eigen value of my mung bean pca.xlsx")
#for the elbow method for the maximum Principle componets
plot(table1$`cumulative percentage of variance`)
pdf("my_plot.pdf", width = 8, height = 6)
plot(table1$`cumulative percentage of variance`)
plot(table1$eigenvalue, col = "darkgreen")
#####################for the creation of the cluster graphis 
library(NbClust)
?dist
sc1<-scale(Bikas[-1 : -2], center = TRUE)
sc1
distance<-dist(sc1)
distance
#elbow method of calculation of the Optimum number of clusters
Bik<-read.csv("D:/MBMAF/MAD.csv")
attach(Bik)
print(Bik)
View(Bik)
library(metan)
?corr_coef
all<-corr_coef(Bik)
options(max.print = 10000)
all
#to extract the data in excell?
table1<-fpca$eig
class(table1)
table1<-as.data.frame(table1)
class(table1)
library(writexl)
write_xlsx(table1, "eigen value of my mung bean pca.xlsx")
#ref data for the extraction of the Correlation Graph
Ex<-all$cor
Ex<-as.data.frame(Ex)
class(Ex)
library(writexl)
write_xlsx(Ex, " corelation vlaue of my Data mung.xlsx")
getwd()
plot(all)
#net method to define the correlation study of the data
require(ggplot2)
require(reshape)
C<-corr_coef(
  Bik,
  type = c("linear", "partial"),
  method = c("pearson", "kendall", "spearman"),
  use = c("pairwise.complete.obs", "everything", "complete.obs"),
  by = NULL,
  verbose = TRUE)
print(C)
plot(C)
#cluster graphic
library(NbClust)
Elimin<-Bik[,-c(2,2)]
Elimin <- na.omit(Elimin)
distance<-dist(Elimin)
distance
print(distance,digits=3)
hc<-hclust(distance)
plot(hc,labels=Bikas$VAR)
rect.hclust(hc, k=4, border = "purple")
rect.hclust(hc, k=4, border = 2:6)

#####MTsI plot 
Bikas<-read.csv("E:/Fertility problems in Mung bean/Mung Final analysis Folder/MFA.csv", header = TRUE)
attach(Bikas)
print(Bikas)
library(metan)
?gamem
model<-gamem(Bikas,
             gen = VAR,
             rep= REP,
             resp = everything())
a=gmd(model,"blupg")
a
print(a,n=2000)
aku<-mgidi(model)
###fixing the selectio  intesity 

SI = 20
mgidi_index = mgidi(model, SI = SI)
p1 <- plot(mgidi_index, SI = SI)
p1
#for the extraction of the all data sets
table2<-aku$PCA
table3<-aku$FA
table4<-aku$MGIDI
table4<-aku$sel_dif
table5<-aku$contri_fac_rank
table6<-aku$contri_fac_rank_sel
class(table2)
class(table3)
class(table4)
class(table5)
class(table6)
table2<-as.data.frame(table2)
table3<-as.data.frame(table3)
table4<-as.data.frame(table4)
table5<-as.data.frame(table5)
table6<-as.data.frame(table6)
class(table2)
class(table3)
class(table4)
class(table5)
class(table6)
library(writexl)
write_xlsx(table2, "eigen value of my mung bean pca.xlsx")
write_xlsx(table3, "FAmy mung bean.xlsx")
write_xlsx(table4, "selection differential of the Mung bean.xlsx")
write_xlsx(table5, "contribution factor rank of the genotypes.xlsx")
write_xlsx(table6, "contribution factor rank of the selected genotypes.xlsx")
getwd()
p1<-plot(aku, type = "contribution")
p1
p2<-plot(aku)
p2

arrange_ggplot(p1, p2)
#new method
get_model_data(model, "lrt")
get_model_data(model, "vcomp")
# Genetic parameters
get_model_data(model, "genpar")

# random effects
get_model_data(model, "ranef")
# Predicted values
predict(model)
?mgidi
P3<-plot(aku,
     type = "contribution",
     genotypes = "all",
     x.lab = "Treatments",
     width = 1,
     title = "The strengths and weaknesses view of treatments",
     rotate = TRUE)

Combined1<-grid.arrange(P3, p1, ncol=2, top='mgdi plot + SW')

ggsave("Figure-16.png",plot = Combined1,  dpi = 800, width = 15, height = 10)
############################3
#DMRT comparision plt
M<-aggregate(Bikas$X30ENN,by=list(VAR),mean)
require(gvlma)
fit=lm(GY.ha~REP+VAR) 
gvmodelfit=gvlma(fit) 
gvmodelfit
anova(lm(X30ENN~(REP+VAR)))
require(agricolae)
dmrtcomparison<-with(Bikas,duncan.test(GY.ha, VAR, 223 , 0.957))      
dmrtcomparison
MSE<-0.957                                                                                                                                                                                                                                                                                                                                                                                                                                                             
n<-240  
alpha<-0.05   
df_error<-223 
q_alpha<-qt(1 - alpha/2, df_error)
LSD_value<-q_alpha * sqrt(MSE / n)
print(LSD_value)
meanval<-Bikas%>%group_by(VAR)%>%summarize(mean(X30ENN))
meanval
sig.let<-dmrtcomparison$groups[order(row.names(dmrtcomparison$groups)),]
sig.let
library(dplyr)
library(tidyr)
library(ggpubr)
P2<-ggplot(data = Bikas, aes(x = VAR, y = GY.ha)) +
  geom_boxplot(aes(fill = VAR)) + geom_violin(width = .7)+
  geom_text(data = Bikas, aes(x = VAR, y = mean(GY.ha), label = sig.let$groups), vjust = 0) +
  geom_errorbar(aes(x = VAR, ymin = GY.ha - 0.6, ymax = GY.ha + 0.6), width = 0.2) +
  ggtitle("DMRT Mean Comparison Plot") + xlab("Genotype of Mung bean") + ylab("Yield.tons/ha")+stat_compare_means(method = "anova")+
  stat_compare_means(label = "p.signif", method = "anova") + rotate_x_text(angle = 45) +
  geom_text(aes(x = VAR[1], y = max(GY.ha) + 1, label = paste("LSD =", round(LSD_value, 3)), hjust = 0), size = 4)

P2
##########################
library(ggplot2)
library(dplyr)
library(ggpubr)

# Assuming 'Bikas' is your dataset and you have already calculated LSD_value
# First, create a summary dataframe for means and significance letters
summary_data <- Bikas %>%
  group_by(VAR) %>%
  summarize(mean_yield = mean(X30ENN, na.rm = TRUE), .groups = 'drop')

# Assuming sig.let is a vector with significance letters corresponding to each VAR
# Make sure the lengths match
if (length(sig.let$groups) == nrow(summary_data)) {
  summary_data <- summary_data %>%
    mutate(significance = sig.let$groups)
} else {
  stop("Length of significance letters does not match number of groups.")
}

# Now create the plot
P21 <- ggplot(data = Bikas, aes(x = VAR, y = X30ENN)) +
  geom_boxplot(aes(fill = VAR)) +
  geom_violin(width = 0.7) +
  geom_text(data = summary_data, aes(x = VAR, y = mean_yield, label = significance), vjust = 0) +
  geom_errorbar(aes(ymin = X30ENN - 0.6, ymax = X30ENN + 0.6), width = 0.2) +
  ggtitle("DMRT Mean Comparison Plot") +
  xlab("Genotype of Mung bean") +
  ylab("30 DAS effective root nodules") +
  stat_compare_means(method = "anova") +
  stat_compare_means(label = "p.signif", method = "anova") +
  rotate_x_text(angle = 45) +
  geom_text(aes(x = VAR[1], y = max(Bikas$X30ENN) + 1, label = paste("LSD =", round(LSD_value, 3))), hjust = 0, size = 4)

print(P21)
####################No of Trifoliate Leaves 
M<-aggregate(Bikas$X45.Ntl,by=list(VAR),mean)
require(gvlma)
fit=lm(X45.Ntl~REP+VAR) 
gvmodelfit=gvlma(fit) 
gvmodelfit
anova(lm(X45.Ntl~(REP+VAR)))
require(agricolae)
dmrtcomparison<-with(Bikas,duncan.test(X45.Ntl, VAR, 223 , 1.5167))      
dmrtcomparison
MSE<-1.5167                                                                                                                                                                                                                                                                                                                                                                                                                                                              
n<-240  
alpha<-0.05   
df_error<-223 
q_alpha<-qt(1 - alpha/2, df_error)
LSD_value<-q_alpha * sqrt(MSE / n)
print(LSD_value)
meanval<-Bikas%>%group_by(VAR)%>%summarize(mean(X45.Ntl))
meanval
sig.let<-dmrtcomparison$groups[order(row.names(dmrtcomparison$groups)),]
sig.let

summary_data <- Bikas %>%
  group_by(VAR) %>%
  summarize(mean_yield = mean(X45.Ntl, na.rm = TRUE), .groups = 'drop')

# Assuming sig.let is a vector with significance letters corresponding to each VAR
# Make sure the lengths match
if (length(sig.let$groups) == nrow(summary_data)) {
  summary_data <- summary_data %>%
    mutate(significance = sig.let$groups)
} else {
  stop("Length of significance letters does not match number of groups.")
}

# Now create the plot
P22 <- ggplot(data = Bikas, aes(x = VAR, y = X45.Ntl)) +
  geom_boxplot(aes(fill = VAR)) +
  geom_violin(width = 0.7) +
  geom_text(data = summary_data, aes(x = VAR, y = mean_yield, label = significance), vjust = 0) +
  geom_errorbar(aes(ymin = X45.Ntl - 0.6, ymax = X45.Ntl + 0.6), width = 0.2) +
  ggtitle("DMRT Mean Comparison Plot") +
  xlab("Genotype of Mung bean") +
  ylab(" 45 DAS Trifoliates Leaves") +
  stat_compare_means(method = "anova") +
  stat_compare_means(label = "p.signif", method = "anova") +
  rotate_x_text(angle = 45) +
  geom_text(aes(x = VAR[1], y = max(Bikas$X45.Ntl) + 1, label = paste("LSD =", round(LSD_value, 3))), hjust = 0, size = 4)

print(P22)
Combined22<-grid.arrange(P22,P11, ncol=2)
getwd()
ggsave("Figure-9.png",plot = Combined22,  dpi = 800, width = 15, height = 10)
############################3

#################next variable 
M<-aggregate(Bikas$PH30DAS,by=list(VAR),mean)
require(gvlma)
fit=lm(PH30DAS~REP+VAR) 
gvmodelfit=gvlma(fit) 
gvmodelfit
anova(lm(PH30DAS~(REP+VAR)))
require(agricolae)
dmrtcomparison<-with(Bikas,duncan.test(PH30DAS, VAR, 223 , 1.5252))      
dmrtcomparison
MSE<-1.5252                                                                                                                                                                                                                                                                                                                                                                                                                                                              
n<-240  
alpha<-0.05   
df_error<-223 
q_alpha<-qt(1 - alpha/2, df_error)
LSD_value<-q_alpha * sqrt(MSE / n)
print(LSD_value)
meanval<-Bikas%>%group_by(VAR)%>%summarize(mean(PH30DAS))
meanval
sig.let<-dmrtcomparison$groups[order(row.names(dmrtcomparison$groups)),]
sig.let

summary_data <- Bikas %>%
  group_by(VAR) %>%
  summarize(mean_yield = mean(PH30DAS, na.rm = TRUE), .groups = 'drop')

# Assuming sig.let is a vector with significance letters corresponding to each VAR
# Make sure the lengths match
if (length(sig.let$groups) == nrow(summary_data)) {
  summary_data <- summary_data %>%
    mutate(significance = sig.let$groups)
} else {
  stop("Length of significance letters does not match number of groups.")
}

# Now create the plot
P3 <- ggplot(data = Bikas, aes(x = VAR, y = PH30DAS)) +
  geom_boxplot(aes(fill = VAR)) +
  geom_violin(width = 0.7) +
  geom_text(data = summary_data, aes(x = VAR, y = mean_yield, label = significance), vjust = 0) +
  geom_errorbar(aes(ymin = PH30DAS - 0.6, ymax = PH30DAS + 0.6), width = 0.2) +
  ggtitle("DMRT Mean Comparison Plot") +
  xlab("Genotype of Mung bean") +
  ylab("30 DAS plant height") +
  stat_compare_means(method = "anova") +
  stat_compare_means(label = "p.signif", method = "anova") +
  rotate_x_text(angle = 45) +
  geom_text(aes(x = VAR[1], y = max(Bikas$PH30DAS) + 1, label = paste("LSD =", round(LSD_value, 3))), hjust = 0, size = 4)

print(P3)
#############45 Days plant height
M<-aggregate(Bikas$X45PH,by=list(VAR),mean)
require(gvlma)
fit=lm(X45PH~REP+VAR) 
gvmodelfit=gvlma(fit) 
gvmodelfit
anova(lm(X45PH~(REP+VAR)))
require(agricolae)
dmrtcomparison<-with(Bikas,duncan.test(X45PH, VAR, 223 , 10.743))      
dmrtcomparison
MSE<-10.743                                                                                                                                                                                                                                                                                                                                                                                                                                                              
n<-240  
alpha<-0.05   
df_error<-223 
q_alpha<-qt(1 - alpha/2, df_error)
LSD_value<-q_alpha * sqrt(MSE / n)
print(LSD_value)
meanval<-Bikas%>%group_by(VAR)%>%summarize(mean(X45PH))
meanval
sig.let<-dmrtcomparison$groups[order(row.names(dmrtcomparison$groups)),]
sig.let

summary_data <- Bikas %>%
  group_by(VAR) %>%
  summarize(mean_yield = mean(X45PH, na.rm = TRUE), .groups = 'drop')

# Assuming sig.let is a vector with significance letters corresponding to each VAR
# Make sure the lengths match
if (length(sig.let$groups) == nrow(summary_data)) {
  summary_data <- summary_data %>%
    mutate(significance = sig.let$groups)
} else {
  stop("Length of significance letters does not match number of groups.")
}

#### Now create the plot
P4 <- ggplot(data = Bikas, aes(x = VAR, y = X45PH)) +
  geom_boxplot(aes(fill = VAR)) +
  geom_violin(width = 0.7) +
  geom_text(data = summary_data, aes(x = VAR, y = mean_yield, label = significance), vjust = 0) +
  geom_errorbar(aes(ymin = X45PH - 0.6, ymax = PH30DAS + 0.6), width = 0.2) +
  ggtitle("DMRT Mean Comparison Plot") +
  xlab("Genotype of Mung bean") +
  ylab("45 DAS plant height") +
  stat_compare_means(method = "anova") +
  stat_compare_means(label = "p.signif", method = "anova") +
  rotate_x_text(angle = 45) +
  geom_text(aes(x = VAR[1], y = max(Bikas$X45PH) + 1, label = paste("LSD =", round(LSD_value, 3))), hjust = 0, size = 4)

print(P4)
##########Diamter 30 DAS diamter of root nodule 
M<-aggregate(Bikas$X30DNL,by=list(VAR),mean)
require(gvlma)
fit=lm(X30DNL~REP+VAR) 
gvmodelfit=gvlma(fit) 
gvmodelfit
anova(lm(X30DNL~(REP+VAR)))
require(agricolae)
dmrtcomparison<-with(Bikas,duncan.test(X30DNL, VAR, 223 , 0.05658))      
dmrtcomparison
MSE<-0.05658                                                                                                                                                                                                                                                                                                                                                                                                                                                              
n<-240  
alpha<-0.05   
df_error<-223 
q_alpha<-qt(1 - alpha/2, df_error)
LSD_value<-q_alpha * sqrt(MSE / n)
print(LSD_value)
meanval<-Bikas%>%group_by(VAR)%>%summarize(mean(X30DNL))
meanval
sig.let<-dmrtcomparison$groups[order(row.names(dmrtcomparison$groups)),]
sig.let

summary_data <- Bikas %>%
  group_by(VAR) %>%
  summarize(mean_yield = mean(X30DNL, na.rm = TRUE), .groups = 'drop')

# Assuming sig.let is a vector with significance letters corresponding to each VAR
# Make sure the lengths match
if (length(sig.let$groups) == nrow(summary_data)) {
  summary_data <- summary_data %>%
    mutate(significance = sig.let$groups)
} else {
  stop("Length of significance letters does not match number of groups.")
}

# Now create the plot
P5 <- ggplot(data = Bikas, aes(x = VAR, y = X30DNL)) +
  geom_boxplot(aes(fill = VAR)) +
  geom_violin(width = 0.7) +
  geom_text(data = summary_data, aes(x = VAR, y = mean_yield, label = significance), vjust = 0) +
  geom_errorbar(aes(ymin = X30DNL - 0.6, ymax = X30DNL + 0.6), width = 0.2) +
  ggtitle("DMRT Mean Comparison Plot") +
  xlab("Genotype of Mung bean") +
  ylab("30 DAS  Diameter of root nodule") +
  stat_compare_means(method = "anova") +
  stat_compare_means(label = "p.signif", method = "anova") +
  rotate_x_text(angle = 45) +
  geom_text(aes(x = VAR[1], y = max(Bikas$X30DNL) + 1, label = paste("LSD =", round(LSD_value, 3))), hjust = 0, size = 4)

print(P5)
###########3Diamter of nodules 45 DAS
M<-aggregate(Bikas$X45.D0Nod,by=list(VAR),mean)
require(gvlma)
fit=lm(X45.D0Nod~REP+VAR) 
gvmodelfit=gvlma(fit) 
gvmodelfit
anova(lm(X45.D0Nod~(REP+VAR)))
require(agricolae)
dmrtcomparison<-with(Bikas,duncan.test(X45.D0Nod, VAR, 223 , 0.1438))      
dmrtcomparison
MSE<-0.1438                                                                                                                                                                                                                                                                                                                                                                                                                                                              
n<-240  
alpha<-0.05   
df_error<-223 
q_alpha<-qt(1 - alpha/2, df_error)
LSD_value<-q_alpha * sqrt(MSE / n)
print(LSD_value)
meanval<-Bikas%>%group_by(VAR)%>%summarize(mean(X45.D0Nod))
meanval
sig.let<-dmrtcomparison$groups[order(row.names(dmrtcomparison$groups)),]
sig.let

summary_data <- Bikas %>%
  group_by(VAR) %>%
  summarize(mean_yield = mean(X45.D0Nod, na.rm = TRUE), .groups = 'drop')

# Assuming sig.let is a vector with significance letters corresponding to each VAR
# Make sure the lengths match
if (length(sig.let$groups) == nrow(summary_data)) {
  summary_data <- summary_data %>%
    mutate(significance = sig.let$groups)
} else {
  stop("Length of significance letters does not match number of groups.")
}

# Now create the plot
P6 <- ggplot(data = Bikas, aes(x = VAR, y = X45.D0Nod)) +
  geom_boxplot(aes(fill = VAR)) +
  geom_violin(width = 0.7) +
  geom_text(data = summary_data, aes(x = VAR, y = mean_yield, label = significance), vjust = 0) +
  geom_errorbar(aes(ymin = X45.D0Nod - 0.6, ymax = X45.D0Nod + 0.6), width = 0.2) +
  ggtitle("DMRT Mean Comparison Plot") +
  xlab("Genotype of Mung bean") +
  ylab("30 DAS plant height") +
  stat_compare_means(method = "anova") +
  stat_compare_means(label = "p.signif", method = "anova") +
  rotate_x_text(angle = 45) +
  geom_text(aes(x = VAR[1], y = max(Bikas$X45.D0Nod) + 1, label = paste("LSD =", round(LSD_value, 3))), hjust = 0, size = 4)

print(P6)
Combined1<-grid.arrange(P3, P4,P5,P6, ncol=2)
getwd()
ggsave("Figure-7.png",plot = Combined1,  dpi = 800, width = 15, height = 10)
############################3
Combined1<-grid.arrange(P3, P4,P5,P6, ncol=2)
getwd()
ggsave("Figure-8.png",plot = Combined1,  dpi = 800, width = 15, height = 10)
############################3
######for next plot ######################3
M<-aggregate(Bikas$X30SPAD,by=list(VAR),mean)
require(gvlma)
fit=lm(X30SPAD~REP+VAR) 
gvmodelfit=gvlma(fit) 
gvmodelfit
anova(lm(X30SPAD~(REP+VAR)))
require(agricolae)
dmrtcomparison<-with(Bikas,duncan.test(X30SPAD, VAR, 223 , 40.066))      
dmrtcomparison
MSE<-40.066                                                                                                                                                                                                                                                                                                                                                                                                                                                               
n<-240  
alpha<-0.05   
df_error<-223 
q_alpha<-qt(1 - alpha/2, df_error)
LSD_value<-q_alpha * sqrt(MSE / n)
print(LSD_value)
meanval<-Bikas%>%group_by(VAR)%>%summarize(mean(X30SPAD))
meanval
sig.let<-dmrtcomparison$groups[order(row.names(dmrtcomparison$groups)),]
sig.let

summary_data <- Bikas %>%
  group_by(VAR) %>%
  summarize(mean_yield = mean(X30SPAD, na.rm = TRUE), .groups = 'drop')

# Assuming sig.let is a vector with significance letters corresponding to each VAR
# Make sure the lengths match
if (length(sig.let$groups) == nrow(summary_data)) {
  summary_data <- summary_data %>%
    mutate(significance = sig.let$groups)
} else {
  stop("Length of significance letters does not match number of groups.")
}

# Now create the plot
P11 <- ggplot(data = Bikas, aes(x = VAR, y = X30SPAD)) +
  geom_boxplot(aes(fill = VAR)) +
  geom_violin(width = 0.7) +
  geom_text(data = summary_data, aes(x = VAR, y = mean_yield, label = significance), vjust = 0) +
  geom_errorbar(aes(ymin = X30SPAD - 0.6, ymax = PH30DAS + 0.6), width = 0.2) +
  ggtitle("DMRT Mean Comparison Plot") +
  xlab("Genotype of Mung bean") +
  ylab("30 SPAD value") +
  stat_compare_means(method = "anova") +
  stat_compare_means(label = "p.signif", method = "anova") +
  rotate_x_text(angle = 45) +
  geom_text(aes(x = VAR[1], y = max(Bikas$X30SPAD) + 1, label = paste("LSD =", round(LSD_value, 3))), hjust = 0, size = 4)

print(P11)

#############45 days spad value plot 
M<-aggregate(Bikas$X45.SPAd.VALUE,by=list(VAR),mean)
require(gvlma)
fit=lm(X45.SPAd.VALUE~REP+VAR) 
gvmodelfit=gvlma(fit) 
gvmodelfit
anova(lm(X45.SPAd.VALUE~(REP+VAR)))
require(agricolae)
dmrtcomparison<-with(Bikas,duncan.test(X45.SPAd.VALUE, VAR, 223 , 256.29))      
dmrtcomparison
MSE<-256.29                                                                                                                                                                                                                                                                                                                                                                                                                                                               
n<-240  
alpha<-0.05   
df_error<-223 
q_alpha<-qt(1 - alpha/2, df_error)
LSD_value<-q_alpha * sqrt(MSE / n)
print(LSD_value)
meanval<-Bikas%>%group_by(VAR)%>%summarize(mean(X45.SPAd.VALUE))
meanval
sig.let<-dmrtcomparison$groups[order(row.names(dmrtcomparison$groups)),]
sig.let

summary_data <- Bikas %>%
  group_by(VAR) %>%
  summarize(mean_yield = mean(X45.SPAd.VALUE, na.rm = TRUE), .groups = 'drop')

# Assuming sig.let is a vector with significance letters corresponding to each VAR
# Make sure the lengths match
if (length(sig.let$groups) == nrow(summary_data)) {
  summary_data <- summary_data %>%
    mutate(significance = sig.let$groups)
} else {
  stop("Length of significance letters does not match number of groups.")
}

# Now create the plot
P12 <- ggplot(data = Bikas, aes(x = VAR, y = X45.SPAd.VALUE)) +
  geom_boxplot(aes(fill = VAR)) +
  geom_violin(width = 0.7) +
  geom_text(data = summary_data, aes(x = VAR, y = mean_yield, label = significance), vjust = 0) +
  geom_errorbar(aes(ymin = X45.SPAd.VALUE - 0.6, ymax = X45.SPAd.VALUE + 0.6), width = 0.2) +
  ggtitle("DMRT Mean Comparison Plot") +
  xlab("Genotype of Mung bean") +
  ylab("45 DAS SPAD value") +
  stat_compare_means(method = "anova") +
  stat_compare_means(label = "p.signif", method = "anova") +
  rotate_x_text(angle = 45) +
  geom_text(aes(x = VAR[1], y = max(Bikas$X45.SPAd.VALUE) + 1, label = paste("LSD =", round(LSD_value, 3))), hjust = 0, size = 4)

print(P12)

####3Root nodules 30 Dyas 
M<-aggregate(Bikas$X30NDl,by=list(VAR),mean)
require(gvlma)
fit=lm(X30NDl~REP+VAR) 
gvmodelfit=gvlma(fit) 
gvmodelfit
anova(lm(X30NDl~(REP+VAR)))
require(agricolae)
dmrtcomparison<-with(Bikas,duncan.test(X30NDl, VAR, 223 , 0.957))      
dmrtcomparison
MSE<-0.957                                                                                                                                                                                                                                                                                                                                                                                                                                                               
n<-240  
alpha<-0.05   
df_error<-223 
q_alpha<-qt(1 - alpha/2, df_error)
LSD_value<-q_alpha * sqrt(MSE / n)
print(LSD_value)
meanval<-Bikas%>%group_by(VAR)%>%summarize(mean(X30NDl))
meanval
sig.let<-dmrtcomparison$groups[order(row.names(dmrtcomparison$groups)),]
sig.let

summary_data <- Bikas %>%
  group_by(VAR) %>%
  summarize(mean_yield = mean(X30NDl, na.rm = TRUE), .groups = 'drop')

# Assuming sig.let is a vector with significance letters corresponding to each VAR
# Make sure the lengths match
if (length(sig.let$groups) == nrow(summary_data)) {
  summary_data <- summary_data %>%
    mutate(significance = sig.let$groups)
} else {
  stop("Length of significance letters does not match number of groups.")
}

# Now create the plot
P13 <- ggplot(data = Bikas, aes(x = VAR, y = X30NDl)) +
  geom_boxplot(aes(fill = VAR)) +
  geom_violin(width = 0.7) +
  geom_text(data = summary_data, aes(x = VAR, y = mean_yield, label = significance), vjust = 0) +
  geom_errorbar(aes(ymin = X30NDl - 0.6, ymax = X30NDl + 0.6), width = 0.2) +
  ggtitle("DMRT Mean Comparison Plot") +
  xlab("Genotype of Mung bean") +
  ylab("30 DAS roots noduls") +
  stat_compare_means(method = "anova") +
  stat_compare_means(label = "p.signif", method = "anova") +
  rotate_x_text(angle = 45) +
  geom_text(aes(x = VAR[1], y = max(Bikas$X30NDl) + 1, label = paste("LSD =", round(LSD_value, 3))), hjust = 0, size = 4)

print(P13)

#####45 days root nodules 
M<-aggregate(Bikas$X45.RNOD,by=list(VAR),mean)
require(gvlma)
fit=lm(X45.RNOD~REP+VAR) 
gvmodelfit=gvlma(fit) 
gvmodelfit
anova(lm(X45.RNOD~(REP+VAR)))
require(agricolae)
dmrtcomparison<-with(Bikas,duncan.test(X45.RNOD, VAR, 223 , 26.825))      
dmrtcomparison
MSE<-26.825                                                                                                                                                                                                                                                                                                                                                                                                                                                               
n<-240  
alpha<-0.05   
df_error<-223 
q_alpha<-qt(1 - alpha/2, df_error)
LSD_value<-q_alpha * sqrt(MSE / n)
print(LSD_value)
meanval<-Bikas%>%group_by(VAR)%>%summarize(mean(X45.RNOD))
meanval
sig.let<-dmrtcomparison$groups[order(row.names(dmrtcomparison$groups)),]
sig.let

summary_data <- Bikas %>%
  group_by(VAR) %>%
  summarize(mean_yield = mean(X45.RNOD, na.rm = TRUE), .groups = 'drop')

# Assuming sig.let is a vector with significance letters corresponding to each VAR
# Make sure the lengths match
if (length(sig.let$groups) == nrow(summary_data)) {
  summary_data <- summary_data %>%
    mutate(significance = sig.let$groups)
} else {
  stop("Length of significance letters does not match number of groups.")
}

# Now create the plot
P14 <- ggplot(data = Bikas, aes(x = VAR, y = X45.RNOD)) +
  geom_boxplot(aes(fill = VAR)) +
  geom_violin(width = 0.7) +
  geom_text(data = summary_data, aes(x = VAR, y = mean_yield, label = significance), vjust = 0) +
  geom_errorbar(aes(ymin = X45.RNOD - 0.6, ymax = X45.RNOD + 0.6), width = 0.2) +
  ggtitle("DMRT Mean Comparison Plot") +
  xlab("Genotype of Mung bean") +
  ylab("45 DAS root nodules") +
  stat_compare_means(method = "anova") +
  stat_compare_means(label = "p.signif", method = "anova") +
  rotate_x_text(angle = 45) +
  geom_text(aes(x = VAR[1], y = max(Bikas$X45.RNOD) + 1, label = paste("LSD =", round(LSD_value, 3))), hjust = 0, size = 4)

print(P14)


library(gridExtra)
Combined1<-grid.arrange(P11, P12,P13,P14, ncol=2)
getwd()
ggsave("Figure-7.png",plot = Combined1,  dpi = 800, width = 15, height = 10)
############################3