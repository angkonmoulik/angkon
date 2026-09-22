library(limma)
library(gclus)
library(Biobase)
library(dplyr)


group <- factor(colnames(comb1))
design <-model.matrix(~ group)
fit <- lmFit(comb1, design)
fit2 <- eBayes(fit)
results <- topTable(fit2, coef=2, n = nrow(comb1),
                    sort.by = "p")
head(results)

##Select Upregulated genes
UP_Data <- results %>%
  filter(  adj.P.Val <0.05 & logFC > 0.9 )

dim(UP_Data )
head(UP_Data)
UP_Genes <-row.names(UP_Data);UP_Genes
write.csv(UP_Genes,file="UP.csv")



##Select Downregulated genes
Down_Data <- results %>%
  filter(  adj.P.Val <0.05 & logFC < -0.9)

dim(Down_Data )
Down_Genes <-row.names(Down_Data);Down_Genes
write.csv(Down_Genes,file="DOWN.csv")


#DEG 

DEGs <- c(UP_Genes,Down_Genes)
length(DEGs)
write.csv(DEGs,file="DEG5LIMMA.csv")
f1=matrix(0,3,3)
f1
N