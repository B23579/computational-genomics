setwd("~/Documents/computational-genomics/kap1-experiment")
library(edgeR)

counts<-read.csv("data/GSE105128_genecount.csv")
head(counts)
dim(counts)

"WT: normal cell, KO: knockout cell. The counts has 7 columns, the first is the gene name. 
It contains the id that identifies the different genes, every raw is related to a single 
gene in the mouse genome.The column WT1 represent the wise type relative to the expression 
level of every normal gene in the mice genome inreplication 1, the second WT2 
contains the expression level of the normal T regulatory cell  and same for WT3. 
KO1 contains expression level of the T-reg cell where KAP1 is shut down and 
cannot produce the gene. ame interpretation for replicate 2 and 3. " 

"This experiment/data aim to identify the genes that behave differently. We check 
whether they have different average levels in Ko t-reg cells compared to With 
type cells. In this experiment, we will be able to determine which portions of
a gene in the mouse genome in the T-reg cell are probably regulated by kap1. 
It is important because we need to know all the different relationships between 
cells, especially for a cell associated with a disease, to identify the type of
gene responsible for a specific disease. Kap1 off implies the autoimmunity."

# Let's explore the data
"Quality check using edgeR"

"1) Load data in edgeR
In this data, rows are gene and columns are relative to samples"

data<-DGEList(counts=counts[,2:7], genes = counts[,1]) # counts receive the matrix of sample

class(data)

data

"a) Let's apply the PCA"

plotMDS(data)
"Interpretation:
We see in the x-axis that there are two groups, but this is not what we expected.
This is telling us that this data may have an issue. We just need to say there 
is something wrong as an assumtion. "



"Let's check the expression level of kap1 to see what happen, 
(why do we decide to check the kap1 gene expression?)"

"Because the authors of the paper did the following experiment: they took two
mice strains and turned off the expression of kap1 in one of them. The group 
with the kap1 expression turned off is called KO, the other WT. Since the mice 
don't cluster according to the experimental condition, The suggestion is to check 
whether the experiment was truly going as expected, meaning that the expression 
of kap1 was lower in the KO group"

"How to get acces to kap1? kap1 is call Trim28, to retried it id gene, let's look
at mart_export.txt using grep Trim28 mart_export.tx , we got 
ENSMUSG00000005566      Trim28"

"We have to retrieve the row that contain ENSMUSG00000005566"

su<-subset(counts,geneName =="ENSMUSG00000005566")
par(mfrow=c(1,2))
plotMDS(data)
barplot(as.numeric(su[,2:7]), names=colnames(counts[,2:7]))

"Interpretation:
WT3 is behaving similarly to KO1 and k02, and KO3 is behaving similarly to WT1 
and WT2. In this case, one of the most likely interpretations is that one sample
was mislabeled. This could happen when we are working with multiple samples at time.
"

"Now, we want to see what will happen during the analysis and compare the result
to the one on the paper by doing a differential expression analysis. We will do 
two types of analysis, in the first one, we will keep the label, and in the second one,
we will correct the label and then perform the analysis.  "

"For the analysis, we need to specify the group."

group<- factor(c("WT","WT","WT","KO","KO","KO"))
group
group <-relevel(group,ref = "WT")
group

data<-DGEList(counts=counts[,2:7], genes = counts[,1], group = group)
data

"Let's follow the pipeline for analysis"

"1) Nomalization factor"

data<-calcNormFactors(data)
data

" Sample show the nowmalization factors that was calculated"

"In the next step, we are going to apply the generalized linear model to perform
the statistical analysis. Usually, the test can be applied. We need to estimate 
the dispersion between the average of the whole data. It is done with a specific 
function. Strong signals have high dispersion and lower movement have low signals.
The first step is to use the model matrix to calculate the expression."

design <-model.matrix(~group)
design
" 1 correspond to the groupe of ko"

data<-estimateDisp(data,design)
data

"Dispersion is information that will be used for linear modeling we are going 
to do "

"Let's fit the linear model to our data"

fit<-glmFit(data)
fit
"In every step, information is going to be added. Fitted. values are the normalized 
expression"

"Let's perform the differential expression analysis using glmlrt to perform 
gene-wise statistical test"

lrt<-glmLRT(fit)
lrt
"lrt contains information about statistical analysis. The table reports the logFC, 
logCPM, LR, and p-value. Let's use the function toptags to extract the most 
expressed genes"

res<- topTags(lrt, n=nrow(counts))
head(res)

"How many genes are significantly differentially expressed?"
sum(res$table$FDR<0.05)
res_tab<-as.data.frame(res)
res_tab[(res$table$FDR<0.05),]

" This result is very different to what was published"

"Let's look at the value of kap1"
subset(res_tab, genes=="ENSMUSG00000005566")
"FDR is 1, so kap1 is not differentialy express."

"Let's do the same analysis by  swapping the label"
"Let's change the group"

colnames(counts)<-c("geneNames","WT1","WT2", 'KO3',"KO1","KO2","WT3")
head(counts)

group<-factor(c("WT","WT", 'KO',"KO","KO","WT"))
group<-relevel(group, ref="WT")
group

data<-DGEList(counts = counts[,2:7],genes =counts[,1], group = group)
data
par(mfrow=c(1,1))
plotMDS(data)

design<-model.matrix(~group)
data<-calcNormFactors(data)
data<-estimateDisp(data,design = design)
fit<-glmFit(data)
lrt<-glmLRT(fit)
res2<-topTags(lrt, n=nrow(counts))
head(res2)
"We get the very significant value"

res.tab2 <- as.data.frame(res2)

summary(decideTests(lrt))

"Let's check which gene is ENSMUSG00000025885. We use the following command :
grep ENSMUSG00000025885 mart_export.txt, the result is Myo5b which is not present
in figure c published by the paper. grep ENSMUSG00000051726 mart_export.txt,
the output is Kcnf1 which is one of the most expressed genes with kap1 in figure
c on the paper. 
so they did the correct analysis, but the data was not mislabeled. During the 
study of this kind of job, we need to be very critical and suspicious of public data."
