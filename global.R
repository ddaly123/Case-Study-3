cancer <- read.csv("C:\\Users\\ddaly\\Documents\\Daly_CS3_DS501\\Data\\breast-cancer.csv", header = FALSE)
colnames(cancer) <- c("Class", "Age", "Menopause", "Tumor_Size", "Inv_Nodes", "Node_Caps", "Deg-Malig", "Breast", "Breast_Quad", "Irradiat")
vis_choices = list("Age", "Menopause", "Tumor_Size", "Inv_Nodes", "Node_Caps", "Deg-Malig", "Breast", "Breast_Quad", "Irradiat")
set.seed(123)
split = sample.split(cancer$Class, SplitRatio = 0.8)
train_set = subset(cancer, split == TRUE)
test_set = subset(cancer, split == FALSE)
valid_index <- createDataPartition(cancer$Class, p= 0.8, list=FALSE)
valid <- cancer[-valid_index,]
cancer <- cancer[valid_index,]