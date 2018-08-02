## SML Creation ------------

new_col_name <- NA
for(j in 1:168){
    new_col_name[j]<- paste("H",j-1,sep="_")}


if(exists("SML2")==FALSE){
m <- data.frame(matrix(nrow=168,ncol=168))
SML2 <- list()

for(x in 1:168){
for(i in 1:168){
for(j in 1:168){

p <- c(seq(i,i+x-1,by=1))
p[c(p>168)] <- c(p[c(p>168)]-168)

m[i,c(p)] <- 1}}
m[is.na(m)] <- 0
SML2[[x]] <- m}

for(j in seq_along(SML2)){
names(SML2[[j]]) <- new_col_name}

for(i in 1:length(SML2)){
    names(SML2)[[i]] <- paste0("Shift.",i)}
}

rm(i)
