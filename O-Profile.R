"Draw height profile of orienteering courses"

library("ggplot2")

### import data
dir = "/Users/Michael/Documents/Orienteering/Events/150516\ Laterns\ Nat\ OL/"
classes_file = "Nat_OL_Laterns_FINAL.ClassesV8.txt"
height_file = "control_height.csv"

classes = read.table(paste(dir, classes_file, sep = ""), header = FALSE, sep = ";", fill = TRUE)
control_height = read.table(paste(dir, height_file, sep = ""), header = TRUE, sep = ",")[,2]
names(control_height) = read.table(paste(dir, height_file, sep = ""), header = TRUE, sep = ",")[,1]
df.all = data.frame(class = character(), start = character(), ctrl = character(), dist = numeric(), length = numeric(), height = numeric())

### loop over classes
for (i in 1:nrow(classes)) {
    n = sum(is.finite(as.numeric(classes[i,][seq(7, ncol(classes[i,]), 2)])))
    class = as.vector(rep(classes[i,1], times = n+1))
    ctrl = as.vector(t(classes)[,i][seq(6, 2*n+6, 2)])
    ctrl = gsub(" ","",ctrl)
    start = as.vector(rep(ctrl[1], times = n+1))
    dist = rep(0, times = n+1)
    length = rep(0, times = n+1)
    height = rep(0, times = n+1)
    height[n+1] = (control_height[ctrl[n+1]]) # height of the finish

### loop over controls
    for (j in 1:n) {
        dist[j+1] = as.numeric(classes[i,2*j+5])
        length[j+1] = dist[j+1] + length[j]
        height[j] = (control_height[ctrl[j]])
        }
  
    df.class = data.frame(class = class, start = start, ctrl = ctrl, dist = dist, length = length, height = height)
    df.all = rbind(df.all, df.class)
}

### plot data
p = ggplot(data=df.all, aes(x=length, y=height, colour=class, label=ctrl)) +
    geom_line() +
    geom_point() +
    coord_fixed(ratio=0.01) +
    scale_x_continuous(breaks=seq(min(df.all$length), max(df.all$length), 1)) +
    scale_y_continuous(breaks=seq(min(df.all$height), max(df.all$height), 50)) +
    guides(col=guide_legend(nrow=24)) +
    geom_text(aes(label=ctrl),hjust=-.5, vjust=-.5, size=2)
print(p)

### save plot
ggsave(paste(dir, "O-Profile.pdf", sep = ""), p, width = 30/2.54, height = 21/2.54)
