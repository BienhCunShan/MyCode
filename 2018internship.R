library(shiny)

#检验是否正态分布
jianyan<-function(data){
  pvalue<-c(0)
  for (i in 1:nrow(data)) {
    if(sum(is.na(data[i,]))==ncol(data)){
      pvalue[i]<-NA
    }
    else{
      temp<-shapiro.test(na.omit((data[i,])))
      pvalue[i]<-temp$p.value
    }
  }
  return(matrix(pvalue>0.05))
}

#输出是正态分布的位置
jianyan.positon<-function(data){
  pvalue<-c(0)
  for (i in 1:nrow(data)) {
    if(sum(is.na(data[i,]))==ncol(data)){
      pvalue[i]<-NA
    }
    else{
      temp<-shapiro.test(na.omit((data[i,])))
      pvalue[i]<-temp$p.value
    }
  }
  return(matrix(which(pvalue>0.05)))
}

#正态分布的5%，15%，25%，50%，75%，85%，95%分位
fenwei<-function(data){
  q<-matrix(0,nrow(data),7)
  for (i in 1:nrow(data)) {
    s<-sd(na.omit(data[i,]))
    if(is.na(s)==F){
      m<-mean(na.omit(data[i,]))
      q[i,]<-qnorm(c(0.05,0.15,0.25,0.5,0.75,0.85,0.95),mean = m,sd = s)
    }
  }
  return(q)
}

#输出分位数（基于现有数据）
q.matrix<-function(data){
  q<-matrix(0,nrow(data),7)
  for (i in 1:nrow(data)) {
    q[i,]<-quantile(na.omit(data[i,]),c(0.05,0.15,0.25,0.5,0.75,0.85,0.95))
  }
  return(q)
}

#聚类总数为10
#设定种子的kmeans算法——为了固定初值
k<-function(data){
  set.seed(1234)
  return(kmeans(data,10))
}

#确定阈值
#阈值从小到大排列
yuzhi<-function(data){
  temp<-k(na.omit(data))
  cluster<-temp$cluster
  d1<-max(data[which(cluster==1)])
  d2<-max(data[which(cluster==2)])
  d3<-max(data[which(cluster==3)])
  d4<-max(data[which(cluster==4)])
  d5<-max(data[which(cluster==5)])
  d6<-max(data[which(cluster==6)])
  d7<-max(data[which(cluster==7)])
  d8<-max(data[which(cluster==8)])
  d9<-max(data[which(cluster==9)])
  d10<-max(data[which(cluster==10)])
  return(sort(c(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10)))
}

#确认数量
#数量按照对应的阈值（从小到大）排列
shuliang<-function(data,yuzhi){
  n1<-length(which(data<=yuzhi[1]))
  n2<-length(which(data<=yuzhi[2]))
  n3<-length(which(data<=yuzhi[3]))
  n4<-length(which(data<=yuzhi[4]))
  n5<-length(which(data<=yuzhi[5]))
  n6<-length(which(data<=yuzhi[6]))
  n7<-length(which(data<=yuzhi[7]))
  n8<-length(which(data<=yuzhi[8]))
  n9<-length(which(data<=yuzhi[9]))
  n10<-length(which(data<=yuzhi[10]))
  return(c(n1,n2-n1,n3-n2,n4-n3,n5-n4,n6-n5,n7-n6,n8-n7,n9-n8,n10-n9))
}

#得到矩阵的阈值
yuzhi.matrix<-function(data){
  Y<-yuzhi(na.omit(data[1,]))
  for (i in 2:nrow(data)) {
    if(sum(is.na(data[i,]))==ncol(data)){
      Y<-rbind(Y,NA)
    }
    else{
      Y<-rbind(Y,yuzhi(na.omit(data[i,])))
    }
  }
  return(Y)
}

#得到矩阵的比例
shuliang.matrix<-function(data){
  S<-shuliang(na.omit(data[1,]),yuzhi(na.omit(data[1,])))/length(na.omit(data[1,]))
  for (i in 2:nrow(data)) {
    if(sum(is.na(data[i,]))==ncol(data)){
      S<-rbind(S,NA)
    }
    else{
      S<-rbind(S,shuliang(na.omit(data[i,]),yuzhi(na.omit(data[i,])))/length(na.omit(data[i,])))
    }
  }
  return(S)
}

#找na的数量
NA.num<-function(data){
  n<-c(0)
  for (i in 1:nrow(data)) {
    n[i]<-sum(is.na(data[i,]))
  }
  return(n)
}

#判断此行能否能用
#输出值为不能用的行数
NA.pandaun<-function(data){
  n<-NA.num(data)
  index<-which(n>=ncol(data)-10)
  return(index)
}

#判断此行是否可以有10个cluster
unique.num<-function(data){
  n<-c(0)
  for (i in 1:nrow(data)) {
    n[i]<-length(unique(data[i,]))
  }
  return(n)
}

#判断此行是否能用
unique.panduan<-function(data){
  n<-unique.num(data)
  index<-which(n<=11)
  return(index)
}


yuzhi.matrix1<-function(data){
  Y<-yuzhi(na.omit(data[1,]))
  for (i in 2:nrow(data)) {
    
    Y<-rbind(Y,yuzhi(na.omit(data[i,])))
    
  }
  return(Y)
}

#界面
ui<-fluidPage(
  headerPanel("阈值选择系统"),
  sidebarPanel(fileInput(inputId = "dataset",label = "choose csv file",
                         accept=c('text/csv', 'text/comma-separated-values,text/plain')),
               radioButtons('sep', 'Separator',
                            c(Comma=',',
                              Semicolon=';',
                              Tab='\t'),
                            'Comma')
               ),
  mainPanel(
    tableOutput("result")
    )
)

server<-function(input,output){
  output$result<-renderTable({
    inFile<-input$dataset
    data <- read.csv(inFile$datapath,header = F)
    data<-as.matrix(data)
    A<-jianyan(data)
    B<-shuliang.matrix(data)
    C<-yuzhi.matrix(data)
    D<-fenwei(data)
    E<-q.matrix(data)
    nam<-c("value1","value2","value3","value4","value5","value6","value7","value8","value9","value10",
           "p1","p2","p3","p4","p5","p6","p7","p8","p9","p10",
           "5%","15%","25%","50%","75%","85%","95%",
           "5%N","15%N","25%N","50%N","75%N","85%N","95%N",
           "norm")
    write.csv(rbind(nam,cbind(C,B,E,D,A)),file="/Users/sihonghe/Desktop/test.csv",quote=F,row.names = F)
    
    read.csv(file="/Users/sihonghe/Desktop/test.csv",sep=input$sep)
  })
}


shinyApp(server = server, ui = ui)

