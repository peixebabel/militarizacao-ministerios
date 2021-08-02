install.packages("ggplot2")
install.packages("gganimate")
install.packages("dplyr")
install.packages("reshape2")
install.packages("devtools")
devtools::install_github("hrbrmstr/streamgraph")

library(ggplot2)
library(gganimate)
library(dplyr)
library(reshape2)
library(devtools)
library(streamgraph)

df <- read.csv('frac_militares_por_orgao_e_mes_ateh_2020-09.csv')
df_transpose <- as.data.frame(t(df[,-1:-2]) )
colnames(df_transpose) <- df$orgao
df_transpose$data <- rownames(df_transpose)

df_melt = melt(df_transpose)
colnames(df_melt) <- c("data", "setor", "fracao")
df_melt[is.na(df_melt)] <- 0

df_tam <- read.csv('numero_servidores_total_e_militares_ativos_inativos_por_orgao_e_mes.csv')

tamanho <- df_tam %>% group_by(orgao) %>% summarise(mean_ = mean(n_total))
df_melt$tamanho <- rep(tamanho$mean_[-4], each=93)

data <- as.Date(paste("1", 01:12, "2013", sep="-"), "%d-%m-%Y")
for (year in 2014:2020){
  data <- append(data, as.Date(paste("1", 01:12, year, sep="-"), "%d-%m-%Y"))
}
df_melt$data <- data[-93:-96]

theme_set(theme_bw())

plot <- ggplot(df_melt,
               aes(x=data, y=fracao, size=tamanho, colour=setor) ) +
              geom_point(show.legend=TRUE, alpha=0.5) +
              theme_minimal() +
              scale_colour_manual(values = rainbow(15))  +
              scale_size(range = c(2,8) ) +
              labs(x='Mês/Ano', y="Fração de Militares")
plot

plot + transition_time(data) +
  labs(title = "Year: {frame_time}") + 
  shadow_mark(alpha = 0.3, size = 1)

plot + facet_wrap(~setor) +
              transition_time(data) +
              labs(title = "Mês/Ano: {frame_time}") +
              shadow_mark(alpha = 0.3) +
              theme_minimal() 


animate(animation, height = 800, width =1200, duration = 20, fps = 20, renderer = av_renderer())
anim_save("small_multiples.mp4")

pp <- streamgraph(df_melt, key="setor", value="fracao", date="data", 
                  height="400px", width="800px") 
pp + theme(axis.text.x = element_text(size=14, angle=45))

p <- ggplot(df_melt, aes(x=data, y=fracao, fill=setor)) + 
        facet_wrap(~setor) +
        annotate("rect", xmin=as.Date("01-01-2019", "%d-%m-%Y"), xmax=as.Date("01-09-2020", "%d-%m-%Y"), 
           ymin=-Inf, ymax=Inf, alpha=0.3, fill="yellow") +
        annotate("rect", xmin=as.Date("01-05-2016", "%d-%m-%Y"), xmax=as.Date("01-01-2019", "%d-%m-%Y"), 
           ymin=-Inf, ymax=Inf, alpha=0.5, fill="gray") +
        geom_area(colour="black", show.legend=FALSE ) +
        labs(x='Mês/Ano', y="Fração de Militares")+
        theme(text = element_text(size=8))
        
ggsave(filename = 'stacked_areas.tiff', height = 1080, width = 1920, units="px", device='tiff', dpi=300, limitsize = FALSE)

stacked <-  ggplot(df_melt, aes(x=data, y=fracao, fill=setor)) + 
            annotate("rect", xmin=as.Date("01-01-2019", "%d-%m-%Y"), xmax=as.Date("01-09-2020", "%d-%m-%Y"), 
                     ymin=-Inf, ymax=Inf, alpha=0.3, fill="yellow") +
            annotate("rect", xmin=as.Date("01-05-2016", "%d-%m-%Y"), xmax=as.Date("01-01-2019", "%d-%m-%Y"), 
                     ymin=-Inf, ymax=Inf, alpha=0.5, fill="gray") +
            facet_wrap(~setor) +
            geom_area(colour="black", show.legend=FALSE)  + 
            transition_reveal(data) +
            labs(x='Mês/Ano', y="Fração de Militares")+
            theme(text = element_text(size=26))


animate(stacked, height = 1080, width = 1920, renderer = av_renderer())
anim_save("stacked_areas.mp4")
