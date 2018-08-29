library(ggplot2) # devtools::install_github("hadley/ggplot2")
library(ggalt)   # devtools::install_github("hrbrmstr/ggalt")
library(dplyr)   # for data_frame() & arrange()

# I'm not crazy enough to input all the data; this will have to do for the example
df <- data_frame(Departamento=c("Amazonas", "Ancash", "Apurimac", "Arequipa", "Ayacucho", "Cajamarca",
                           "Callao", "Cusco", "Huancavelica", "Huanuco", "Ica", "Junin",
                           "La Libertad", "Lambayeque", "Lima", "Loreto", "Madre de Dios", "Moquegua",
                           "Pasco", "Piura", "Puno", "San Mart??n", "Tacna", "Tumbes", "Ucayali"),
                 JNC=c(0.158, 0.140, 0.120, 0.0930, 0.120, 0.171,
                           0.169, 0.0900, 0.0960, 0.0870, 0.163, 0.100,
                           0.119, 0.104, 0.177, 0.134, 0.0910, 0.142,
                           0.104, 0.151, 0.106, 0.143, 0.163, 0.127, 0.0820),
                 AHA=c(0.350, 0.326, 0.317, 0.300, 0.280, 0.354,
                                 0.358, 0.233, 0.208, 0.211, 0.363, 0.294,
                                 0.270, 0.291, 0.381, 0.308, 0.260, 0.320,
                                 0.256, 0.319, 0.286, 0.286, 0.391, 0.299, 0.224),
                 diff= paste("+",sprintf("%.1f", (AHA-JNC)*100))
)

# we want to keep the order in the plot, so we use a factor for Departamento

df$Departamento <- factor(df$Departamento, levels=rev(df$Departamento))


# we only want the first line values with "%" symbols (to avoid chart junk)
# quick hack; there is a more efficient way to do this
percent_first <- function(x) {
  x <- sprintf("%.1f%%", round(x*100,1))
  x[2:length(x)] <- sub("%$", "", x[2:length(x)])
  x
}

gg <- ggplot()
# doing this vs y axis major grid line
gg <- gg + geom_segment(data=df, aes(y=Departamento, yend=Departamento, x=0, xend=0.45), color="#b2b2b2", size=0.15)
# dum???dum???dum!bell
gg <- gg + geom_dumbbell(data=df, aes(y=Departamento, x=JNC, xend=AHA),
                         size=1.5, color="#b2b2b2", size_x = 5, size_xend = 5, colour_x ="#9fb059", colour_xend ="#edae52")
# text below points
gg <- gg + geom_text(data=filter(df, Departamento=="Amazonas"),
                     aes(x=JNC, y=Departamento, label="JNC"),
                     color="#9fb059", size=5, vjust=-2, fontface="bold", family="Calibri")
gg <- gg + geom_text(data=filter(df, Departamento=="Amazonas"),
                     aes(x=AHA, y=Departamento, label="AHA"),
                     color="#edae52", size=5, vjust=-2, fontface="bold", family="Calibri")
# text above points
gg <- gg + geom_text(data=df, aes(x=JNC, y=Departamento, label=percent_first(JNC)),
                     color="#9fb059", size=2.75, vjust=2, family="Calibri")
gg <- gg + geom_text(data=df, color="#edae52", size=2.75, vjust=2, family="Calibri",
                     aes(x=AHA, y=Departamento, label=percent_first(AHA)))
# difference column
gg <- gg + geom_rect(data=df, aes(xmin= 0.5, xmax= 0.6, ymin=-Inf, ymax=Inf), fill="#efefe3")
gg <- gg + geom_text(data=df, aes(label=diff, y=Departamento, x= 0.55), fontface="bold", size=3, family="Calibri")
gg <- gg + geom_text(data=filter(df, Departamento=="Amazonas"), aes(x=0.55, y=Departamento, label="DIFF"),
                     color="#7a7d7e", size=3.1, vjust=-2, fontface="bold", family="Calibri")
gg <- gg + scale_x_continuous(expand=c(0,0), limits=c(0, 0.65))
gg <- gg + scale_y_discrete(expand=c(0.075,0))
gg <- gg + labs(x=NULL, y=NULL, title="T??tulo",
                subtitle="Subt??tulo")
gg <- gg + theme_bw(base_family="Calibri")
gg <- gg + theme(panel.grid.major=element_blank())
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(axis.text.y = element_text(size=16))
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text.x=element_blank())
gg <- gg + theme(plot.title=element_text(face="bold"))
gg <- gg + theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(b=12)))
gg <- gg + theme(plot.caption=element_text(size=7, margin=margin(t=12), color="#7a7d7e"))
gg
