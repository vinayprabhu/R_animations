# Clear R workspace
rm(list = ls() ) 
# Set the working directory 
setwd("C:/Users/Lawrence/Documents/R")
library(igraph)
# Now, let us generate the edge list
# e1 = c(rep(1,5),rep(2,19),rep(3,19),rep(4,19),rep(5,19),rep(6,19),seq(7,101))
# e2 = c(seq(2,101),rep(1,length(seq(7,101))))
# el = cbind(e1,e2)
# n_edges = nrow(el)
# g = graph.edgelist(el[,1:2],directed = T)
# plot(g,layout=layout.fruchterman.reingold(g),vertex.label="",edge.arrow.size=0.1)

num_tech=20
num_per_tech=30
num_callers=num_tech*num_per_tech
techies=seq(2,num_tech+1)
callers=seq(num_tech+2,num_tech*num_per_tech+num_tech+1)


tech_dl=vector()
tech_dl_end=vector()
for (i in 1:num_tech)
{
  tech_dl=c(tech_dl,rep(techies[i],num_per_tech))
  caller_strt=min(callers)+num_per_tech*(i-1)
  tech_dl_end=c(tech_dl_end,seq(caller_strt,caller_strt+num_per_tech-1))
  
}
e_from=c(callers,tech_dl)
e_to=c(rep(1,num_callers),tech_dl_end)
el = cbind(e_from,e_to)
n_edges = nrow(el)
g = graph.edgelist(el[,1:2],directed = T)
autocurve.edges(g)
#plot(g,layout=layout.fruchterman.reingold(g),edge.arrow.size=0.1)

#Function for generating random time stamps
latemail <- function(N, st = "2012/01/01", et = "2012/12/31") {
  st <- as.POSIXct(as.Date(st))
  et <- as.POSIXct(as.Date(et))
  dt <- as.numeric(difftime(et,st,unit = "sec"))
  ev <- sort(runif(N, 0, dt))
  rt <- st + ev
}
rand_time_stamps = latemail(1000)

#n_frames=15

shapes = c("sphere",rep("square",num_tech),rep("circle",num_callers))
#layout <- layout.random(g)

#####################################################################
#####################################################################
edge_width = rep(0.01,ecount(g))
edge_arrow_size = rep(0.01,ecount(g))
edge_arrow_width = rep(0.01,ecount(g))
edge_color = rep("gray100",ecount(g))
V(g)$color = rep("grey",vcount(g))
vertex.color = V(g)$color
E(g)$alpha <- 0

# plot(g,layout=layout,
#      #vertex.label=v_shapes,
#      vertex.label="",
#      #vertex.size=1+2*log(graph.strength(g)),
#      vertex.size=vertex_size,
#      vertex.shape=shapes,
#      #edge.label=rand_time_stamps,
#      edge.width=edge_width,
#      edge.color=edge_color,
#      #asp=9/16,margin=-0.15,
#      edge.arrow.size=edge_arrow_size,
#      edge.arrow.width=edge_arrow_width,
#      main="Actors: Callers, SC, Technicians"
# )
#####################################################################

vertex_size = round(log(degree(g)) +1 )
vertex_size[1] = 12
vertex_size[techies] = 6

#layout<- layout.fruchterman.reingold(g)
layout <- layout.random(g)
#layout <- layout.reingold.tilford(g, circular=F)
k = 1
#####################################################################
png(
  "C:/Users/Lawrence/Documents/R/Sears_anim/example%03d.png", width = 2000,height =
    1400
)
#coords <- layout.auto(g)
#plot(gtest, layout=coords, rescale=FALSE, xlim=range(coords[,1]), ylim=range(coords[,2]))


for (i in 1:10)
  #png(file="example%03d.png", width=1600,height=900)
{
  #Pick a bunch of random callers
  random_callers = round(runif(round(runif(
    1, min = 4, max = 9
  )), min = min(callers), max = max(callers)))
  for (j in 1:length(random_callers))
  {
    #The Uplink Call
    edge_selected = get.edge.ids(
      g, c(random_callers[j],1), directed = TRUE, error = FALSE, multi = FALSE
    )
    edge_width[edge_selected] = 5
    edge_arrow_size[edge_selected] = 2
    edge_arrow_width[edge_selected] = 2
    edge_color[edge_selected] = "red"
    V(g)$color[random_callers[j]] = "red"
    V(g)$color[1] = "red"
    E(g)$alpha <- 255
    plot(edge.curved=F,g,layout = layout,
         #vertex.label=v_shapes,
         vertex.label = "",
         #vertex.size=1+2*log(graph.strength(g)),
         vertex.size = vertex_size,
         vertex.shape = shapes,
         #edge.label=rand_time_stamps,
         edge.width = edge_width,
         edge.color = edge_color,
         asp=9/16,margin=-0.15,
         edge.arrow.size = 2,
         edge.arrow.width = 2,
         #       ,
         #       #main="Underlying communication graph"
         #       main=paste("Time stamp:",rand_time_stamps[i]," | ","Caller :#" ,i)
    )
    title(
      paste(
        "Time stamp:",rand_time_stamps[k],"Caller #:",random_callers[j]
      ),cex.main = 3,col.main = "green"
    )
    k = k + 1
  }
  #####################################################################
  #####################################################################
  edge_width = rep(0.01,ecount(g))
  edge_arrow_size = rep(0.01,ecount(g))
  edge_arrow_width = rep(0.01,ecount(g))
  edge_color = rep("gray100",ecount(g))
  V(g)$color = rep("grey",vcount(g))
  vertex.color = V(g)$color
  #####################################################################
  #####################################################################
  for (j in 1:length(random_callers))
  {
    # Find out who the technician is
    nei = neighborhood(g,order = 1, nodes = random_callers[j], mode = "in")
    x = unlist(nei[1])
    tech_chosen = x[x != random_callers[j]]
    edge_selected = get.edge.ids(
      g, c(tech_chosen,random_callers[j]), directed = TRUE, error = FALSE, multi = FALSE
    )
    edge_width[edge_selected] = 5
    edge_arrow_size[edge_selected] = 20
    edge_arrow_width[edge_selected] = 20
    edge_color[edge_selected] = "blue"
    V(g)$color[random_callers[j]] = "blue"
    V(g)$color[tech_chosen] = "blue"
    title_plt = paste("Time stamp:",rand_time_stamps[k]," | ","Service :#" ,i)
    k=k+1
    plot(edge.curved=F,g,layout = layout,
         #vertex.label=v_shapes,
         vertex.label = "",
         #vertex.size=1+2*log(graph.strength(g)),
         vertex.size = vertex_size,
         vertex.shape = shapes,
         #edge.label=rand_time_stamps,
         edge.width = edge_width,
         edge.color = edge_color,
         asp=9/16,margin=-0.15,
         edge.arrow.size = 2,
         edge.arrow.width = 2
         #       ,
         #       #main="Underlying communication graph"
         #       main=title_plt
    )
    title(
      paste("Time stamp:",rand_time_stamps[k],'Service#:',tech_chosen),cex.main = 3,col.main =
        "green"
    )
    k = k + 1
  }
  #####################################################################
  #####################################################################
  edge_width = rep(0.01,ecount(g))
  edge_arrow_size = rep(0.01,ecount(g))
  edge_arrow_width = rep(0.01,ecount(g))
  edge_color = rep("gray100",ecount(g))
  V(g)$color = rep("grey",vcount(g))
  vertex.color = V(g)$color
  E(g)$alpha <- 0
  
  plot(edge.curved=F,g,layout = layout,
       #vertex.label=v_shapes,
       vertex.label = "",
       #vertex.size=1+2*log(graph.strength(g)),
       vertex.size = vertex_size,
       vertex.shape = shapes,
       #edge.label=rand_time_stamps,
       edge.width = edge_width,
       edge.color = edge_color,
       asp=9/16,margin=-0.15,
       edge.arrow.size = edge_arrow_size,
       edge.arrow.width = edge_arrow_width
       #       ,
       #       main=paste("Time stamp:",rand_time_stamps[1])
  )
  title(
    paste("Time stamp:",rand_time_stamps[k]),cex.main = 3,col.main =
      "green"
  )
  k=k+1
  #####################################################################
  #####################################################################
  
  
}
dev.off()
