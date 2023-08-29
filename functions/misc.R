
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


lm_eqn <- function(df,formula){
  m <- lm(formula, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}




sector_change<-function(dt){
  dt[,sector_short := sector]
  dt[sector=="Mining of coal and lignite; extraction of peat (10)", sector_short:="Mining of coal and lignite; extraction of peat"]
  dt[sector =="Production of electricity by fossil fuel, biomass, other; Steam supply", sector_short:= "Fossil fuel electricity; steam supply"]
  dt[sector =="Production of electricity by renewables", sector_short:= "Renewable electricity"]
  dt[sector =="Petroleum Refining, Gas Production and Distribution, Coke, Auto Fuel", sector_short:= "Petroleum refining, gas, auto fuel, coke"]
  dt[sector=="Air transport (62)", sector_short:="Air transport"]
  dt[sector=="Post and telecommunications (64)", sector_short:="Post and telecommunications"]
  dt[sector=="Hotels and restaurants (55)", sector_short:="Hotels and restaurants"]
  dt[sector=="Wholesale trade and commission trade, except of motor vehicles and motorcycles (51)", sector_short:="Wholesale trade and commission trade"]
  dt[sector=="Financial intermediation, except insurance and pension funding (65)", sector_short:="Financial intermediation"]
  dt[sector=="Real estate activities (70)", sector_short:="Real estate activities"]
  dt[sector==" \"\"Real estate activities (70)\"\"", sector_short:="Real estate activities"]
  dt[sector=="Insurance and pension funding, except compulsory social security (66)", sector_short:="Insurance and pension funding"]
  dt[sector=="Sale, maintenance, repair of motor vehicles, motor vehicles parts, motorcycles, motor cycles parts and accessoiries", sector_short:="Sale, maintenance, repair of motor vehicles"]
  dt[sector=="Retail trade, except of motor vehicles and motorcycles; repair of personal and household goods (52)", sector_short:="Retail trade"]
  
  dt
}

