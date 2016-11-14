# interaction between the variogram and the simulation result
# the simulation is based on Rgeostats package
# also need the package tcltk2 to create the window

library(RGeostats)
library(tcltk2)

simulate_from_vario<-function(){

  #sliders
  win1 <- tktoplevel()
  tktitle(win1) <- "Infering of the model"
  sliderValuenugget <- tclVar("0.")
  sliderValuerange1 <- tclVar("0.5")
  sliderValuean1 <- tclVar("1.")

  win1$env$labelnugget <- tk2label(win1,
                                text = "nugget of Y1: 0.")
  win1$env$labelrange1 <- tk2label(win1,
                                   text = "range of Y1: 0.5")
  win1$env$labelan1 <- tk2label(win1,
                                text = "anisotropy of Y1: 0.5")

  # scroll
  win1$env$model1 <- tk2listbox(win1, height = 4, selectmode = "single")
  tkgrid(tk2label(win1, text = "What type of model for Y1 ?", justify = "left"),
         padx = 10, pady =c(15, 5), sticky = "w")
  tkgrid(win1$env$model1, padx = 10, pady = c(5, 10))

  models <- c("gaussian", "cubic","exponential","spherical")
  for (model in models){
    tkinsert(win1$env$model1, "end", model)
  }
  tkselection.set(win1$env$model1, 2)

  onChange <- function(...) {
    #sliders
    valuenugget <- floor(as.double(tclvalue(sliderValuenugget))*100)/100.
    valuerange1 <- floor(as.double(tclvalue(sliderValuerange1))*100)/100.
    valuean1 <- floor(as.double(tclvalue(sliderValuean1))*10)/10.

    # the value parameters are going to be changed by another function
    nugget<-valuenugget
    r1<-valuerange1
    an1<-valuean1

    labelnugget <- sprintf("nugget: %s", valuenugget)
    labelrange1 <- sprintf("range: %s", valuerange1)
    labelan1 <- sprintf("anisotropy: %s", valuean1)

    tkconfigure(win1$env$labelnugget, text = labelnugget)
    tkconfigure(win1$env$labelrange1, text = labelrange1)
    tkconfigure(win1$env$labelan1, text = labelan1)

    model1Choice <- models[as.numeric(tkcurselection(win1$env$model1)) + 1]

    if (identical(model1Choice, "gaussian")){
      curve( (1-nugget)*(1-exp(-( x/ (r1*sqrt(3) ) )^2))+nugget ,ylim=c(0,1))
    }

    if (identical(model1Choice, "cubic")){
      curve( (1-nugget)*(7*((x/r1)^2)-(35/4)*((x/r1)^3)+(7/2)*((x/r1)^5)-(3/4)*((x/r1)^7))+nugget,from=0,to=r1,xlim=c(0,1),ylim=c(0,1) )
    }

    if (identical(model1Choice, "exponential")){
      curve( (1-nugget)*(1-exp(-x/r1)) +nugget,ylim=c(0,1))
    }

    if (identical(model1Choice, "spherical")){
      curve( (1-nugget)*((3*x/(2*r1) )-( (x^3)/ (2*r1^3)))+nugget,from=0,to=r1,xlim=c(0,1),ylim=c(0,1))
    }
  }

  simulation <- function(...){
    #sliders
    valuenugget <- floor(as.double(tclvalue(sliderValuenugget))*100)/100.
    valuerange1 <- floor(as.double(tclvalue(sliderValuerange1))*100)/100.
    valuean1 <- floor(as.double(tclvalue(sliderValuean1))*10)/10.

    # the value parameters are going to be changed by another function
    nugget<-valuenugget
    r1<-valuerange1
    an1<-valuean1

    labelnugget <- sprintf("nugget: %s", valuenugget)
    labelrange1 <- sprintf("range: %s", valuerange1)
    labelan1 <- sprintf("anisotropy: %s", valuean1)

    tkconfigure(win1$env$labelnugget, text = labelnugget)
    tkconfigure(win1$env$labelrange1, text = labelrange1)
    tkconfigure(win1$env$labelan1, text = labelan1)

    model1Choice <- models[as.numeric(tkcurselection(win1$env$model1)) + 1]


    if (identical(model1Choice, "gaussian")){
      model<-model.create(ndim=2,nvar=1,vartype=4,sill=1-nugget,aniso.coeffs=c(1,an1),range=r1)
      model<-model.create(ndim=2,nvar=1,vartype=1,sill=nugget,model = model)
    }
    if (identical(model1Choice, "cubic")){
      model<-model.create(ndim=2,nvar=1,vartype=5,sill=1-nugget,aniso.coeffs=c(1,an1),range=r1)
      model<-model.create(ndim=2,nvar=1,vartype=1,sill=nugget,model = model)
    }
    if (identical(model1Choice, "exponential")){
      model<-model.create(ndim=2,nvar=1,vartype=2,sill=1-nugget,aniso.coeffs=c(1,an1),range=r1)
      model<-model.create(ndim=2,nvar=1,vartype=1,sill=nugget,model = model)
    }
    if (identical(model1Choice, "spherical")){
      model<-model.create(ndim=2,nvar=1,vartype=3,sill=1-nugget,aniso.coeffs=c(1,an1),range=r1)
      model<-model.create(ndim=2,nvar=1,vartype=1,sill=nugget,model = model)
    }

    seed=sample(1:100,1)

    grid<-db.create(flag.grid=TRUE,x0=c(0,0),nx=c(100,100),dx=c(0.01,0.01))
    grid<-simtub(dbout=grid,model=model,seed=seed,nbsimu=1,nbtuba = 1000)
    plot(grid)
    lag<-(1:40)/10.
    windows()
    expvario<-vario.grid(grid,nlag = 30)
    plot(expvario)
    print(expvario)
  }

  tkgrid(win1$env$labelnugget, padx = 0.1, pady = c(10, 5))
  win1$env$slidernugget <- tk2scale(win1, from = 0., to = 1.,
                                    variable = sliderValuenugget, orient = "horizontal", length = 500,
                                    command = onChange)
  tkgrid(win1$env$slidernugget, padx = 0.1, pady = c(5, 10))

  tkgrid(win1$env$labelrange1, padx = 0.1, pady = c(10, 5))
  win1$env$sliderrange1 <- tk2scale(win1, from = 0.01, to = 2,
                                    variable = sliderValuerange1, orient = "horizontal", length = 500,
                                    command = onChange)
  tkgrid(win1$env$sliderrange1, padx = 0.1, pady = c(5, 10))

  tkgrid(win1$env$labelan1, padx = 0.1, pady = c(10, 5))
  win1$env$slideran1 <- tk2scale(win1, from = 0.1, to = 10,
                                 variable = sliderValuean1, orient = "horizontal", length = 500,
                                 command = onChange)
  tkgrid(win1$env$slideran1, padx = 0.1, pady = c(5, 10))

  win1$env$butOK <-tk2button(win1, text = "Simulation", width = -6, command = simulation)
  tkgrid(win1$env$butOK, padx = 10, pady = c(5, 10))
}
