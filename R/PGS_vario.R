# simulate a facies model based on PGS algorithm

library(RGeostats)
library(tcltk2)

PGS_vario<-function(){

  #sliders
  win1 <- tktoplevel()
  tktitle(win1) <- "Infering of the model"
  sliderValuerange1 <- tclVar("0.5")
  sliderValuean1 <- tclVar("1.")
  sliderValuerange2 <- tclVar("0.5")
  sliderValuean2 <- tclVar("1.")

  win1$env$labelrange1 <- tk2label(win1,
                                   text = "range of Y1: 0.5")
  win1$env$labelan1 <- tk2label(win1,
                                text = "anisotropy of Y1: 0.5")
  win1$env$labelrange2 <- tk2label(win1,
                                   text = "range of Y2: 0.5")
  win1$env$labelan2 <- tk2label(win1,
                                text = "anisotropy of Y2: 0.5")

  # scroll
  win1$env$method <- tk2listbox(win1, height = 4, selectmode = "single")
  tkgrid(tk2label(win1, text = "What type of method ?", justify = "left"),
         padx = 10, pady =c(15, 5), sticky = "w")
  tkgrid(win1$env$method, padx = 10, pady = c(5, 10))

  win1$env$model1 <- tk2listbox(win1, height = 4, selectmode = "single")
  tkgrid(tk2label(win1, text = "What type of model for Y1 ?", justify = "left"),
         padx = 10, pady =c(15, 5), sticky = "w")
  tkgrid(win1$env$model1, padx = 10, pady = c(5, 10))

  win1$env$model2 <- tk2listbox(win1, height = 4, selectmode = "single")
  tkgrid(tk2label(win1, text = "What type of model for Y2 ?", justify = "left"),
         padx = 10, pady =c(15, 5), sticky = "w")
  tkgrid(win1$env$model2, padx = 10, pady = c(5, 10))

  models <- c("gaussian", "cubic","exponential","spherical")
  for (model in models){
    tkinsert(win1$env$model1, "end", model)
    tkinsert(win1$env$model2, "end", model)
  }
  tkselection.set(win1$env$model1, 2)
  tkselection.set(win1$env$model2, 2)

  methods <- c("TGS", "PGS")
  for (method in methods){
    tkinsert(win1$env$method, "end", method)
  }
  tkselection.set(win1$env$method, 2)

  onChange <- function(...) {
    #sliders
    valuerange1 <- floor(as.double(tclvalue(sliderValuerange1))*100)/100.
    valuean1 <- floor(as.double(tclvalue(sliderValuean1))*10)/10.

    # the value parameters are going to be changed by another function
    r1<-valuerange1
    an1<-valuean1

    labelrange1 <- sprintf("range of Y1: %s", valuerange1)
    labelan1 <- sprintf("anisotropy of Y1: %s", valuean1)

    tkconfigure(win1$env$labelrange1, text = labelrange1)
    tkconfigure(win1$env$labelan1, text = labelan1)

    model1Choice <- models[as.numeric(tkcurselection(win1$env$model1)) + 1]

    if (identical(model1Choice, "gaussian")){
      curve( 1-exp(-( x/ (r1*sqrt(3) ) )^2) )
    }

    if (identical(model1Choice, "cubic")){
      curve( 7*((x/r1)^2)-(35/4)*((x/r1)^3)+(7/2)*((x/r1)^5)-(3/4)*((x/r1)^7),from=0,to=r1,xlim=c(0,1) )
    }

    if (identical(model1Choice, "exponential")){
      curve( 1-exp(-x/r1) )
    }

    if (identical(model1Choice, "spherical")){
      curve( (3*x/(2*r1) )-( (x^3)/ (2*r1^3)),from=0,to=r1,xlim=c(0,1))
    }
  }

  simulation <- function(...){
    #sliders
    valuerange1 <- floor(as.double(tclvalue(sliderValuerange1))*100)/100.
    valuean1 <- floor(as.double(tclvalue(sliderValuean1))*10)/10.
    valuerange2 <- floor(as.double(tclvalue(sliderValuerange2))*100)/100.
    valuean2 <- floor(as.double(tclvalue(sliderValuean2))*10)/10.

    # the value parameters are going to be changed by another function
    r1<-valuerange1
    an1<-valuean1
    r2<-valuerange2
    an2<-valuean2

    labelrange1 <- sprintf("range of Y1: %s", valuerange1)
    labelan1 <- sprintf("anisotropy of Y1: %s", valuean1)
    labelrange2 <- sprintf("range of Y2: %s", valuerange2)
    labelan2 <- sprintf("anisotropy of Y2: %s", valuean2)

    tkconfigure(win1$env$labelrange1, text = labelrange1)
    tkconfigure(win1$env$labelan1, text = labelan1)
    tkconfigure(win1$env$labelrange2, text = labelrange2)
    tkconfigure(win1$env$labelan2, text = labelan2)

    model1Choice <- models[as.numeric(tkcurselection(win1$env$model1)) + 1]
    model2Choice <- models[as.numeric(tkcurselection(win1$env$model2)) + 1]
    methodChoice <-methods[as.numeric(tkcurselection(win1$env$method)) + 1]


    if (identical(model1Choice, "gaussian")){
      model<-model.create(ndim=2,nvar=1,vartype=4,aniso.coeffs=c(1,an1),range=r1)
    }
    if (identical(model1Choice, "cubic")){
      model<-model.create(ndim=2,nvar=1,vartype=5,aniso.coeffs=c(1,an1),range=r1)
    }
    if (identical(model1Choice, "exponential")){
      model<-model.create(ndim=2,nvar=1,vartype=2,aniso.coeffs=c(1,an1),range=r1)
    }
    if (identical(model1Choice, "spherical")){
      model<-model.create(ndim=2,nvar=1,vartype=3,aniso.coeffs=c(1,an1),range=r1)
    }

    if (identical(model2Choice, "gaussian")){
      model2<-model.create(ndim=2,nvar=1,vartype=4,aniso.coeffs=c(1,an2),range=r2)
    }
    if (identical(model2Choice, "cubic")){
      model2<-model.create(ndim=2,nvar=1,vartype=5,aniso.coeffs=c(1,an2),range=r2)
    }
    if (identical(model2Choice, "exponential")){
      model2<-model.create(ndim=2,nvar=1,vartype=2,aniso.coeffs=c(1,an2),range=r2)
    }
    if (identical(model2Choice, "spherical")){
      model2<-model.create(ndim=2,nvar=1,vartype=3,aniso.coeffs=c(1,an2),range=r2)
    }

    seed=sample(1:100,1)

    grid<-db.create(flag.grid=TRUE,x0=c(0,0),nx=c(100,100),dx=c(0.01,0.01))
    facies<- rep(3,length=length(grid[,1])) # initialization with facies 3

    if(identical(methodChoice, "TGS")){
      grid<-simtub(dbout=grid,model=model,seed=seed,nbsimu=1,nbtuba = 1000)
      facies[grid[,4]<0]<-1
      facies[(grid[,4]>0)&(grid[,4]<qnorm(0.75))]<-2

    }
    if(identical(methodChoice, "PGS")){
      grid<-simtub(dbout=grid,model=model,seed=seed,nbsimu=1,nbtuba = 1000)
      grid<-simtub(dbout=grid,model=model2,seed=seed,nbsimu=1,nbtuba = 1000)
      facies[grid[,4]<0]<-1
      facies[(grid[,4]>0)&(grid[,5]>0)]<-2
    }

    grid<-db.add(grid,facies)
    plot(grid)
  }

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

  tkgrid(win1$env$labelrange2, padx = 0.1, pady = c(10, 5))
  win1$env$sliderrange2 <- tk2scale(win1, from = 0.01, to = 2,
                                    variable = sliderValuerange2, orient = "horizontal", length = 500,
                                    command = onChange)
  tkgrid(win1$env$sliderrange2, padx = 0.1, pady = c(5, 10))

  tkgrid(win1$env$labelan2, padx = 0.1, pady = c(10, 5))
  win1$env$slideran2 <- tk2scale(win1, from = 0.1, to = 10,
                                 variable = sliderValuean2, orient = "horizontal", length = 500,
                                 command = onChange)
  tkgrid(win1$env$slideran2, padx = 0.1, pady = c(5, 10))

  win1$env$butOK <-tk2button(win1, text = "Simulation", width = -6, command = simulation)
  tkgrid(win1$env$butOK, padx = 10, pady = c(5, 10))
}
