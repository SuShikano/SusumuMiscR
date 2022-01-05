# ##############################################################################
#' Investigate the feeling thermometer and LR placement
#'
#' @param skalo Matrix of thermometer scores
#' @param lr Matrix of left-right placement of parties (and leaders)
#' @param self.lr Vector of left-right self-placement
#' @param distance A numeric value, which determines the distance metric.
#'         1 (deafult) is the straight distance,
#'         2 is the squared distance.
#' @param party.label A vector inclunding the party label. Default is NULL.
#' @param output.type determines the type of output. Possible values are:
#'        "table" for the table of estaimted coefficients.
#'        "estimates" for objects.
#'        "weight" for the individual weight of lr distance as vector.
#'        "data" for the long-format data as matrix.
#' @return Depending on the specified type of output, the function
#'         returns various type of object.
# ---------------------------------------------------------------------------- #
#
# Function to estimate the effect of left right scale on the feeling thermometer
#

skalo.lr <- function(skalo,
                     lr,
                     self.lr,
                     distance=1,
                     party.label=NULL,
                     output.type="table"){

    library(lme4)
    # some checks
    if (ncol(skalo) != ncol(lr)){
      stop("The number of columns of thermometer scores and lr-placement differs.")
    }
    if (nrow(skalo) != nrow(lr)){
      stop("The number of observations of thermometer scores and lr-placement differs.")
    }
    if (nrow(skalo) != length(self.lr)){
      stop("The number of observations of thermometer scores and lr-self-placement differs.")
    }

    if (!is.null(party.label)){
       if (ncol(skalo) != length(party.label)){
         stop("Length of the label does not match the number of parties.")
       }
    }

  if (distance!=1 & distance!=2){
      stop("Distance must be either 1 or 2.")
  }

  # Making distance
    lr.dist <- abs(lr - self.lr)
    lr.dist <- lr.dist^distance
    # Making directional utility
    lr.dire <- (lr-6)*(self.lr-6)

    long.skalo <- c(as.matrix(skalo))

    long.lr.dist <- c(as.matrix(lr.dist))
    long.lr.dire <- c(as.matrix(lr.dire))
    long.party <- rep(1:dim(skalo)[2],each=dim(skalo)[1])
    long.id <- rep(1:dim(skalo)[1],dim(skalo)[2])
    #long.party.comp <- c(party.comp)

    valid.cases <- ifelse(!is.na(long.lr.dist)&!is.na(long.skalo),1,0)
    valid.cases.parties <- ifelse(valid.cases==1&long.party<=8,1,0)

    # if only data is needed
    if (output.type=="data"){
      long.data <- cbind(long.skalo,long.lr.dist,
                         long.id,long.party)
      colnames(long.data) <- c("skalo","lr.dist",
                             "resp","party")
      all.outputs <- long.data
    }else{
    # --------------------------------------------------------------------------
    # ESTIMATION
    # --------------------------------------------------------------------------

    # party-wise regression
    #party.coef <- NULL
    #for (i in 1:dim(skalo)[2]){
    #  this.result <- lm(long.skalo[long.party==i] ~ long.lr.dist[long.party==i])
    #  party.coef <- cbind(party.coef,c(coef(this.result) , summary(this.result)$r.squared))
    #}
    #colnames(party.coef) <- party.label

    # multi-level regression models
    # Fixed Model: no multi-level structure
    m.f1  <- glm(long.skalo ~ long.lr.dist,family=gaussian)
    summary(m.f1)
    var(long.skalo[!is.na(long.lr.dist)],na.rm=T)
    var(m.f1$residuals)

    # Fixed model:
    m.f2  <- glm(long.skalo ~ long.lr.dist+ as.factor(long.party)- 1,
                 family=gaussian)
    summary(m.f2)
    var(m.f2$residuals)

    # Mixed Model: individual-level intercept and slopes
    m.r1 <- lmer(long.skalo ~  (1+ long.lr.dist|long.id) )
    summary(m.r1)
    summary(m.r1)$AICtab

    m.r2 <- lmer(long.skalo ~  (1+ long.lr.dist|long.id) +
                   as.factor(long.party)- 1)
    summary(m.r2)
    summary(m.r2)$AICtab

    # Mixed Model: individual-level slopes
    m.r3 <- lmer(long.skalo ~  long.lr.dist +
                   (0+ long.lr.dist|long.id) +
                   as.factor(long.party)- 1)
    summary(m.r3)
    summary(m.r3)$AICtab


    m.r2.dir <- lmer(long.skalo ~  (1+ long.lr.dire|long.id) + as.factor(long.party) -1)
    summary(m.r2.dir)
    summary(m.r2.dir)$AICtab

    # Mixed Model: ordinal dependent variable
    #m.r2.ord <- clm(as.factor(long.skalo) ~   long.lr.dist*as.factor(long.id) + as.factor(long.party) ,maxIter=1000)
    #m.r2.ord <- clmm(as.factor(long.skalo) ~  (1+ long.lr.dist|long.id) + as.factor(long.party),control=clmm.control(grtol=1e-2) ,maxIter=1000)
    #summary(m.r2.ord)
    #summary(m.r2.ord)$AICtab


    # --------------------------------------------------------------------------------------
    ## output table
    if (output.type=="table"){
      rownames(summary(m.f1)$coefficients)
      rownames(summary(m.f2)$coefficients)
      o1 <- match(c("(Intercept)",rownames(summary(m.f2)$coefficients)),rownames(summary(m.f1)$coefficients))
      rownames(summary(m.r1)$coefficients)
      o3 <- match(c("(Intercept)",rownames(summary(m.f2)$coefficients)),rownames(summary(m.r1)$coefficients))
      rownames(summary(m.r2)$coefficients)
      o4 <- match(c("(Intercept)",rownames(summary(m.f2)$coefficients)),rownames(summary(m.r2)$coefficients))
      rownames(summary(m.r3)$coefficients)
      o5 <- match(c("(Intercept)",rownames(summary(m.f2)$coefficients)),rownames(summary(m.r3)$coefficients))

      coef.tab <- cbind(summary(m.f1)$coefficients[o1,1:2],
                        rbind(NA,summary(m.f2)$coefficients[,1:2]),
                        summary(m.r1)$coefficients[o3,1:2],
                        summary(m.r2)$coefficients[o4,1:2],
                        summary(m.r3)$coefficients[o5,1:2],
                        summary(m.r2.dir)$coefficients[o4,1:2])
      rownames(coef.tab)[2] <- "LR Distance"
      rownames(coef.tab)[3:nrow(coef.tab)] <- party.label

      var.tab <- cbind(NA,NA,NA,NA,
                       c(diag(summary(m.r1)$varcor$long.id),sqrt(summary(m.r1)$sigma)),NA,
                       c(diag(summary(m.r2)$varcor$long.id),sqrt(summary(m.r2)$sigma)),NA,
                       c(NA,diag(summary(m.r3)$varcor$long.id),sqrt(summary(m.r3)$sigma)),NA,
                       c(diag(summary(m.r2.dir)$varcor$long.id),sqrt(summary(m.r2.dir)$sigma)),NA)
      rownames(var.tab)<- c("VAR: Intercept","VAR: LR Distance","VAR: residuals")
      sd.tab <- sqrt(var.tab)
      rownames(sd.tab)<- c("SD: Intercept","SD: LR Distance","SD: residuals")

      AIC <- c(summary(m.f1)$aic,NA,
               summary(m.f2)$aic,NA,
               summary(m.r1)$AICtab,NA,
               summary(m.r2)$AICtab,NA,
               summary(m.r3)$AICtab,NA,
               summary(m.r2.dir)$AICtab,NA)

      multi.output <- rbind(coef.tab,sd.tab,AIC)

      all.outputs <- multi.output
    }

    if (output.type=="estimates"){
      all.outputs <- list(m.f1=m.f1,m.f2=m.f2,m.r1=m.r1,m.r2=m.r2,m.r3=m.r3)
    }

    if (output.type=="weight"){

      ind.slope <- unlist(ranef(m.r3))
      fixed.effects <- fixef(m.r3)
      mean.slope <- fixed.effects[names(fixed.effects)=="long.lr.dist"]
      ind.slope <- ind.slope + mean.slope

      names(ind.slope) <- NULL

      weight.dat <- cbind(skalo,NA)
      num.nonmissing <- ifelse(!is.na(long.lr.dist) & !is.na(long.skalo),1,0)
      num.nonmissing <- tapply(num.nonmissing,as.factor(long.id),sum)

      weight.dat[num.nonmissing>0,ncol(weight.dat)] <- ind.slope

      colnames(weight.dat)[ncol(weight.dat)] <- "lr.weight"

      all.outputs <- weight.dat
    }
    }# end of output.type!="data"

    all.outputs
}













