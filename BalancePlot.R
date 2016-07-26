# Note: I developed this function from code created by Rocio Titiunik, Devin Caughey, and Jas Sekhon.

BalancePlot=function(Data, Treat, Covariates, Names.To.Print, Shade.Color = "blanchedalmond", 
                     Title = "Balance Plot", Title.Size = 1.2, na.rm = FALSE, 
                     Built.In.Tests = c("T.Test"), Point.Color = "red", Other.Tests = NULL, 
                     pch = NULL, Year.Covariates = NULL, Different.Test=NULL, Observational.Data = NULL, 
                     Observational.Treat = NULL, Observational.Point.Color = "blue", 
                     Sample.Name = "Sub-Sample", O.Name = "All Units", Legend = FALSE, 
                     Paired = FALSE, Observational.Paired = FALSE) 
{
    if (length(Treat) == 1) {
        Treat = Data[, Treat]
    }
    if (length(Observational.Treat) == 1) {
        Observational.Treat = Observational.Data[, Treat]
    }
    if (na.rm == FALSE) {
        if (any(is.na(cbind(Data[, Covariates], Treat))) == TRUE) {
            return("NAs Detected. Must set na.rm=TRUE")
        }
    }
    mar = par()$mar
    op = par(mar = c(3, 432/28+5/28 * length(Covariates), 0, 1))
    t = Data[Treat == 1, ]
    o.t = Observational.Data[Observational.Treat == 1, ]
    c = Data[Treat == 0, ]
    o.c = Observational.Data[Observational.Treat == 0, ]
    sample = rbind(t, c)
    o.sample = rbind(o.t, o.c)
    options(scipen = 100, digits = 4)
    covs = cbind(Covariates, Names.To.Print)
    r = 1000
    varline = 9.5
    nline = 6
    tline = 4.5
    cline = 1
    plot(x = NULL, y = NULL, xlim = c(0, 1), ylim = c(1, nrow(covs) + 
                                                          3), ylab = "", xlab = "", xaxt = "n", yaxt = "n", bty = "n")
    mtext(text = c("Variable\nName", "Treatment\nMean", "Control\nMean"), 
          side = 2, font = 2, line = c(varline + 2, tline + 1.2, 
                                       cline + 0.53), adj = 0.5, las = 2, at = nrow(covs) + 
              1.18, cex = 0.7)
    mtext(text = Title, side = 2, font = 2, line = -3, adj = 0.6, 
          las = 2, at = nrow(covs) + 2.5, cex = Title.Size)
    for (i in 1:nrow(covs)) {
        aty = nrow(covs) - i + 1
        mtext(text = covs[i, 2], side = 2, line = varline, adj = 1, 
              las = 2, at = aty, cex = 0.7)
        if(length(Different.Test)==0){      
        meanT = signif(mean(sample[Treat == 1, covs[i, 1]], na.rm = TRUE))}
        if(length(Different.Test)==1){      
        meanT = Different.Test(Data[,covs[i,1]])[1]}
        if (covs[i, 1] %in% Year.Covariates) {
            meanT = signif(meanT, digits = 4)
        }
        if (abs(meanT) < 0.1) {
            meanT = signif(meanT, digits = 1)
        }
        if (abs(meanT) > 0.1 & abs(meanT) < 10) {
            meanT = signif(meanT, digits = 3)
        }
        if (abs(meanT) > 10 & abs(meanT) < 100) {
            meanT = signif(meanT, digits = 3)
        }
               if (abs(meanT) > 100 & abs(meanT) < 1000) {
            meanT = signif(meanT, digits = 4)
        }
        if (abs(as.numeric(meanT)) >= 0.1 & abs(as.numeric(meanT)) < 
                1 & nchar(meanT) == 3) {
            meanT = paste(meanT, sep = "")
        }
        if (meanT > 1000 & !(covs[i, 1] %in% Year.Covariates)) {
            meanT = format(meanT, digits = 2, big.mark = ",")
        }
        if(length(Different.Test)==0){  
        meanC = signif(mean(sample[Treat == 0, covs[i, 1]], na.rm = TRUE))}
               if(length(Different.Test)==1){      
        meanC = Different.Test(Data[,covs[i,1]])[2]}
        if (covs[i, 1] %in% Year.Covariates) {
            meanC = signif(meanC, digits = 4)
        }
        if (abs(meanC) < 0.1) {
            meanC = signif(meanC, digits = 1)
        }
        if (meanC > 0.1 & abs(meanC) < 10) {
            meanC = signif(meanC, digits = 3)
        }
        if (abs(meanC) > 10 & abs(meanC) < 100) {
            meanC = signif(meanC, digits = 3)
        }
                if (abs(meanC) > 100 & abs(meanC) < 1000) {
            meanC = signif(meanC, digits = 4)
        }
        if (as.numeric(meanC)%%0.1 == 0 & abs(as.numeric(meanC)) < 
                1) {
            meanC = paste(meanC, sep = "")
        }
        if (meanC > 1000 & !(covs[i, 1] %in% Year.Covariates)) {
            meanC = prettyNum(meanC, big.mark = ",")
        }
        mtext(text = c(meanT, meanC), side = 2, line = c(tline, 
                                                         cline - 0.6), adj = 1, las = 2, at = aty, cex = 0.7)
        if (aty%%2 == 1) {
            polygon(x = c(0, 0, 1, 1), y = c(aty - 0.5, aty + 
                                                 0.5, aty + 0.5, aty - 0.5), border = FALSE, col = Shade.Color)
        }
                if (length(Different.Test)==1 & length(o.sample) > 
                0) {
            p4 = Different.Test(Observational.Data[,covs[i,1]])[3]
            points(pch = 17, col = Observational.Point.Color, 
                   x = p4, y = aty)
        }
        
        if ("T.Test" %in% Built.In.Tests & length(o.sample) > 
                0) {
            if (all(c(o.t[, covs[i, 1]], o.c[, covs[i, 1]]) %in% 
                        c(0, 1)) & sum(as.numeric(c(o.t[, covs[i, 1]], 
                                                    o.c[, covs[i, 1]]))) < 10) {
                if (Paired == FALSE) 
                    p1 = Permutation.Test(o.t[, covs[i, 1]], o.c[, 
                                                                 covs[i, 1]])
                if (Paired == TRUE) 
                    p1 = Permutation.Test(o.t[, covs[i, 1]], o.c[, 
                                                                 covs[i, 1]], Paired = TRUE)
                points(pch = 18, col = Observational.Point.Color, 
                       x = p1, y = aty)
            }
            else {
                if (Observational.Paired == FALSE) 
                    p3 = t.test(o.t[, covs[i, 1]], o.c[, covs[i, 
                                                              1]])$p.value
                if (Observational.Paired == TRUE) 
                    p3 = t.test(o.t[, covs[i, 1]], o.c[, covs[i, 
                                                              1]], paired = TRUE)$p.value
                points(pch = 18, col = Observational.Point.Color, 
                       x = p3, y = aty)
            }
        }
        if ("KS.Test" %in% Built.In.Tests & length(o.sample) > 
                0) {
            p4 = ks.test(o.t[, covs[i, 1]], o.c[, covs[i, 1]])$p.value
            points(pch = 17, col = Observational.Point.Color, 
                   x = p4, y = aty)
        }
                        if (length(Different.Test)==1) {
            p4 = Different.Test(Data[,covs[i,1]])[3]
            points(pch = pch[1], col = Point.Color[1], 
                   x = p4, y = aty)
        }
        if ("T.Test" %in% Built.In.Tests) {
            if (all(c(t[, covs[i, 1]], c[, covs[i, 1]]) %in% 
                        c(0, 1)) & sum(as.numeric(c(t[, covs[i, 1]], 
                                                    c[, covs[i, 1]]))) < 10) {
                if (Paired == FALSE) 
                    p1 = Permutation.Test(t[, covs[i, 1]], c[, 
                                                             covs[i, 1]])
                if (Paired == TRUE) 
                    p1 = Permutation.Test(t[, covs[i, 1]], c[, 
                                                             covs[i, 1]], Paired = TRUE)
                points(pch = 18, col = Point.Color, x = p1, y = aty)
            }
            else {
                if (Paired == FALSE) 
                    p1 = t.test(t[, covs[i, 1]], c[, covs[i, 1]])$p.value
                if (Paired == TRUE) 
                    p1 = t.test(t[, covs[i, 1]], c[, covs[i, 1]], 
                                paired = TRUE)$p.value
                points(pch = 18, col = Point.Color, x = p1, y = aty)
            }
        }
        if ("KS.Test" %in% Built.In.Tests) {
            p2 = ks.test(t[, covs[i, 1]], c[, covs[i, 1]])$p.value
            points(pch = 17, col = Point.Color, x = p2, y = aty)
        }
        if (length(Other.Tests) > 0) {
            for (z in 1:length(Other.Tests)) {
                fun = Other.Tests[[z]]
                px = as.numeric(fun(close[,covs[i,1]]))
                points(pch = pch[(z+1)], col = Point.Color[(z+1)], x = px, 
                       y = aty)
            }
        }
    }
    segments(x0 = 0, x1 = 0, y0 = 0.49, y1 = nrow(covs) + 0.48)
    segments(x0 = 0, x1 = 1, y0 = 0.49, y1 = 0.49)
    segments(x0 = c(0.05, 0.1), x1 = c(0.05, 0.1), y0 = 0.5, 
             y1 = nrow(covs) + 0.48, lty = "dotted")
    mtext(side = 1, at = c(0, 0.05, 0.1, 1), text = c("0", ".05", 
                                                      ".1", "1"), cex = 0.7, line = -0.2 + 5/nrow(covs) - 0.01 * 
              nrow(covs))
    mtext(side = 1, line = 1.2, at = 0.5, text = "p-value")
    if (Legend == TRUE) {
        par(xpd = TRUE)
        if (length(Built.In.Tests) == 2 & length(o.sample) > 
                0) {
            legend(-1.3, 0.4, cex = 0.6, c(paste("t-test (", 
                                                 O.Name, ")", sep = ""), paste("KS-test (", O.Name, 
                                                                               ")", sep = ""), paste("t-test (", Sample.Name, 
                                                                                                     ")", sep = ""), paste("KS-test (", Sample.Name, 
                                                                                                                           ")", sep = "")), pch = c(18, 17, 18, 17), col = c(Observational.Point.Color, 
                                                                                                                                                                             Observational.Point.Color, Point.Color, Point.Color))
        }
        if (length(Different.Test) == 1) {
            legend(-0.9, 0.5, cex = 0.6, c("Local Linear (rdrobust)","Local Linear (Conventional)", "Local Polynomial")
                                                 , pch = pch, col = Point.Color) 
                                                                                                                     
        }
        if (length(Built.In.Tests) == 1 & "KS.Test" %in% Built.In.Tests & 
                length(o.sample) > 0) {
            legend(-1.3, 0.4, cex = 0.6, c(paste("KS-test (", 
                                                 O.Name, ")", sep = ""), paste("KS-test (", Sample.Name, 
                                                                               ")")), pch = c(17, 17), col = c(Observational.Point.Color, 
                                                                                                               Point.Color))
        }
        if (length(Built.In.Tests) == 2 & length(o.sample) == 
                0) {
            legend(-1.3, 0.4, cex = 0.6, c("t-test", "KS-test"), 
                   pch = c(18, 17), col = c(Point.Color, Point.Color))
        }
    }
    op = par(mar = mar)
}


RDTest=function(v){
output1=rdpointest(v,close$MinDist)
output2=as.numeric(rdrobust(v,close$MinDist,all=TRUE)[[3]][[3,4]])
return(c(output1[1],output1[2],output2))}


WarRDTest=function(v){
output1=rdpointest(v,close$WarMinDist)
output2=as.numeric(rdrobust(v,close$WarMinDist,all=TRUE)[[3]][[3,4]])
return(c(output1[1],output1[2],output2))}


# Example

# BalancePlot(Data=sample, Treat=sample$Treat, Title="Balance Between the Qualifiers and Non-qualifiers",Covariates=c("Irst",'Milex','Milper','Tpop','Upop','BirthRate','DeathRate','InfantMortality','Energy','Imports','Exports','LandArea','CINC','Democracy','GreatPower','EngagedCivilWar','EndedCivilWar','EntranceYear','SexRatio','LifeExpectancy','MedianAge','Alliances','USAlly','SoccerMostPopular','PrevAppear','AGGYearBefore','AGG3YearsBefore','AGG5YearsBefore'), Names.To.Print=c('Iron and Steel Production','Military Expenditures', 'Military Personnel', 'Total Population', 'Urban Population', 'Birth Rate', 'Death Rate', 'Infant Mortality', 'Energy Production', 'Imports', 'Exports', 'Land Area', 'Material Power Score', 'Level of Democracy', 'Great Power Status', 'Engaged in Civil War', 'Resolved Civil War', 'Year of State Formation', 'Sex Ratio', 'Life Expectancy', 'Median Age', "Number of Alliances", "U.S. Ally", "Soccer Most Popular Sport", 'Appearance at Previous World Cup',  'MIDs Initiated in the Year Before', 'MIDs Initiated in the 3 Years Before', 'MIDs Initiated in the 5 Years Before'), Shade.Color="cadetblue2", na.rm=FALSE, Built.In.Tests=c("T.Test","KS.Test"), Point.Color="black", Sample.Name="RD Sample", Year.Covariates=c("EntranceYear"), Observational.Data=data, Observational.Treat=data$Treat, Observational.Point.Color="gray90", Legend=TRUE, Paired=TRUE, Observational.Paired=TRUE)




# Note: I developed this function from code created by Rocio Titiunik, Devin Caughey, and Jas Sekhon.

BalancePlot2=function(Data, Treat, Covariates, Names.To.Print, Shade.Color = "blanchedalmond", 
                     Title = "Balance Plot", Title.Size = 1.2, na.rm = FALSE, 
                     Built.In.Tests = c("T.Test"), Point.Color = "red", Other.Tests = NULL, 
                     pch = NULL, Year.Covariates = NULL, Different.Test=NULL, Observational.Data = NULL, 
                     Observational.Treat = NULL, Observational.Point.Color = "blue", 
                     Sample.Name = "Sub-Sample", O.Name = "All Units", Legend = FALSE, 
                     Paired = FALSE, Observational.Paired = FALSE) 
{
    if (length(Treat) == 1) {
        Treat = Data[, Treat]
    }
    if (length(Observational.Treat) == 1) {
        Observational.Treat = Observational.Data[, Treat]
    }
    if (na.rm == FALSE) {
        if (any(is.na(cbind(Data[, Covariates], Treat))) == TRUE) {
            return("NAs Detected. Must set na.rm=TRUE")
        }
    }
    mar = par()$mar
    op = par(mar = c(3, 432/28+5/28 * length(Covariates), 0, 1))
    t = Data[Treat == 1, ]
    o.t = Observational.Data[Observational.Treat == 1, ]
    c = Data[Treat == 0, ]
    o.c = Observational.Data[Observational.Treat == 0, ]
    sample = rbind(t, c)
    o.sample = rbind(o.t, o.c)
    options(scipen = 100, digits = 4)
    covs = cbind(Covariates, Names.To.Print)
    r = 1000
    varline = 9.5
    nline = 6
    tline = 4.5
    cline = 1
    plot(x = NULL, y = NULL, xlim = c(0, 1), ylim = c(1, nrow(covs) + 
                                                          3), ylab = "", xlab = "", xaxt = "n", yaxt = "n", bty = "n")
    mtext(text = c("Variable\nName", "Treatment\nMean", "Control\nMean"), 
          side = 2, font = 2, line = c(varline + 2, tline + 1.2, 
                                       cline + 0.53), adj = 0.5, las = 2, at = nrow(covs) + 
              1.18, cex = 0.7)
    mtext(text = Title, side = 2, font = 2, line = -3, adj = 0.6, 
          las = 2, at = nrow(covs) + 2, cex = Title.Size)
    for (i in 1:nrow(covs)) {
        aty = nrow(covs) - i + 1
        mtext(text = covs[i, 2], side = 2, line = varline, adj = 1, 
              las = 2, at = aty, cex = 0.7)
        if(length(Different.Test)==0){      
        meanT = signif(mean(sample[Treat == 1, covs[i, 1]], na.rm = TRUE))}
        if(length(Different.Test)==1){      
        meanT = Different.Test(Data[,covs[i,1]])[1]}
        if (covs[i, 1] %in% Year.Covariates) {
            meanT = signif(meanT, digits = 4)
        }
        if (abs(meanT) < 0.1) {
            meanT = signif(meanT, digits = 1)
        }
        if (abs(meanT) > 0.1 & abs(meanT) < 10) {
            meanT = signif(meanT, digits = 3)
        }
        if (abs(meanT) > 10 & abs(meanT) < 100) {
            meanT = signif(meanT, digits = 3)
        }
               if (abs(meanT) > 100 & abs(meanT) < 1000) {
            meanT = signif(meanT, digits = 4)
        }
        if (abs(as.numeric(meanT)) >= 0.1 & abs(as.numeric(meanT)) < 
                1 & nchar(meanT) == 3) {
            meanT = paste(meanT, sep = "")
        }
        if (meanT > 1000 & !(covs[i, 1] %in% Year.Covariates)) {
            meanT = format(meanT, digits = 2, big.mark = ",")
        }
        if(length(Different.Test)==0){  
        meanC = signif(mean(sample[Treat == 0, covs[i, 1]], na.rm = TRUE))}
               if(length(Different.Test)==1){      
        meanC = Different.Test(Data[,covs[i,1]])[2]}
        if (covs[i, 1] %in% Year.Covariates) {
            meanC = signif(meanC, digits = 4)
        }
        if (abs(meanC) < 0.1) {
            meanC = signif(meanC, digits = 1)
        }
        if (meanC > 0.1 & abs(meanC) < 10) {
            meanC = signif(meanC, digits = 3)
        }
        if (abs(meanC) > 10 & abs(meanC) < 100) {
            meanC = signif(meanC, digits = 3)
        }
                if (abs(meanC) > 100 & abs(meanC) < 1000) {
            meanC = signif(meanC, digits = 4)
        }
        if (as.numeric(meanC)%%0.1 == 0 & abs(as.numeric(meanC)) < 
                1) {
            meanC = paste(meanC, sep = "")
        }
        if (meanC > 1000 & !(covs[i, 1] %in% Year.Covariates)) {
            meanC = prettyNum(meanC, big.mark = ",")
        }
        mtext(text = c(meanT, meanC), side = 2, line = c(tline, 
                                                         cline - 0.6), adj = 1, las = 2, at = aty, cex = 0.7)
        if (aty%%2 == 1) {
            polygon(x = c(0, 0, 1, 1), y = c(aty - 0.5, aty + 
                                                 0.5, aty + 0.5, aty - 0.5), border = FALSE, col = Shade.Color)
        }
                if (length(Different.Test)==1 & length(o.sample) > 
                0) {
            p4 = Different.Test(Observational.Data[,covs[i,1]])[3]
            points(pch = 17, col = Observational.Point.Color, 
                   x = p4, y = aty)
        }
        
        if ("T.Test" %in% Built.In.Tests & length(o.sample) > 
                0) {
            if (all(c(o.t[, covs[i, 1]], o.c[, covs[i, 1]]) %in% 
                        c(0, 1)) & sum(as.numeric(c(o.t[, covs[i, 1]], 
                                                    o.c[, covs[i, 1]]))) < 10) {
                if (Paired == FALSE) 
                    p1 = Permutation.Test(o.t[, covs[i, 1]], o.c[, 
                                                                 covs[i, 1]])
                if (Paired == TRUE) 
                    p1 = Permutation.Test(o.t[, covs[i, 1]], o.c[, 
                                                                 covs[i, 1]], Paired = TRUE)
                points(pch = 18, col = Observational.Point.Color, 
                       x = p1, y = aty)
            }
            else {
                if (Observational.Paired == FALSE) 
                    p3 = t.test(o.t[, covs[i, 1]], o.c[, covs[i, 
                                                              1]])$p.value
                if (Observational.Paired == TRUE) 
                    p3 = t.test(o.t[, covs[i, 1]], o.c[, covs[i, 
                                                              1]], paired = TRUE)$p.value
                points(pch = 18, col = Observational.Point.Color, 
                       x = p3, y = aty)
            }
        }
        if ("KS.Test" %in% Built.In.Tests & length(o.sample) > 
                0) {
            p4 = ks.test(o.t[, covs[i, 1]], o.c[, covs[i, 1]])$p.value
            points(pch = 17, col = Observational.Point.Color, 
                   x = p4, y = aty)
        }
                        if (length(Different.Test)==1) {
            p4 = Different.Test(Data[,covs[i,1]])[3]
            points(pch = pch, col = Point.Color, 
                   x = p4, y = aty)
        }
        if ("T.Test" %in% Built.In.Tests) {
            if (all(c(t[, covs[i, 1]], c[, covs[i, 1]]) %in% 
                        c(0, 1)) & sum(as.numeric(c(t[, covs[i, 1]], 
                                                    c[, covs[i, 1]]))) < 10) {
                if (Paired == FALSE) 
                    p1 = Permutation.Test(t[, covs[i, 1]], c[, 
                                                             covs[i, 1]])
                if (Paired == TRUE) 
                    p1 = Permutation.Test(t[, covs[i, 1]], c[, 
                                                             covs[i, 1]], Paired = TRUE)
                points(pch = 18, col = Point.Color, x = p1, y = aty)
            }
            else {
                if (Paired == FALSE) 
                    p1 = t.test(t[, covs[i, 1]], c[, covs[i, 1]])$p.value
                if (Paired == TRUE) 
                    p1 = t.test(t[, covs[i, 1]], c[, covs[i, 1]], 
                                paired = TRUE)$p.value
                points(pch = 18, col = Point.Color, x = p1, y = aty)
            }
        }
        if ("KS.Test" %in% Built.In.Tests) {
            p2 = ks.test(t[, covs[i, 1]], c[, covs[i, 1]])$p.value
            points(pch = 17, col = Point.Color, x = p2, y = aty)
        }
        if (length(Other.Tests) > 0) {
            if (length(o.sample) > 0) {
                for (j in 1:length(Other.Tests)) {
                    fun = Other.Tests[[j]]
                    px = as.numeric(fun(covs[i, 1]))
                    points(pch = pch[j], col = Observational.Point.Color, 
                           x = px, y = aty)
                }}
            for (z in 1:length(Other.Tests)) {
                fun = Other.Tests[[z]]
                px = as.numeric(fun(covs[i,1]))
                points(pch = pch[z], col = Point.Color, x = px, 
                       y = aty)
            }
        }
    }
    segments(x0 = 0, x1 = 0, y0 = 0.49, y1 = nrow(covs) + 0.48)
    segments(x0 = 0, x1 = 1, y0 = 0.49, y1 = 0.49)
    segments(x0 = c(0.05, 0.1), x1 = c(0.05, 0.1), y0 = 0.5, 
             y1 = nrow(covs) + 0.48, lty = "dotted")
    mtext(side = 1, at = c(0, 0.05, 0.1, 1), text = c("0", ".05", 
                                                      ".1", "1"), cex = 0.7, line = -0.2 + 5/nrow(covs) - 0.01 * 
              nrow(covs))
    mtext(side = 1, line = 1.2, at = 0.5, text = "p-value")
    if (Legend == TRUE) {
        par(xpd = TRUE)
        if (length(Built.In.Tests) == 2 & length(o.sample) > 
                0) {
            legend(-1.3, 0.4, cex = 0.6, c(paste("t-test (", 
                                                 O.Name, ")", sep = ""), paste("KS-test (", O.Name, 
                                                                               ")", sep = ""), paste("t-test (", Sample.Name, 
                                                                                                     ")", sep = ""), paste("KS-test (", Sample.Name, 
                                                                                                                           ")", sep = "")), pch = c(18, 17, 18, 17), col = c(Observational.Point.Color, 
                                                                                                                                                                             Observational.Point.Color, Point.Color, Point.Color))
        }
        if (length(Built.In.Tests) == 1 & "T.Test" %in% Built.In.Tests & 
                length(o.sample) > 0) {
            legend(-1.3, 0.4, cex = 0.6, c(paste("t-test (", 
                                                 O.Name, ")", sep = ""), paste("t-test (", Sample.Name, 
                                                                               ")", sep = "")), pch = c(18, 18), col = c(Observational.Point.Color, 
                                                                                                                         Point.Color))
        }
        if (length(Built.In.Tests) == 1 & "KS.Test" %in% Built.In.Tests & 
                length(o.sample) > 0) {
            legend(-1.3, 0.4, cex = 0.6, c(paste("KS-test (", 
                                                 O.Name, ")", sep = ""), paste("KS-test (", Sample.Name, 
                                                                               ")")), pch = c(17, 17), col = c(Observational.Point.Color, 
                                                                                                               Point.Color))
        }
        if (length(Built.In.Tests) == 2 & length(o.sample) == 
                0) {
            legend(-1.3, 0.4, cex = 0.6, c("t-test", "KS-test"), 
                   pch = c(18, 17), col = c(Point.Color, Point.Color))
        }
    }
    op = par(mar = mar)
}


RDTest=function(v){
output1=rdpointest(v,close$MinDist)
output2=as.numeric(rdrobust(v,close$MinDist,all=TRUE)[[3]][[3,4]])
return(c(output1[1],output1[2],output2))}

Conventional=function(v){
output2=as.numeric(rdrobust(v,close$MinDist,all=TRUE)[[3]][1,4])
return(output2)}



Polynomial2=function(v){
close2=close[abs(close$MinDist)<=0.0183,]
close2$United2=close2$United^2
output=summary(lm(v[abs(close$MinDist)<=0.0183]~close2$United+close2$MinDist+I(close2$MinDist*close2$United)+I(close2$MinDist*close2$United2)))$coefficients[2,4]
return(output)}



Polynomial3=function(v){
close2=close[abs(close$MinDist)<=0.0183,]
close2$United2=close2$United^2
close2$United3=close2$United^3
output=summary(lm(v[abs(close$MinDist)<=0.0183]~close2$United+close2$MinDist+I(close2$MinDist*close2$United)+I(close2$MinDist*close2$United2)+I(close2$MinDist*close2$United3)))$coefficients[2,4]
return(output)}






WarRDTest=function(v){
output1=rdpointest(v,close$WarMinDist)
output2=as.numeric(rdrobust(v,close$WarMinDist,all=TRUE)[[3]][[3,4]])
return(c(output1[1],output1[2],output2))}



