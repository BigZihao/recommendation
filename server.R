
library(magrittr)
library(shiny)
library(dplyr)
library(ggplot2)
library(gam)
library(car)
library(mgcv)
library(splines)
library(plotly)
library(rbokeh)
library(effects)
library(reshape2)

# Define server logic required to draw a histogram

function(input, output){



    output$plot1 <-  renderPlot({

      somesome= final6 %>% filter(Product_Code== input$Prod32) %>% group_by(YTD,CHANNEL) %>%
        summarise(
          Error=sum(Error),
          AllCompetitors=sum(AllCompetitorVolume),
          mediaVolume=sum(mediaVolume),
          priceVolume=sum(priceVolume),
          distributionVolume=sum(distributionVolume),
          tradeVolume=sum(tradeVolume),
          Volume=sum(Y)
        )
      somesome=data.frame(somesome) %>% reshape2::melt(id=c("CHANNEL","YTD"))
      some1=NULL
      for(l in levels(somesome$CHANNEL))
      {
        ss=somesome[somesome$CHANNEL==l,]
        s=data.frame("CHANNEL"=l,"variable"=ss[ss$YTD=="2016",3],"value"=(ss[ss$YTD=="2016","value"]-ss[ss$YTD=="2015","value"])/ss[ss$YTD=="2015"&ss$variable=="Volume","value"])
        some1=data.frame(rbind(some1,s))
        }
       some1$above=ifelse(some1$value<0,"positive","negative")
      some1=data.frame(some1)

    ggplot( some1, aes(x=variable, y=value,fill = above)) +
              geom_bar(stat="identity", position="identity")+coord_flip() +
              geom_text(aes(label=paste(round(value*100,1),"%",sep="")))+
              scale_y_continuous(labels = percent_format()) +
              theme(axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position="none")+
      facet_wrap(~ CHANNEL)


    }

    )



    output$plot2 <- renderPlot( {


      somesome= final6 %>% filter(Product_Code==  input$Prod32) %>% group_by(CHANNEL) %>%
        summarise(
          BaseandError=sum(Error)/sum(Y),
          AllCompetitors=sum(AllCompetitorVolume)/sum(Y),
          mediaVolume=sum(mediaVolume)/sum(Y),
          priceVolume=sum(priceVolume)/sum(Y),
          distributionVolume=sum(distributionVolume)/sum(Y),
          tradeVolume=sum(tradeVolume)/sum(Y),
          Volume=1
        )

      somesome=data.frame(somesome) %>%  reshape2::melt()
      somesome$above=ifelse(somesome$value<0,"positive","negative")
      somesome=data.frame(somesome)

      ggplot( somesome, aes(x=variable, y=value,fill = above)) +
        geom_bar(stat="identity", position="identity")+coord_flip() +
        geom_text(aes(label=paste(round(value*100,1),"%",sep="")))+
        scale_y_continuous(labels = percent_format()) +
        theme(axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position="none")+
         facet_wrap(~ CHANNEL)



    }

    )


    output$plot3 <- renderPlot( {

    final6  %>%   filter(Product_Code== input$Prod32) %>%  group_by(CHANNEL) %>% summarise(price=mean(priceElas),
                         distribution=mean(distributionElas)) %>%
      reshape2::melt() %>% ggplot(aes(CHANNEL,value)) + geom_point(aes(colour=factor(variable)))+ylab("Elasticity")

    }
    )


    output$plot4 <-   renderPlot( {
      b2=final6 %>% filter(Product_Code==  input$Prod32)
      lm1=gam(y~seasonalAveraged+ns(price,2)+trade+ns(distribution,2)+media+Geo_Code+Month-1,
              data=b2,weights=ybar)
      plot(allEffects(lm1)[2])

      }
    )


    output$plot5 <-  renderPlot( {
      b1=final6  %>%   filter(Product_Code== input$Prod32)
      lm1=gam(y~seasonalAveraged+ns(price,2)+trade+ns(distribution,2)+media+Geo_Code+Month-1,
              data=b1,weights=ybar)
      plot(allEffects(lm1)[4])
    }
    )


    output$plot6<- renderRbokeh({
      sharedName=final6[,c("CHANNEL","priceElas","distributionElas","Y",paste(input$Group2))]
      names(sharedName)=c("CHANNEL","priceElas","distributionElas","Y","Group")
      sharedName=sharedName %>% group_by(CHANNEL,Group) %>%
        summarize(price=mean(priceElas),distribution=mean(distributionElas),Y=mean(Y)) %>%
        collect

      figure(width = NULL, height = NULL,
                xlab = "Price Elasticity",
                ylab = "Distribution Elasticity",
                title = "Price and Distribution Elasticity") %>%
        ly_points(price, distribution, data = sharedName,
                  color = CHANNEL, size = scale(sqrt(Y)),
                  hover = list(Group,CHANNEL, price, distribution,Y), legend = FALSE)


    })


    output$plot7<- renderRbokeh({
      sharedName2=final6[,c("CHANNEL","priceCoef","distributionCoef","Y",paste(input$Group2))]
      names(sharedName2)=c("CHANNEL","priceCoef","distributionCoef","Y","Group")
      sharedName2=sharedName2 %>% group_by(CHANNEL,Group) %>%
        summarize(price=mean(priceCoef),distribution=mean(distributionCoef),Y=mean(Y)) %>%
        collect

      figure(width = NULL, height = NULL,
             xlab = "Price Elasticity",
             ylab = "Distribution Elasticity",
             title = "Price and Distribution Elasticity") %>%
        ly_points(price, distribution, data = sharedName2,
                  color = CHANNEL, size = scale(sqrt(Y)),
                  hover = list(Group,CHANNEL, price, distribution,Y), legend = FALSE)


    })


    output$plot8 <-  renderDygraph({


      b8=final6[,c("Date","CXVolume","MEVolume","PACVolume","NMVolume")] %>% group_by(Date) %>% summarise(CX=sum(CXVolume),
                                                                                                  ME=sum(MEVolume),
                                                                                                  PAC=sum(PACVolume),
                                                                                                  NM=sum(NMVolume))
      b8=xts(b8[,-1],order.by=as.POSIXct(b8$Date))
      dygraph(b8 ,
              ylab = "Cannibalization Volume")%>%
        dySeries("CX", label = "CX") %>%
        dySeries("ME", label = "ME") %>%
        dySeries("PAC", label = "PAC") %>%
        dySeries("NM", label = "NM") %>%
        dyOptions(stackedGraph = TRUE) %>%
        dyRangeSelector(height = 20)%>%
        dyLegend(show = "follow")


    }
    )


    output$plot10 <-  renderPlotly( {
      b10=final6[,c(paste(input$out),"Date","Prod",paste(input$Group))]
      names(b10)=c("value","Date","Prod","Group")
      b10$Date=as.POSIXct(b10$Date)
      ggplotly(b10 %>% group_by(Date,Group,Prod) %>% summarise(Value=sum( value)) %>%
        ggplot(aes(Date,Value,group=factor(Prod))) + ylab(paste(input$out))+
        geom_line(aes(colour=factor(Prod)))+facet_wrap(~Group, ncol = 3)+ scale_x_datetime(date_labels = "%y %b")
      )


    }
    )


    output$plot9 <-  renderPlot( {

      b1=final6[final6$Prod=="CX",]

      b1=b1[,c(paste(input$out),
                 paste("sizeME",input$out,sep=""),
                 paste("sizeNM",input$out,sep=""),
                 paste("sizeCL",input$out,sep=""),
                 paste("sizePC",input$out,sep=""))]
      names(b1)=c(paste("CX",input$out,sep=""),
                    "ME",
                    "NM",
                    "CL",
                    "PC")
      corrplot.mixed(cor(b1))

    }
    )


    output$plot11 <-  renderPlotly( {
      b11=final6[,c(paste(input$out),"Prod",paste(input$Group),"Month","Geo_Code","Y")]
      names(b11)=c("value","Prod","Group","Month","Geo_Code","Y")
      p=b11 %>% group_by(Group,Month,Geo_Code,Prod) %>% summarise(Y=sum(Y),value=sum(value)) %>% ggplot(aes(x=value,y=Y,
      text = paste("Product:", Prod,"Month:", Month,"Geo_Code:", Geo_Code))) +
        geom_point(aes(colour=factor(Group)))+ xlab(paste(input$out))
      ggplotly(p)

    }
    )


    output$plot12 <-  renderPlot( {

      data12=final6[,c("tradeVolume","CHANNEL","Y",paste(input$Group2))]
      names(data12)=c("tradeVolume","CHANNEL","Y","Group")

      data.frame(data12 %>% filter(CHANNEL!="LIQUOR") %>% group_by(CHANNEL,Group) %>% summarise(TradeContribution=sum(tradeVolume)/sum(Y))) %>%
        ggplot(aes(Group,TradeContribution,fill=CHANNEL))+geom_bar(stat="identity", position = "dodge")+xlab(paste(input$Group2))


    }
    )

    output$plot13 <-  renderPlot( {

      data13=final6[,c("mediaVolume","CHANNEL","Y",paste(input$Group2))]
      names(data13)=c("mediaVolume","CHANNEL","Y","Group")

    data.frame(data13 %>% group_by(CHANNEL,Group) %>% summarise(mediaContribution=sum(mediaVolume)/sum(Y))) %>%
      ggplot(aes(Group,mediaContribution,fill=CHANNEL))+geom_bar(stat="identity", position = "dodge")+xlab(paste(input$Group2))

      }
    )


    output$plot14<-renderPlot({
      ll=apply(final6[final6$Prod=="CX",c("AllCompetitorVolume","CLVolume","MEVolume","PACVolume","NMVolume")],2,sum)
      names(ll)=c("All","CL","ME","PAC","NM")
      ll=data.frame(variable=names(ll),Cannibalization=unlist(ll))
      ll$variable=factor(ll$variable,levels=c("All","ME","CL","PAC","NM"))
      ll %>% ggplot(aes(variable,Cannibalization)) +geom_bar(stat="identity", position="identity")
    })

    output$plot15<-renderPlot({


      l=apply(final6[final6$Prod=="CX",c("AllCompetitorVolume","MEVolume","sizeMEpriceVolume","sizeMEtradeVolume","sizeMEdistributionVolume","sizeMEmediaVolume",
                                          "CLVolume","sizeCLpriceVolume","sizeCLtradeVolume","sizeCLdistributionVolume","sizeCLmediaVolume",
                                          "PACVolume","sizePACpriceVolume","sizePACtradeVolume","sizePACdistributionVolume","sizePACmediaVolume",
                                          "NMVolume","sizeNMpriceVolume","sizeNMtradeVolume","sizeNMdistributionVolume","sizeNMmediaVolume"
      )],2,sum)

      names(l)=c("All","ME","ME price"," ME trade "," ME distribution "," ME media ",
                 "CL "," CL price "," CL trade "," CL distribution "," CL media ",
                 "PAC "," PAC price "," PAC trade "," PAC distribution "," PAC media ",
                 "NM "," NM price "," NM trade "," NM distribution "," NM media ")
      l=data.frame(variable=names(l),Cannibalization=unlist(l))
      l$variable=factor(l$variable,levels=c("All","ME","CL ", "PAC ","NM ","ME price"," ME trade "," ME distribution "," ME media ",
                                               " CL price "," CL trade "," CL distribution "," CL media ",
                                              " PAC price "," PAC trade "," PAC distribution "," PAC media ",
                                               " NM price "," NM trade "," NM distribution "," NM media "))

      l$above="All"
      l$above[grep("ME",l$variable)]="ME"
      l$above[grep("CL",l$variable)]="CL"
      l$above[grep("PAC",l$variable)]="PAC"
      l$above[grep("NM",l$variable)]="NM"
      l %>% ggplot(aes(variable,Cannibalization,fill=above)) +geom_bar(stat="identity", position="identity")+
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))




    })



    output$plot16<-renderPlot({

    data1=final6 %>% group_by(check,CHANNEL,Product_Code) %>% summarise(price=mean(priceCoef),distribution=mean(distributionCoef),trade=mean(tradeCoef),Comy=mean(ComyCoef))
    data1$price[data1$price>0]=NA
    data1$distribution[data1$distribution>1]=NA
    data1$trade[data1$trade<0]=NA
    data1$Comy[data1$Comy>0]=NA
    data1=data1[,c("price","distribution","trade","Comy")]
    names(data1)=c("price","distribution","trade","AllCompetitors")
    aggr_plot <- aggr(data1, col=c('#43a2ca','#a8ddb5'), numbers=TRUE, sortVars=TRUE, labels=names(data1), cex.axis=.7, gap=3, ylab=c("Histogram of Bad Coefficients","Pattern"))

    })


    output$table1 <-renderTable({

      data1= data.frame(
        variable=c("Y","Price","Distribution",
                   "TP","QM","Trade","TV","Digital","OOH","Media"),
        RawData=c("TL.VOL","TL.BVP","TL.CACV","TP.VOL",
                  "QM.VOL","QM+TP","T60+Hisp T60","I40+Hisp I40","OOH+Hisp OOH","TV+Digital+OOH"),
        Tactic=c("Total Volume Sales","Total Base Price","Total Average Weekly Total Points of Category Weighted Distribution",
                 "-","-","-","-","-","-","-"))

      data.frame(data1)
    })


    output$plot17<-renderChorddiag({

    m1=final6 %>% group_by(Prod) %>% summarise(sum(Y),sum(CLVolume),sum(CXVolume),sum(MEVolume),sum(NMVolume),sum(PACVolume))
    m2=as.matrix(data.frame(m1[,-c(1:2)]))
    diag(m2)=c(unlist(m1[,2]))
    m2=abs(m2)
    haircolors <- c("CL","CX","ME","NM","PAC")
    dimnames(m2) <- list(have = haircolors,
                         prefer = haircolors)
    chorddiag(m2, groupnamePadding = 20,showTicks = F)
     })


    output$plot18<-renderPlot({
    m=final6 %>% group_by(YTD,Prod) %>% summarise(sum(Y),sum(CLVolume),sum(CXVolume),sum(MEVolume),sum(NMVolume),sum(PACVolume))
    m=data.frame(m[m$YTD=="2016",c(1:2)],m[m$YTD=="2016",c(3:8)]-m[m$YTD=="2015",c(3:8)])
    rbind(data.frame(Prod=m[,2],variable="Sourcing Within Portfolio",value=apply(m[4:8],2,sum)),
          data.frame(Prod=m[,2],variable="Brand Growth",value=m[,3])) %>%
      ggplot(aes(x=Prod, y=value,fill=variable))+ geom_bar(stat="identity", position="identity")+coord_flip() +
      geom_text(aes(label=round(value/1000)))+ylab("Volume(K)")
    })



    output$plot19<-renderPlot({

    final6 %>% filter(Prod=="CX") %>% group_by(Product_Code) %>% summarise(CLdistribution=mean(sizeCLdistributionElas),
                                                                                MEdistribution=mean(sizeMEdistributionElas),
                                                                                NMdistribution=mean(sizeNMdistributionElas),
                                                                                PACdistribution=mean(sizePACdistributionElas)) %>%
      reshape2::melt() %>% ggplot(aes(Product_Code,value)) + geom_point(aes(colour=factor(variable)))+coord_flip()
    })


    output$plot20<-renderPlot({

      final6 %>% filter(Prod=="CX") %>% group_by(Product_Code) %>% summarise(CLprice=mean(sizeCLpriceElas),
                                                                                  MEprice=mean(sizeMEpriceElas),
                                                                                  NMprice=mean(sizeNMpriceElas),
                                                                                  PACprice=mean(sizePACpriceElas)) %>%
        reshape2::melt() %>% ggplot(aes(Product_Code,value)) + geom_point(aes(colour=factor(variable)))+coord_flip()
    })




    output$plot21 <-  renderPlot({

      somesome=final6[,c(paste(input$out),"YTD","Prod",paste(input$Group))]
      names(somesome)=c("value","YTD","Prod","Group")
      somesome=somesome %>% group_by(YTD,Group,Prod) %>% summarise(Value=sum( value)) %>% reshape2::melt(id=c("Group","YTD","Prod"))
      some1=NULL
      for(l in levels(somesome$Group))
      {
        ss=somesome[somesome$Group==l,]
        s=data.frame("Group"=l,"variable"=ss[ss$YTD=="2016",3],"value"=(ss[ss$YTD=="2016","value"]-ss[ss$YTD=="2015","value"])/ss[ss$YTD=="2015","value"])
        some1=data.frame(rbind(some1,s))
      }
      some1$above=ifelse(some1$value<0,"positive","negative")
      some1=data.frame(some1)

      ggplot( some1, aes(x=variable, y=value,fill = above)) +
        geom_bar(stat="identity", position="identity")+coord_flip() +
        geom_text(aes(label=paste(round(value*100,1),"%",sep="")))+
        scale_y_continuous(labels = percent_format()) +
        ylab(input$out)+
        theme(axis.title.x=element_blank(),legend.position="none")+
        facet_wrap(~ Group, ncol = 3)

    }
    )



    output$plot22 <-  renderPlot({

      b22=final6[,c(paste(input$out),"CHANNEL","Geo_Code")]
      names(b22)=c("value","CHANNEL","Geo_Code")
      b22=b22 %>% group_by(CHANNEL,Geo_Code) %>% summarise(value=sum(value))
      b22=b22[order(b22$value,decreasing = T),][1:30,]
      b22$Geo_Code=as.factor(as.character(b22$Geo_Code))
      ggplot(b22, aes(x= reorder(Geo_Code, value), y=value,fill=CHANNEL)) +
        geom_bar(stat="identity", position="identity")+
        ylab(input$out)+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        xlab("Markets")

    }
    )



    # Filter data based on selections
    output$table2 <- DT::renderDataTable(DT::datatable({
      data <- final6[,c("CHANNEL","Prod","Size","Geo_Code","Date","Y","TRADE","PRICE","DISTRIBUTION","MEDIA")]
      if (input$CHANNEL != "All") {
        data <- data[data$CHANNEL == input$CHANNEL ,]
      }
      if (input$Prod != "All") {
        data <- data[data$Prod == input$Prod,]
      }
      if (input$Size != "All") {
        data <- data[data$Size == input$Size,]
      }
      data[,c("Y","TRADE","PRICE","DISTRIBUTION","MEDIA")]=round(data[,c("Y","TRADE","PRICE","DISTRIBUTION","MEDIA")],2)
      data
    }))

    output$plot23 <-  renderDygraph({

      data23 <- final6[,c("CHANNEL","Prod","Size","Geo_Code","Date",paste(input$Variable21))]
      names(data23)=c("CHANNEL","Prod","Size","Geo_Code","Date","value")
      if (input$CHANNEL != "All") {
        data23 <- data23[data23$CHANNEL == input$CHANNEL ,]
      }
      if (input$Prod != "All") {
        data23 <- data23[data23$Prod == input$Prod,]
      }
      if (input$Size != "All") {
        data23 <- data23[data23$Size == input$Size,]
      }
      data23=data23 %>% group_by(Date) %>% summarise(value=sum(value))
      data23=data23[,c("Date","value")]
      names(data23)=c("Date",paste(input$Variable21))
      data23=xts(data23[,-1],order.by=as.POSIXct(data23$Date))
      if(dim(data23)[1]!=0){
      dygraph(data23 , ylab = paste(input$Variable21))%>%
        dyRangeSelector(height = 20)%>%
        dyLegend(show = "follow")}

    }
    )

    output$text1 <- renderText({
      input$out
    })

    output$ex1 <- renderUI({
      withMathJax(
        helpText('$$y-media=price+distribution+GeoCode*trade+AllCompetitorVolume+GeoCode+Month$$'))
    })




}


