analyzeDiff<-function(imageFile1,imageFile2,coordinates) {
  options(warn=-1)
  
  library(tiff);
  image1<-readTIFF(imageFile1);
  image2<-readTIFF(imageFile2);
  ##cut images if necessay, in order to make them the same size
  minH<-min(dim(image1)[1],dim(image2)[1]);
  minW<-min(dim(image1)[2],dim(image2)[2]);
  image1<-image1[1:minH,1:minW,];
  image2<-image2[1:minH,1:minW,];
  
  if (coordinates=="RGB") {
 
  #######
  ##RGB##
  #######
  
  ##R
  difImage<-(image1[,,1])-(image2[,,1]);
  R_ABS<-sum(abs(difImage))/(dim(difImage)[1]*dim(difImage)[2]);
  R<-sum(difImage)/(dim(difImage)[1]*dim(difImage)[2]);

  ##G
  difImage<-(image1[,,2])-(image2[,,2]);
  G_ABS<-sum(abs(difImage))/(dim(difImage)[1]*dim(difImage)[2]);
  G<-sum(difImage)/(dim(difImage)[1]*dim(difImage)[2]);

  ##B
  difImage<-(image1[,,3])-(image2[,,3]);
  B_ABS<-sum(abs(difImage))/(dim(difImage)[1]*dim(difImage)[2]);
  B<-sum(difImage)/(dim(difImage)[1]*dim(difImage)[2]);
  
  result<-matrix(data<-c(R_ABS,R,G_ABS,G,B_ABS,B),nrow=2,ncol=3,dimnames=list(c("Absolute Value","Value"),c('Red (Cyan)','Green (Magenta)','Blue (Yellow)')));
  
  options(warn=-1);
  print(result);
  
  }
  
  else 
    
    {
  
  #######
  ##LAB##
  #######
  image1Lab<-image1;
  image2Lab<-image2;
  deltaImage<-image2[,,1];
  
  ##Convert to LAB
   for (i in 1:dim(image1)[1]) {
     cat("\014");
     print(paste("Converting image1 to Lab coordinates: ",round(100*i/dim(image1)[1], 2), "%", sep=""));
     line<-do.call(cbind, list(image1[i,,1],image1[i,,2],image1[i,,3]));
     matrixLine<-matrix(line,ncol=3);
     labLine<-convertColor(matrixLine,from="sRGB",to="Lab");
     image1Lab[i,,1]<-labLine[,1];
     image1Lab[i,,2]<-labLine[,2];
     image1Lab[i,,3]<-labLine[,3];
   }
  
  for (i in 1:dim(image2)[1]) {
    cat("\014");
    print(paste("Converting image2 to Lab coordinates: ",round(100*i/dim(image2)[1], 2), "%", sep=""));
    line<-do.call(cbind, list(image2[i,,1],image2[i,,2],image2[i,,3]));
    matrixLine<-matrix(line,ncol=3);
    labLine<-convertColor(matrixLine,from="sRGB",to="Lab");
    image2Lab[i,,1]<-labLine[,1];
    image2Lab[i,,2]<-labLine[,2];
    image2Lab[i,,3]<-labLine[,3];
  }
  
  for (i in 1:dim(deltaImage)[1]) {
    cat("\014");
    print(paste("Calculating delta matrix: ",round(100*i/dim(deltaImage)[1], 2), "%", sep=""));
    for (j in 1:dim(deltaImage)[2]) {
      deltaImage[i,j]<-sqrt((image2Lab[i,j,1]-image1Lab[i,j,1])^2+(image2Lab[i,j,2]-image1Lab[i,j,2])^2+(image2Lab[i,j,3]-image1Lab[i,j,3])^2);
    }
  }
  
  
  cat("\014");
  
  ##L
  difImage<-(image1Lab[,,1])-(image2Lab[,,1]);
  L_ABS<-sum(abs(difImage))/(dim(difImage)[1]*dim(difImage)[2]);
  L<-sum(difImage)/(dim(difImage)[1]*dim(difImage)[2]);
  
  ##A
  difImage<-(image1Lab[,,2])-(image2Lab[,,2]);
  A_ABS<-sum(abs(difImage))/(dim(difImage)[1]*dim(difImage)[2]);
  A<-sum(difImage)/(dim(difImage)[1]*dim(difImage)[2]);
  
  ##B
  difImage<-(image1Lab[,,3])-(image2Lab[,,3]);
  B_ABS<-sum(abs(difImage))/(dim(difImage)[1]*dim(difImage)[2]);
  B<-sum(difImage)/(dim(difImage)[1]*dim(difImage)[2]);
  
  ##Delta
  Delta<-sum(deltaImage)/(dim(deltaImage)[1]*dim(deltaImage)[2]);
  
  result<-matrix(data<-c(L_ABS,L,A_ABS,A,B_ABS,B),nrow=2,ncol=3,dimnames=list(c("Absolute Value","Value"),c('L','a','b')));
  
  options(warn=-1);
  print(result);
  print(paste("Delta:",Delta));
  
  };
  
  }