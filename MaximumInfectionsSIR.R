# Setup
install.packages('ggplot2');
install.packages('latex2exp');
library(latex2exp);
library(ggplot2);

Rnought<-seq(0, 10, 0.01);
final_size<-1-1/Rnought*(1+log(Rnought));
data <- data.frame("Rnought" = Rnought, "final_size"=final_size);
ggplot(data=data, aes(x=Rnought, y=final_size))+
  geom_line(col='red')+xlab(TeX(r'($R_0$)'))+ylab(TeX(r'($I^*$)'))+ggtitle('Maximimum infected in SIR model')+
  geom_vline(xintercept=1,linetype='dashed', color='blue',size=0.8)+ylim(0, 1)+scale_x_continuous(breaks=seq(0, 10, 1))
