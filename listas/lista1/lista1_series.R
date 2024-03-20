pacman::p_load(TSA)
data(larain)


plot(larain,ylab='Inches',xlab='Year',type='o')

plot(y=larain,x=zlag(larain),ylab='Inches',
       xlab='Previous Year Inches')


data(color)
plot(color,ylab='Color Property',xlab='Batch',type='o')


data(oilfilters); plot(oilfilters,type='o',ylab='Sales')



plot(oilfilters,type='l',ylab='Sales')
 points(y=oilfilters,x=time(oilfilters),
         pch=as.vector(season(oilfilters)))