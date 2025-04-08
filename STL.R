decomp = stl(ts_freq_mens_cinema_0020_corr, s.window="periodic")
# show(decomp)
plot(decomp)

fitstl = stlm(ts_freq_mens_cinema_0020_corr)

prevstl <- forecast(fitstl,12) #période d'une année

# show(prevsts) # pas mettre en annexe

plot(prevstl) # en annexe

summary(prevstl)