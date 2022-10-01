nama <- c('Rama', 'Jaya','Dova', 'Deta')
uts <- c(80,70,75,88)
uas <- c(40,55,70,80)
tugas1 <- c(0,80,85,75)
tugas2 <- c(75,90,0,65)
data <- data.frame(nama, uts,uas,tugas1,tugas2)


for(i in 1:nrow(data)){
  nilai.akhir <- (0.4*data[i,2]) + (0.3*data[i,3]) + (0.2*(data[i,4]+data[i,5]))
  if(nilai.akhir >= 80){
    konversi <- 'A'
  }else if (nilai.akhir>= 65 && nilai.akhir< 80){
    konversi <-'B'
  }else if(nilai.akhir>= 50 && nilai.akhir< 65){
    konversi <- 'C'
  }else{
    konversi <- 'D'
  }
  print(paste('nilai akhir', data[i,1], 'adalah', nilai.akhir,'atau', konversi))
}
