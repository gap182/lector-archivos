program prueba2

implicit none

integer :: a

open(201,file="prueba2.txt")

write(201,*) 1

write(201,*) 2

write(201,*) 3

write(201,*) 4

write(201,"(I5)", advance='no') 5

! close(201)

! open(201,file="prueba2.txt",status="old")

! read(201,*) a 


write(201,*) 6
write(201,*) 7

close(201)

! write(*,*) a


end program prueba2



