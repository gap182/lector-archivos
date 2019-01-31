program prueba
implicit none

INTEGER, PARAMETER :: DP=SELECTED_REAL_KIND(P=15)
real(dp) :: a(740,18)
integer :: b(740,2), intpos(2), charpos(1)
character(30) ::  c(740,1), name


name='jla_lcparams.txt'
intpos=(/4,18/)
charpos=(/1/)

! character(30) :: name 

call read_data(name,21,740,a,2,b,1,c,1,intpos,charpos)



end program prueba