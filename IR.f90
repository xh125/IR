program main
implicit none
character(len=20) :: input
logical :: alive
integer,parameter::inputfile=11,outputfile=12
integer ::counter=0,lines
real :: x,y,w2
real,allocatable:: x0(:),y0(:)
real :: left,right
real ::A=3.0,step=0.05,w !A为半高宽，step为x增加的间距
integer ::status


w2=A**2/2/LOG(2.0)
write(*,*)"input file name:"
read(*,*)input

inquire(file=input,exist=alive)
if(.not.alive)then
  write(*,*)input,"does not exist"
  stop
end if
open(unit=inputfile,file=input,access="sequential",status="old")
read(inputfile,*)
do while(.true.)
  read(unit=inputfile,fmt="(6X,F9.2,11X,F9.4)",iostat=status) x,y
  
  if(status/=0)exit
  counter=counter+1
  write(*,*)counter,"line:",x,y
end do

lines=counter
rewind(inputfile)

allocate(x0(counter))
allocate(y0(counter))
read(inputfile,*)
do counter=1,lines
  read(unit=inputfile,fmt="(6X,F9.2,11X,F9.4)",iostat=status) x0(counter),y0(counter)
  if(status/=0)exit
end do

write(*,*)"left:"
read(*,*)left
write(*,*)"right:"
read(*,*)right

open(unit=outputfile,file="data.dat")
x=left
do while(x<=right)
  y=0.0
  do counter=1,lines
    if(abs(x-x0(counter)<=3*A)) then
     y=y0(counter)/EXP(2*(x-x0(counter))**2/w2)+y
    end if
  end do
  write(unit=outputfile,fmt="(F9.2,1X,F9.4)")x,y
  x=x+step
end do


stop
end program