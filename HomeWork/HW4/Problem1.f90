!###############################
! Name: Problem1
! Purpose: Execute Newton-Raphson's Method
! History: Made by schli242 on 10/1/15
!##############################

      Program Problem1
      Implicit None
!Difference between current and last x which we consider converging
      Real*8, Parameter :: eps =10**(-16)
      Real*8 ::  x, lastx
      Integer :: i
!For graphing
      Real*8, Dimension(1:1000) :: f
      Open(unit=10, file="cubicfunc")
      Open(unit=11, file="sinefunc")
   
      Do i=1, 1001
        x=5*(DBLE(i)-1)/1000
        Write(10,*) x, func1(x)
      End do

      Do i=1, 1001
        x=5*(i-1)/1000
        Write(11,*) x, func2(x)
      End do
      Write(*,*) 'For the cubic function'
!Begins at x
      x=-10
!Iterates to a maximum of 100
      Do i = 1, 100
!Computes new x value
      x= x-func1(x)/diffunc1(x)
!Writes at what iteration it stops and Exits do loop
      if (abs(func1(x))<=eps) Write(*,*) 'Stops at ', i
      if (abs(func1(x))<=eps) Exit
!Updates the last x value
      lastx=x
      End do
!Writes the root
      Write (*,*) 'Starting at -10, the root is ', x
      Write(*,*) ''
! Repeats procedure but for new x values
      x=0
      Do i = 1, 100
      x= x-func1(x)/diffunc1(x)
      if (abs(func1(x))<=eps) Write(*,*) 'Stops at ', i
      if (abs(func1(x))<=eps) Exit
      lastx=x
      End do
      Write (*,*) 'Starting at 0, the root is ', x
      Write(*,*) ''

      x=1
      Do i = 1, 100
      x= x-func1(x)/diffunc1(x)
      if (abs(func1(x))<=eps) Write(*,*) 'Stops at ', i
      if (abs(func1(x))<=eps) Exit
      lastx=x
      End do
      Write (*,*) 'Starting at 1, the root is ', x
      Write(*,*) ''
      
       x=5
      Do i = 1, 100
      x= x-func1(x)/diffunc1(x)
      if (abs(func1(x))<=eps) Write(*,*) 'Stops at ', i
      if (abs(func1(x))<=eps) Exit
      lastx=x
      End do
      Write (*,*) 'Starting at 5, the root is ', x
      Write(*,*) ''

      Write(*,*) 'The required iterations increase as x is ', &
      'farther away from the root'

! Repeats procedure for new function and new x values
      Write(*,*) 'For the sinx function'
      x=-1.5
      Do i = 1, 100
      x= x-func2(x)/diffunc2(x)
      if (abs(x-lastx)<=eps) Write(*,*) 'Stops at ', i
      if (abs(x-lastx)<=eps) Exit
      lastx=x
      End do
      Write (*,*) 'Starting at -1.5, the root is ', x
      Write(*,*) ''

      x=0
      Do i = 1, 100
      x= x-func2(x)/diffunc2(x)
      if (abs(func2(x))<=eps) Write(*,*) 'Stops at ', i
      if (abs(func2(x))<=eps) Exit
      lastx=x
      End do
      Write (*,*) 'Starting at 0, the root is ', x
      Write(*,*) ''

      x=1
      Do i = 1, 100
      x= x-func2(x)/diffunc2(x)
      if (abs(x-lastx)<=eps) Write(*,*) 'Stops at ', i
      if (abs(x-lastx)<=eps) Exit
      lastx=x
      End do
      Write (*,*) 'Starting at 1, the root is ', x
      Write(*,*) ''
      Write(*,*) 'The roots are converging at the same iteration ', &
      'for single and double precision'
      Contains
! Computes the cubic function     
      Function func1 (x)
      Implicit None
      Real*8 :: func1
      Real*8, intent(in) :: x
      func1 = x**3-7*x**2+14*x-3
      End function func1
! Computes the derivative of the cubic function
      Function diffunc1 (x)
      Implicit None
      Real*8 :: diffunc1
      Real*8, intent(in) :: x
      diffunc1= 3*x**2-14*x+14
      End function diffunc1
! Computes the sine function
      Function func2 (x)
      Implicit None
      Real*8 :: func2
      Real*8, intent(in) :: x
      func2 = sin(x)-x**3-0.25
      End function func2
! Computes the derivative of the sine function
      Function diffunc2 (x)
      Implicit None
      Real*8 :: diffunc2
      Real*8, intent(in) :: x
      diffunc2 = cos(x)-3*x**2
      End function diffunc2

      End Program Problem1
