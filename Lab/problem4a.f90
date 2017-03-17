!####################################
! Name: Problem4a
! Purpose: Approximate a sine wave
! History: Made by schli242 on 9/13/15
!####################################

      Program Problem4a
      Implicit None

      Real*8 :: error, xvalue
      Real*8, Parameter :: Pi = 3.141592
      Integer :: l, N, j
      Real*8, Dimension(0:100) :: time
      Real*8 :: tim 
!Iterates over values of x between -pi and pi, writing the error
!between our sine and the computer's sine
      real :: start, finish, star, fin
      Do j =0, 100
      call cpu_time(start)
      Do l = 0, 100000
      xvalue = pi*l/50000-pi
      error = sinN(xvalue, 13) - sin(xvalue)
      !Write(*,*) 'The error at ', xvalue, 'is ', error
      End Do
      call cpu_time(finish)
      !print '("Time = ",f6.3, " seconds.")', finish-start
      time(j) = finish-start
      End Do
      tim = 0
      Do j=0, 100
      tim = tim + time(j)
      End do
      tim = tim/101
      Write(*, *) "Time of old function: ", tim  
      DO j = 0, 100
      call cpu_time(star)
      Do l = 0, 100000
      xvalue = pi*l/50000-pi
      error = sinN2(xvalue, 13) - sin(xvalue)
      !Write(*,*) 'The error at ', xvalue, 'is ', error
      End Do  
      call cpu_time(fin)
      time(j) = fin-star
      End Do
      tim = 0
      Do j=0, 100
      tim = tim + time(j)
      End do
      tim =tim/101
      Write(*,*) "Time of new function: ", tim
      !print '("Time = ",f6.3, " seconds.")', fin-star
      DO j = 0, 100
      call cpu_time(star)
      Do l = 0, 100000
      xvalue = pi*l/50000-pi
      error = sin(xvalue)
      !Write(*,*) 'The error at ', xvalue, 'is ', error
      End Do  
      call cpu_time(fin)
      time(j) = fin-star
      End Do
      tim = 0
      Do j=0, 100
      tim = tim + time(j)
      End do
      tim =tim/101
      Write(*,*) "Time of computer's sine: ", tim

      Contains
! Our function for sine. A taylor series.
      Real*8 Function sinN (x, n)
      Implicit None
      Real*8, intent(in) :: x
      Integer, intent(in) :: n
      Real*8 :: s = 0
      Integer :: i = 1
      s=0
!Each iteration adds on the next term to the sum
      Do i=0, n
      s = s + ((-1)**i)*(x**(2*i+1))/factorial(2*i+1)
      End Do
      sinN = s
      End Function sinN


      Real*8 Function sinN2 (x, n)
      Implicit None
      Real*8, intent(in) :: x
      Integer, intent(in) :: n
      Real*8 :: s = 0
      Real*8 :: A = 1
      Integer :: i = 1
      s=0
!Each iteration adds on the next term to the sum
      Do i=0, n
      s = s*A
      A=-(x**2)/((2*l+1)*(2*l))*A
      End Do
      sinN2 = s
      End Function sinN2
! A function which computes the factorial of the input
      Real*8 Function factorial (n)
      Implicit None
      Integer, intent(in) :: n
      Integer :: i
      Real*8 :: t=1
      t = 1
! Handles the case of n=0
      If( n == 0) Then
      factorial = 1
! Handles all other cases for n>0 and n is an integer
      Else
      Do i=1, n
      t = t*real(i, 8)
      End do
      factorial = t
      End if
      End Function factorial
      End Program
