!####################################
! Name: Problem4a
! Purpose: Approximate a sine wave
! History: Made by schli242 on 9/13/15
!####################################

      Program Problem4a
      Implicit None

      Real*8 :: error, xvalue
      Real*8, Parameter :: Pi = 3.141592
      Integer :: l, N
!Iterates over values of x between -pi and pi, writing the error
!between our sine and the computer's sine
      Do l = 0, 20
      xvalue = pi*l/10-pi
      error = sinN(xvalue, 6) - sin(xvalue)
      Write(*,*) 'The error at ', xvalue, 'is ', error
      End Do  

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
