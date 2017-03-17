!#################################
! Name: Problem1a
! Purpose: Calculate Bessel Functions using upward recursion
! History: Made by schli242 on 9/18/15
!#################################

      Program Problem1a
! Array holds the xvalues used to test the Program
      Real*8, Dimension(1:3) :: xvalues
! Array holds the j values
      Real*8, Dimension(0:8) :: j
! Holds the current x value
      Real*8 :: x
! Iterators
      Integer :: i, l
! Opens data file for graphing j values
      open(unit=10, file='problem1adata')
! Our test values
      xvalues(1)= 0.1
      xvalues(2) = 1.0
      xvalues(3) = 10.0
!holds the x value for graphing (last one was 11)
        x = 11
!Produces the first 2 j values
        j(0)=sin(x)/x
        j(1)=(sin(x)-x*cos(x))/(x**2)
!Constructs the next few j values for graphing 
      Do l = 2, 4
          j(l) =(2*l-1)/x*j(l-1)-j(l-2)
      End do
!Writes j values to the created data file
      Do i = 0, 4
        Write(10, *) i, j(i)
      End do
! Tests the accuracy of j compared to know values
      Do i = 1, 3
        Do l = 0, 8
          j(l)=0
        End do
        x = xvalues(i)
! Uses a taylor approximation for small x values to increase accuracy
        If(x<1) then
          j(0) = 1-(x**2)/6+(x**4)/120
          j(1)= x/3 -(x**3)/30
        Else
! Uses trig functions if x is large enough
          j(0)=sin(x)/x
          j(1)=(sin(x)-x*cos(x))/(x**2)
        End if
! Generates other j values
        Do l = 2, 8
          j(l) =(2*l-1)/x*j(l-1)-j(l-2)
        End do
! Writes the j values for comparison to actual values
! Uses plot.script to graph
        Write(*, *) j(3), j(5), j(8)
      End do
      Write(*,*) 'Except for x=0.1 (which has egregeous errors,) ', &
      'forward recursion has 4 digits of precision. Even with the ', &
      'taylor approximation, the j values are unreliable for x<1.'
      End Program
