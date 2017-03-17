!#################################
! Name: Problem1a
! Purpose: Calculate Bessel Functions using upward recursion
! History: Made by schli242 on 9/18/15
! Note: Uses plotb.script to graph
!#################################

      Program Problem1a 
! Array holds the xvalues used to test the Program
      Real*8, Dimension(1:3) :: xvalues
! Array holds the j values
      Real*8, Dimension(0:75) :: j
! Holds the current x value
      Real*8 :: x, factor
! The actual value of j0
      Real*8, Parameter :: j0 =.841470985
! Iterators
      Integer :: i, l
! Opens data file for graphing j values
      open(unit=10, file='problem1bdata')
! Our test values
      xvalues(1)= 0.1
      xvalues(2) = 1.0
      xvalues(3) = 10.0

!holds the x value for graphing 
        x = 10
!Produces the first 2 j values
      j(75)= 1.0
      j(74) = 1.0
!Constructs the next few j values for graphing 
      Do l = 73, 0, -1
          j(l) =(2*l+3)/x*j(l+1)-j(l+2)
      End do
! Computes the factor which our j's are off by
      factor = j0/j(0)
! Fixes the j's
      Do l=0, 75
        j(l)=factor*j(l)
      End do
!Writes j values to the created data file
! Uses plotb.script
      Do i = 0, 4
        Write(10, *) i, j(i)
      End do
      Write(*, *) j(3), j(5), j(8)
! The values at x for j(3), j(5), and j(8)
! x= 1 8.0229231095178164E-006   8.1053485371790501E-010   2.4453494624073976E-016
! x= 1 9.0065808173587902E-003   9.2561155530671645E-005   2.8264987081442335E-008
! x= 3 2.7199628023960440       0.29332489798341643        2.6802879899657044E-003
! x= 5 -1.0083558241736925      -0.46864227240232587       -2.5190990866860656E-002
! x= 7 -1.4453010623675603E-002   1.5435893622670576       0.37601410792183587 
! x= 9 -2.0595171048494811       0.64785509910881145        2.0214424784021547
! x= 10 0.61090655865299104       0.85898649324949794       -1.9423926314367046 
! x= 11 -0.45276347792839189       0.88760269541924719      -0.99256724571709809
      End Program

