!################################
! Name: Problem2
! Purpose: Calculate Forward Differences
! History: Made by schli242 on 9/21/15
!################################

      Program Problem2
      Implicit None
! Iterators
      Integer :: m, j, a
! Creates the arrays that contain the f values 
      Real*8, Dimension(0:5) :: fquad
      Real*8, Dimension(0:5) :: fsin

! Tests the Binomial Function
      Do m =1, 4
        Do j=0, m
        Write(*, *) m, 'Choose', j, 'is ', Binomial(m, j)
        End Do
      End Do
! Initializes the f values
      fquad(0)=0
      fquad(1)=0.04
      fquad(2)=0.16
      fquad(3)=0.36
      fquad(4)=0.64
      fquad(5)=1
      fsin(0)=0
      fsin(1)=0.198669331
      fsin(2)=0.389418342
      fsin(3)=0.564642473
      fsin(4)=0.717356091
      fsin(5)=0.841470985
! Writes the forward differences for fquad 
      Do a = 1, 4
        Write(*,*) 'The ', a, ' Forward Difference of x^2 is ', &
        ForwardDiff(a, 0, fquad, 5)
      End do
! Writes the forward differences for fsin
      a= 1
      Do a = 1, 4
        Write(*,*) 'The ', a, ' Forward Difference of sin(x) is ', &
        ForwardDiff(a, 0, fsin, 5)
      End do
      Contains
! Computes the binomial coefficients
! n Choose i
      Integer Function Binomial (n, i)
      Implicit None
      Integer, Intent(in) :: n, i
      Integer :: l
      Binomial = 1
      Do l=1, i
        Binomial=Binomial*(n-l+1)/(l)
      End do
      End function Binomial
! Computes the forward Differences
! n is the order of the difference, k is the fvalue that we are computing
! the forward difference for, fvalues is the array containing the fvalues
! fsize is the size of fvalues
      Real Function ForwardDiff (n, k, fvalues, fsize)
      Implicit None
      Integer, Intent(in) :: n, k, fsize
      Real*8, Dimension(0:fsize), Intent(in) :: fvalues
      Real*8 :: x
      Integer :: l
      x = 0
      Do l = 0, n
        x = x+((-1)**l)*Binomial(n, l)*fvalues(k+n-l)
      End Do
      ForwardDiff = Real(x)
      End function ForwardDiff
      End Program Problem2
