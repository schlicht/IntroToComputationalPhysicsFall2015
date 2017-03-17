!########################################
! Name: Problem1
! Purpose: Compare Forward and Central Differences
! History: Made by schli242 on 9/23/15
!########################################
      Program Problem1
      Implicit None
      Integer :: n
! The f array holds the f(x) values
      Real*8, Dimension(0:5000) :: f
      Integer :: fsize = size(f), i
! ferror is the error for forward difference at the current 
!point while maxferror is the max error for the current value
      Real*8 :: ferror = 0, maxferror = 0
! Its the same for the central difference
      Real*8 :: cerror = 0, maxcerror = 0
! c is the derivative at the current point
      Real*8 :: c
! Files for storing values for plotting
      open(unit=10, file="forwarddifferror")
      open(unit=11, file="centraldifferror")
! Loops througn n values in the range of 500 to 5000
      Do n = 500, 5000, 500
! Iterates through all points for the current n
        Do i = 0, n
!Calculates the f value and stores in the f array
          f(i) = tanh(DBLE(DBLE(-2)+ DBLE(4*DBLE(i)/DBLE(n))))
        End do
! Iterates through the points
        Do i = 0, n-1
! Calculates the current derivative
          c = cosh(DBLE(DBLE(-2) + DBLE(4*DBLE(i)/DBLE(n))))**(-2)

! Determines the error
          ferror = abs((ForwardDiff(1, i, f, fsize)*n/4-c)/c)
          
! The if statement ensures that the central difference doesn't
! go out of the range
          if (i/=0 .AND. i/=n-1) then
!Computes the error in the central difference
          cerror = abs((CentralDiff(1, i, f, fsize)*n/8-c)/c)
          end if
! Replaces the maxerror if larger than maxerror
          if(ferror > maxferror) then
            maxferror = ferror
          endif
          if(cerror > maxcerror) then
            maxcerror = cerror
          endif
        End do
!Writes the values to their files for plotting
        Write(10, *) log(DBLE(n)), log(maxferror)
        Write(11, *) log(DBLE(n)), log(maxcerror)
!Resets errors for the next iteration
        ferror = 0
        cerror = 0
        maxferror = 0
        maxcerror = 0
      End do
      Do i =0, 1

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
      Real*8 Function ForwardDiff (n, k, fvalues, fsize)
      Implicit None
      Integer, Intent(in) :: n, k, fsize
      Real*8, Dimension(0:fsize), Intent(in) :: fvalues
      Real*8 :: x
      Integer :: l
      x = 0
      Do l = 0, n
        x = x+((-1)**l)*Binomial(n, l)*fvalues(k+n-l)
      End Do
      ForwardDiff = x
      ForwardDiff= x
      End function ForwardDiff
! Behaves the same way as FowardDiff except it uses central difference
      Real*8 Function CentralDiff (n, k, fvalues, fsize)
      Implicit None
      Integer, Intent(in) :: n, k, fsize
      Real*8, Dimension(0:fsize), Intent(in) :: fvalues
      Real*8 :: x
      Integer :: l
      x = 0
      Do l = 0, n
        x = x+((-1)**l)*DBLE(Binomial(n, l))*fvalues(Int(k+n/2-l+.5))
      End Do
      CentralDiff = fvalues(i+1)-fvalues(i-1)
      End function CentralDiff
      End Program

