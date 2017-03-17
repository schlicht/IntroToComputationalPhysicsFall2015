!#############################################
! Name: Problem1
! Purpose: Execute Transformation and Rejection Methods
! History: Made by schli242 on 10/20/15
!#############################################

      Program Problem1
      Implicit None
      Integer :: n=1000000, bins=100

      Call transform(n, bins)
      Call rejection(n, bins)

      Contains
!Executes the transform method
      Subroutine transform (n, bins)
        Implicit None
        Integer, Intent(in) :: n, bins
        Integer, Parameter :: seed=86456
        Real*8 :: num
        Real*8, Dimension(1:bins) :: xbins
        Integer :: i, p
        Real*8 :: x, y
        
        open(unit=10, file="binstrans.dat")
!Generates random number sequence
        Call srand(seed)
!Initializes the bins to be empty
        Do i=1, bins
          xbins(i)=0
        End do

        Do i=1, n
!Obtains random number
          Call Random_Number(y)
!Converts to the probability distribution
          x=(DBLE(8)*y)**(1/DBLE(3))
!Finds bin to put x in   
          p=INT(DBLE(bins/2)*x+0.5)
!Adds to that bin
          xbins(p)=xbins(p)+1
        End do
!Writes output to file        
        Do i=1, bins
          Write(10,*) DBLE(i)/50, xbins(i)
        End do
         
      End subroutine transform
!Executes Rejection Method
      Subroutine rejection (n, bins)
        Implicit None
        Integer, Intent(in) :: n, bins
        Integer, Parameter :: seed=86456
        Real*8, Dimension(1:bins) :: xbins
        Integer :: i, p
        Real*8 :: x, y
        
        open(unit=11, file="binsrej.dat")
!Generates Random Number sequence
        Call srand(seed)
!Initializes bin to be zero
        Do i=1, bins
          xbins(i)=0
        End do
        i=0
        Do while (i<n)
!Gets two random numbers. Y must be less than the 
!probability function at x for it to be accepted
          Call Random_Number(y)
          y=2*y
          Call Random_Number(x)
          x= 2*x
!Adds to bin if acceptable
          if(y<=DBLE(3)/DBLE(8)*(x**2)) then
            i=i+1
            p=INT(DBLE(bins/2)*x+0.5)
            xbins(p)=xbins(p)+1
          End if
        End do
!Writes output to file        
        Do i=1, bins
          Write(11,*) DBLE(i)/2, xbins(i)
        End do
         
      End subroutine rejection

      End Program Problem1
