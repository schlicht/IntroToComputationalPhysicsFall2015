!###################################
! Name: Problem1
! Purpose: Solve using LU Decomposition
! History: Made by schli242 on 10/28/15
!###################################

      Program Problem1
      Implicit None
      
      Real*8, Dimension(1:3, 1:3) :: A, inA, C
      Real*8, Dimension(1:3) :: x, b
      Real :: start, finish
      Integer :: i, j
      Open(unit=10, file='problem1.dat')
!Used to test if code works      
      !A=reshape((/ 1, 1, 1, 3, 4, 3, 3, 3, 4 /), shape(A))
      !b=(/2, 1, 0/)
      !inA = LUDecomp(A, 3)
      !x=LUforsub(inA, 3, b)
      !C=Matrixgen(3, 3)
!Used to test if the program works
!      Do i=1, 3
!        Do j=1, 3
!          Write(*,*) C(i, j)
!        End do
!      End do
!      Do i=1, 3
!        Write(*,*) x(i)
!      End do

! Outputs log(N) v. log(time) which obeys the expected 3rd power behavior
      Do i=250, 1000, 250
        call cpu_time(start)
        Call execute(i)
        Call cpu_time(finish)
        Write(10, *) log(DBLE(i)), log(finish-start)
      End do
      
      Contains
!Generates random Matrix of size N v. M
      Function Matrixgen(N, M)
        Implicit None
        Integer, Intent(in) :: N, M
        Real*8, Dimension(1:N, 1:M) :: A
        Real*8 :: x
        Integer :: i, j
        Real*8, Dimension(1:N, 1:M) :: Matrixgen
        Do i=1, N
          Do j=1, M
            Call Random_Number(x)
            A(i, j) = x
          End do
        End do
        Matrixgen = A
      End Function Matrixgen
! Generates random column matrix of length N
      Function Colgen(N)
        Implicit None
        Integer, Intent(in) :: N
        Real*8, Dimension(1:N) :: b
        Real*8 :: x
        Integer :: i
        Real*8, Dimension(1:N) :: Colgen
        Do i=1, N
          Call Random_Number(x)
          b(i) = x
        End do
        Colgen = b
      End Function Colgen
!Does LU Decomposition
      Function LUDecomp(A, N)
       Implicit None
       Integer, Intent(in) :: N
       Real*8, Dimension(1:N, 1:N), Intent(in) :: A
       Real*8, Dimension(1:N, 1:N) :: LU
       Integer :: i, j, k
       Real*8 :: y
       Real*8, Dimension(1:N, 1:N) :: LUDecomp
       
       Do j=1, N
         LU(1, j) = A(1, j)
         Do i=1, j
           y=0
           If(i>1) then
             Do k=1, i-1
               y=y+LU(i,k)*LU(k, j)
             End do
           End if
           LU(i, j)=A(i,j)-y
         End do

         Do i =j+1, N
           y=0
           If(j>1) then
             Do k=1, j-1
               y=y+ LU(i,k)*LU(k,j)
             End do
           End if
           LU(i, j)=(A(i,j)-y)/LU(j, j) 
         End do 
       End do
      
      LUDecomp = LU 
       
      End function LUDecomp
!Does LUforward substitution and backward substitution
      Function LUforsub(LU, N, b)
        Implicit None
        Integer, Intent(in) :: N
        Real*8, Dimension(1:N, 1:N), Intent(in) :: LU
        Real*8, Dimension(1:N), Intent(in) :: b
        Real*8, Dimension(1:N) :: x, y
        Integer :: i, j
        Real*8 :: a
        Real*8, Dimension(1:N) :: LUforsub

        y(1)=b(1)

        Do i=2, N
          a=0
          Do j=1, i-1
            a=a+ LU(i,j)*y(j)
          End do
          y(i)=b(i)-a
        End do

        x(N)=y(N)/LU(N,N)
        
        Do i=N-1, 1, -1
          a=0
          Do j=i+1, N
            a=a+LU(i, j)*x(j)
          End do
          x(i)=(y(i)-a)/LU(i,i)
        End do

        LUforsub= x

      End function LUforsub

!Solves the entire system for a random input of size N
      Subroutine execute(N)
        Integer, Intent(in) :: N
        Real*8, Dimension(1:N, 1:N) :: mat, LUmat
        Real*8, Dimension(1:N) :: b, x
        mat = Matrixgen(N, N)
        b = Colgen(N)
        LUmat = LUDecomp(mat, N)
        x = LUforsub(LUmat, N, b)
      
      End subroutine execute
      End Program
