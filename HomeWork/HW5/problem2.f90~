!#################################
! Name: Problem2
! Purpose: Use Gaussian Elimination to find inverse matrix
! History: Made by schli242 on 10/11/15
!################################

      Program Problem2
      Implicit None
      Real*8, Dimension(1:4, 1:4) :: A, B
      Integer, Parameter :: siz = 4
      Real*8, Dimension(1:4, 1:4) :: inA, id
      Integer:: x
      Real*8 :: s
      Integer :: i, j, k
! Creates the matrix
      A(1, 1)=0.0162
      A(1, 2)=0.6946
      A(1, 3)=0.7575
      A(1, 4)=0.6268
      A(2, 1)=0.2785
      A(2, 2)=0.8000
      A(2, 3)=0.9872
      A(2, 4)=0.1821 
      A(3, 1)=0.7116
      A(3, 2)=0.5465
      A(3, 3)=0.4708
      A(3, 4)=0.2163 
      A(4, 1)=0.7455
      A(4, 2)=0.7631
      A(4, 3)=0.2036
      A(4, 4)=0.6604
!Creates an identity matrix
      id(1, 1)=1
      id(1, 2)=0
      id(1, 3)=0
      id(1, 4)=0
      id(2, 1)=0
      id(2, 2)=1
      id(2, 3)=0
      id(2, 4)=0 
      id(3, 1)=0
      id(3, 2)=0
      id(3, 3)=1
      id(3, 4)=0 
      id(4, 1)=0
      id(4, 2)=0
      id(4, 3)=0
      id(4, 4)=1  
!Initializes the inverse matrix
      Do i=1, siz
        Do j=1, siz
         inA(i, j) = 0
        End do
      End do

      Do i =1, siz
! Determines the row with the highest first value
!        x=i
!        If(i<siz) then
!          Do j=i+1, siz
!            if(abs(A(j, i))>abs(A(x, i))) then
!              Write(*,*) abs(A(j, i)), abs(A(x, i)), x, i, A(x, i)
!             x=j
!            End if
!            Write(*,*) i, j, x
!          End do
!        End if
! Switches the rows so the highest value is first
!        If(x/=i) then
!          Call swap(i, x, siz, A)
!          Call swap(i, x, siz, id)
!        End If

! Subtracts the rows until the identity is created.
        Do j = i+1, siz
!scaling factor to multiply the leading row by
          s = A(j, i)/A(i, i)
          Do k = i, siz
!Subtracts rows for both matrices
            A(j, k) = A(j, k)-s*A(i, k)
            id(j,k) = id(j,k)-s*id(i,k)
          End do
        End do
      End do

! Back Substitution
      Do i=1, siz
!Writes first term
        inA(siz, i)=id(siz, i)/A(siz, siz)
        Do j=siz-1, 1, -1
!Adds the b term 
            inA(j, i)=id(j,i)
          Do k=j+1, siz
! Adds the sum
            inA(j,i)=inA(j,i)- A(j,k)*inA(k, i)
          End do
!Scales by coefficient to get values
            inA(j,i)=inA(j,i)/A(j,j)
        End do
      End do
!Prints inverse of A
    Write(*,*) 
    Call prin(inA, siz)

!Prints the matrix product of A with its inverse
     Write(*,*)
      Do i = 1, siz
        Do j = 1, siz
          B(i, j)=0
          Do k = 1, siz
            B(i,j)= B(i,j)+ A(i, k)*inA(k, j)
          End do 
        End do
      End do
      Call prin(B, siz)
      Contains
!Routine swaps matrices for highest term as leading row
      Subroutine swap (i, x, siz, A)
        Implicit None
        Integer, Intent(in) :: i, x, siz
        Real*8, Dimension(1:siz, 1:siz), Intent(inout) :: A
        Real*8, Dimension(1:siz) :: temp
        Integer :: j
        Do j =1, siz
          temp(j) = A(i, j)
        End do
        Do j=1, siz
          A(i, j) = A(x, j)
        End do
        Do j=1, siz
          A(x, j) = temp(j)
        End do
      End subroutine swap
! Routine prints the matrix
      Subroutine prin (A, siz)
        Integer, Intent(in) :: siz
        Real*8, Dimension(1:siz, 1:siz), Intent(in) :: A
        Integer :: i, j
        Do i = 1, siz
            Write(*,*) A(i, 1), A(i, 2), A(i, 3), A(i, 4)
        End do
      End subroutine prin

      End Program Problem2
