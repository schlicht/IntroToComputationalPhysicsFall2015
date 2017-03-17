!#################################
! Name: Problem 2a
! Purpose: Execute Spline Method
! History: Made by schli242 on 10/28/15
!#################################


      Program Problem2
      Implicit None
      Integer, Parameter :: N =3
!Coefficient Matrix
      Real*8, Dimension(0:N, 0:N) :: A
      Real*8, Dimension(0:N) :: b, D, f
      Real*8, Dimension(0:N-1):: g, h
!a, b, c, and d values
      Real*8, Dimension(0:N-1) :: a1, b1, c1, d1
      Integer :: i, j
      Open(unit=10, file="bandD.dat")
      Open(unit=11, file="abcdvalues.dat")
!f values at the sampled x values
      f = (/1, 4, 15, 40/)
!Writes A matrix
      Do i=0, N
        Do j=0, N
          A(i, j) = 0
        End do
      End do
      A(0,0) =2
      A(0, 1) = 1
      A(N, N-1) = 1
      A(N, N)= 2
      Do i=1, N-1
        A(i, i) =4
        A(i, i+1) =1
        A(i, i-1)= 1
     End do
     Do i =0, N
       Do j=0, N
!        Write(*,*) A(i, j)
       End do
     End do
!Writes b column vector using f values
     b(0)= 3*(f(1)-f(0))
     b(1)= 3*(f(2)-f(0))
     b(N-1)= 3*(f(N)-f(N-2))
     b(N)= 3*(f(N)-f(N-1))
     If(N>3) then
!       Do i=2, N-2
!        b(i)=3*(f(i+1)-f(i-1))
!       End do
     End if
!Produces g and h column vectors
      g(N-1) = -A(N,N-1)/A(N,N)
      h(N-1)=b(N)/A(N,N)
      Do i=N-1, 1, -1
        g(i-1) = -A(i,i-1)/(A(i,i)+A(i, i+1)*g(i))
        h(i-1) = (b(i)-A(i, i+1)*h(i))/(A(i,i)+A(i, i+1)*g(i))
      End do
!Creates D values 
      D(0) = (b(0)-A(0,1)*h(0))/(A(0,0)+A(0,1)*g(0))
      Do i=0, N-1
        D(i+1)= g(i)*D(i)+h(i)
      End do
      Do i = 0, N
        Write(*,*) D(i)
      End do
!Creates a, b, c, and d values
      Do i= 0, N-1
        a1(i)=f(i)
        b1(i)=D(i)
        c1(i)=3*(f(i+1)-f(i))-2*D(i)-D(i+1)
        d1(i)=2*(f(i)-f(i+1))+D(i)+D(i+1)
      End do
!Writes the values to a file to verify that they are the correct values
      Do i=0, N-1
        Write(11, *) i, a1(i), b1(i), c1(i), d1(i)
        Write(10,*) i, D(i), b(i)
      End do
        Write(10, *) i, D(N), b(N)

      End Program Problem2
