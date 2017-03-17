!####################################3
! Name: Problem2
! Purpose: Determine Convolution of Functions
! History: Made by schli242 on 11/17/15
!#####################################

      Program Problem2
      Implicit None
      Integer :: N=256
      Real*8 :: pi = 3.14159265359
      Open(unit=10, file='tophat.dat')
      Call cosineauto(N, 10, 'cosin05.dat') !produces gaussian
      Call sineauto(N, 11, 'sine005.dat')
      Call ranfauto(N, 11, 'ranf005.dat')
!      Call gaussconvol(N, 1.0, 11, 'gauss01.dat')
!     Call gaussconvol(N, 2.0, 12, 'gauss20.dat')
!      Call gaussconvol(N, 1.0, 13, 'gauss50.dat')
!      Call gaussconvol(N, 1.0, 14, 'gauss10.dat')
!      Write(*,*) 'The gaussian becomes sharper as a/N increases.'
      Contains
      Real*8 function ranf(iseed)
        Integer, intent(inout) :: iseed
        Integer :: a, m
        
        a=16807
        M= 2147483647
        iseed = MOD(a*iseed, m)
        ranf = 0.5*(DBLE(iseed)/DBLE(m)+1)
        Return
      End function ranf

      Subroutine ranfauto(N, u, str)
      Implicit None
      Integer, Intent(in) :: N, u
      Character(LEN=11), Intent(in) :: str 
      Real, Dimension(0: N-1) :: rx1, ry1, rx2, ry2
      Real, Dimension(0: N-1) :: sx1, sy1, sx2, sy2
      Real, Dimension(0: N-1) :: x, y
      Integer :: i
      Open(unit=u, file=str)
      rx1(0)=0
      ry1(0)=0
      sx1(0)=0
      sy1(0)=0
      Do i=0, N-1
        rx1(i) = ranf(1201363)
        ry1(i) = 0
        sx1(i) = rx1(i)
        sy1(i)=0
      End do
      Do i=0, N-1
        rx2(i)=rx1(i)
        ry2(i)=ry1(i)
        sx2(i)=sx1(i)
        sy2(i)=sy2(i)
      End do

      Call CFFT(rx2, ry2, N, 8, 1)
      Call CFFT(sx2, sy2, N, 8, 1)
      Do i=0, N-1
        x(i)=(rx2(i)*sx2(i)+ry2(i)*sy2(i))/N
        y(i)=(-rx2(i)*sy2(i)+ry2(i)*sx2(i))/N
	Write(*,*) rx2(i), sx2(i)
      End do

      Call CFFT(x, y, N, 8, -1)
      
      Do i = 0, N-1
        Write(u,*) i, x(i), y(i)
      End do 
      End Subroutine ranfauto

!same routine except performs for gaussian
      Subroutine cosineauto(N, u, str)
      Implicit None
      Integer, Intent(in) :: N, u
      Character(LEN=11), Intent(in) :: str 
      Real, Dimension(0: N-1) :: rx1, ry1, rx2, ry2
      Real, Dimension(0: N-1) :: sx1, sy1, sx2, sy2
      Real, Dimension(0: N-1) :: x, y
      Integer :: i
      Open(unit=u, file=str)
      rx1(0)=0
      ry1(0)=0
      sx1(0)=0
      sy1(0)=0
      Do i=0, N-1
        rx1(i) = cos(DBLE(2)*pi*DBLE(i-N/2)/DBLE((N-1)))
        ry1(i) = 0
        sx1(i) = rx1(i)
        sy1(i)=0
      End do
      Do i=0, N-1
        rx2(i)=rx1(i)
        ry2(i)=ry1(i)
        sx2(i)=sx1(i)
        sy2(i)=sy2(i)
      End do

      Call CFFT(rx2, ry2, N, 8, 1)
      Call CFFT(sx2, sy2, N, 8, 1)
      Do i=0, N-1
        x(i)=(rx2(i)*sx2(i)+ry2(i)*sy2(i))/N
        y(i)=(-rx2(i)*sy2(i)+ry2(i)*sx2(i))/N
	Write(*,*) rx2(i), sx2(i)
      End do

      Call CFFT(x, y, N, 8, -1)
      
      Do i = 0, N-1
        Write(u,*) i, x(i), y(i)
      End do 
      End Subroutine cosineauto

      Subroutine sineauto(N, u, str)
      Implicit None
      Integer, Intent(in) :: N, u
      Character(LEN=11), Intent(in) :: str 
      Real, Dimension(0: N-1) :: rx1, ry1, rx2, ry2
      Real, Dimension(0: N-1) :: sx1, sy1, sx2, sy2
      Real, Dimension(0: N-1) :: x, y
      Integer :: i
      Open(unit=u, file=str)
      rx1(0)=0
      ry1(0)=0
      sx1(0)=0
      sy1(0)=0
      Do i=0, N-1
        rx1(i) = cos(DBLE(2)*pi*DBLE(i)/DBLE((N-1)))
        ry1(i) = 0
        sx1(i) = sin(DBLE(2)*pi*DBLE(i)/DBLE((N-1)))
        sy1(i)=0
	Write(*,*) rx1(i), sx1(i)
      End do
      Do i=0, N-1
        rx2(i)=rx1(i)
        ry2(i)=ry1(i)
        sx2(i)=sx1(i)
        sy2(i)=sy2(i)
      End do

      Call CFFT(rx2, ry2, N, 8, 1)
      Call CFFT(sx2, sy2, N, 8, 1)
      Do i=0, N-1
        x(i)=(rx2(i)*sx2(i)+ry2(i)*sy2(i))/N
        y(i)=(-rx2(i)*sy2(i)+ry2(i)*sx2(i))/N
      End do

      Call CFFT(x, y, N, 8, -1)
      
      Do i = 0, N-1
        Write(u,*) i, x(i), y(i)
      End do 
      End Subroutine sineauto
      SUBROUTINE CFFT( X, Y, N, M, ITYPE )
!c
!c  This is a very simple Cooley-Tukey Radix-2 DIF Complex FFT.
!c 
!c  The data sequence is of size N = 2**M.
!c  X and Y contain the real and imaginary parts of the data.
!c
!c  ITYPE .ne. -1 for forward transform
!c  ITYPE .eq. -1 for backward transform
!c
!c  The forward transform computes
!c     Z(k) = sum_{j=0}^{N-1} z(j)*exp(-2ijk*pi/N)
!c
!c  The backward transform computes
!c     z(j) = (1/N) * sum_{k=0}^{N-1} Z(k)*exp(2ijk*pi/N)
!c
!c
!c  Steve Kifowit, 31 October 1998
!c       
!c
!c ... Scalar arguments ...
      INTEGER  N, M, ITYPE
!c ... Array arguments ...
      REAL  X(*), Y(*)
!c ... Local scalars ...
      INTEGER  I, J, K, L, N1, N2, IE, IA
      REAL  C, S, XT, YT, P, TWOPI, A
!c ... Parameters ...
      PARAMETER  ( TWOPI = 6.283185307179586476925287 )
!c ... Intrinsic functions
      INTRINSIC  SIN, COS
!c
!c ... Exe. statements ...
!c
!c ... Quick return ...
      IF ( N .EQ. 1 ) RETURN
!c
!c ... Conjugate if necessary ...
      IF ( ITYPE .EQ. -1 ) THEN
	 DO 1, I = 1, N
	    Y(I) = - Y(I)
 1       CONTINUE
      ENDIF
!c
!c ... Main loop ...
      P = TWOPI / N
      N2 = N
      DO 10, K = 1, M
         N1 = N2
         N2 = N2 / 2
         IE = N / N1
         IA = 1
         DO 20, J = 1, N2
	    A = ( IA - 1 ) * P
            C = COS( A ) 
            S = SIN( A )
            IA = IA + IE
            DO 30, I = J, N, N1
               L = I + N2
               XT = X(I) - X(L)
               X(I) = X(I) + X(L)
               YT = Y(I) - Y(L)
               Y(I) = Y(I) + Y(L)
               X(L) = C * XT + S * YT
               Y(L) = C * YT - S * XT
 30         CONTINUE
 20      CONTINUE
 10   CONTINUE
!c
!c ... Bit reversal permutation ...
 100  J = 1
      N1 = N - 1
      DO 104, I = 1, N1
         IF ( I .GE. J ) GOTO 101
         XT = X(J)
         X(J) = X(I)
         X(I) = XT
         YT = Y(J)
         Y(J) = Y(I)
         Y(I) = YT
 101     K = N / 2
 102     IF ( K .GE. J ) GOTO 103
         J = J - K
         K = K / 2
         GOTO 102
 103     J = J + K
 104  CONTINUE
!c
!c ... Conjugate and normalize if necessary ...
      IF ( ITYPE .EQ. -1 ) THEN
	 DO 3, I = 1, N
	    Y(I) = - Y(I) / N
            X(I) = X(I) / N
 3       CONTINUE
      ENDIF
      RETURN
!c
!c ... End of subroutine CFFT ...
!c
      END
      End Program Problem2
