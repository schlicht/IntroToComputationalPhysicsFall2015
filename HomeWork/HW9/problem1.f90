!###############################
! Name: Problem1
! Purpose:Apply Runge Kutta to an Anharmonic Oscillator
! History: Made by schli242 11/11/15
!###############################

      Program Problem1
      Implicit None
      Real*8, Dimension(1:2) :: x0=(/0.0, 25.13274123/)
      Integer :: N=3200
!Call runge Kutta and calculates the period
      Write(*,*) 'Period for x0=0.1:'
      Call rungeKutta(N, x0, DBLE(0.1), 10, 'prob1010000.dat')
      Write(*,*) 'Period for x0=0.5:'
      Call rungeKutta(N, x0, DBLE(0.5), 11, 'prob1050000.dat')
      Write(*,*) 'Period for x0=0.99:'
      Call rungeKutta(N, x0, DBLE(0.99), 12, 'prob1099000.dat')
      Write(*, *) "N-R was verified by checking that the zeros were at the transition from", &
      'negative values to positive values at x=0' 

      Contains
!Performs Runge Kutta
!N is the number of iterations, x0 are the boundary points, yinit is the initial value
!u is the output port and string is the output file
      Subroutine rungeKutta(N, x0, yinit, u, string)
      Character(LEN=15), Intent(in) :: string
      Real*8, Dimension(0:1), Intent(in) :: x0
      Real*8, Intent(in) :: yinit
      Integer, Intent(in) :: N, u
!Stores k values
      Real*8, Dimension(1:2, 1:4) :: k
!Arrays carrying x, y, v values
      Real*8, Dimension(1:N) :: x, y, v
      Real*8 :: dx, period
      Integer :: i
      Open(unit=u, file=string)
!Creates dx and x values
      dx=(x0(1)-x0(0))/N
      Do i = 1, N
        x(i)= x0(0)+dx*DBLE(i)
      End do
!Sets up initial values
      y(1)=yinit
      v(1)=0
!Executes Runge Kutta
      Do i=1, N
        k(1, 1) = dx*v(i)
        k(2, 1) = -dx*y(i)*(1-y(i))
        k(1, 2) = dx*(v(i)+0.5*k(2,1))
        k(2, 2) = -dx*(y(i)+ 0.5*k(1, 1))*(1-y(i)- 0.5*k(1, 1))
        k(1, 3) = dx*(v(i)+0.5*k(2, 2))
        k(2, 3) = -dx*(y(i)+0.5*k(1, 2))*(1-(y(i)+0.5*k(1, 2)))
        k(1, 4) = dx*(v(i)+k(2, 3))
        k(2, 4) = -dx*(y(i)+k(1, 3))*(1-(y(i)+k(1, 3)))
        y(i+1) = y(i)+ DBLE((k(1, 1)+2*k(1, 2)+2*k(1, 3)+k(1, 4))/6)
        v(i+1) = v(i)+ DBLE((k(2, 1)+2*k(2, 2)+2*k(2, 3)+k(2, 4))/6)
      End do
!Outputs data for graphing
      Do i=1, N
        Write(u,*) x(i), y(i)/yinit, v(i)/yinit
      End do
!Calculates period and initially looks for zero at points choosen by initial conditions
      if (u == 12) then
        period = findPeriod(N, y, v, x0, (/DBLE(6), DBLE(20)/))
      else
        period = findPeriod(N, y, v, x0, (/DBLE(1.5),DBLE(8)/))
      end if
      Write(*,*) period
      End subroutine rungeKutta
!Finds the period
! N is the length of the arrays, yi, and vi are the arrays containing the values, x0 are the
!boundary points, and guess contains guesses of where the zeros (choosen from data)
      Real*8 Function findPeriod(N, yi, vi, x0, guess)
      Implicit None
      Integer, Intent(in) :: N
      Real*8, Dimension(1:2), Intent(in) :: x0, guess
      Real*8, Dimension(1:N), Intent(in) :: yi, vi
      Real*8 :: eps = 10**(-2), v, y, x, x1=0, x2=0, lastx, dt
      Integer :: i, j, t

!Begins at guess
      x=guess(1)
!Iterates to a maximum of 100
      Do i = 1, 100
!Computes new x value
      t= Int(N*x/x0(2))
      v= (vi(t+1)+vi(t))/2
      y= yi(t)
      x= x-y/v
!Exits do loop when zero is found
      if (abs(y)<=eps) Exit
!Updates the last x value
      lastx=x
      End do
      x1=x

      !Begins at guess 2
      x=guess(2)

!Iterates to a maximum of 100
      Do i = 1, 100
!Computes new x value
      t= Int(N*x/x0(2))
      v= (vi(t+1)+vi(t))/2
      y= yi(t)
      x= x-y/v
!Exits do loop when zero is found
      if (abs(y)<=eps) Exit
!Updates the last x value
      lastx=x
      End do
      x2=x
!      Write(*,*) x2
      if(abs(x2-x1) >0.1) then
        findperiod = (x2-x1)
      end if 
      End function findPeriod
      End Program Problem1
