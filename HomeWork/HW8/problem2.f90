!#########################################
! Name: Problem2
! Purpose: Perform Predictor-Executer Method
! History: Made by schli242 on 11/8/15
!#########################################

      Program Problem2
      Implicit None
!Sets boundary of t values
      Real*8, Dimension(1:2) :: t0=(/ DBLE(0), DBLE(12.56637061) /)
!Outputs error to file
      Open(unit=9, file='prob2error.dat')
!Calls predictor method for N steps, over t0, with initial x0, writes to port u, and file x
      Call predictor(400, t0, DBLE(0.01), 10, 'prob200400.dat')
      Call predictor(800, t0, DBLE(0.01), 11, 'prob200800.dat')
      Call predictor(1600, t0, DBLE(0.01), 12, 'prob201600.dat')
      Call predictor(3200, t0, DBLE(0.01), 13, 'prob203200.dat')
      Call predictor(6400, t0, DBLE(0.01), 14, 'prob206400.dat')
      Call predictor(12800, t0, DBLE(0.01), 15, 'prob212800.dat')
      Write(*,*) "The predictor method converges more slowly and has a higher", &
                 " error than the Runge Kutta Method"
      Write(*,*) "Since error decreases as dt decreases over the range tested ", &
                 ", the optimal dt is much smaller where machine accuracy is important"
      Contains
      Subroutine predictor(N, t0, yinit, u, str)
      Implicit None
!Size and output port
      Integer, Intent(in) :: N, u
      Real*8, Dimension(1:2), Intent(in) :: t0
      Real*8, Intent(in) :: yinit
      Character(len=14), Intent(in) :: str
!vectors for predictor method
      Real*8, Dimension(1:2) :: b, c, d, e, f
!time vector, y vector, and velocity vector
      Real*8, Dimension(1:N) :: t, y, vy
!dt, value holders g and h, errors for calculating max error 
      Real*8 :: dt, g, h, merror, mverror, maxerror, maxverror
      Integer :: i
      Open(unit=u, file=str)
!vectors used for 3rd order
      b = (/0, 0/)
      c = (/ 3.0/2.0, -1.0/2.0 /)
      d = (/ 4.0/3.0, -1.0/3.0/)
      e = (/ 0.5, 0.5/)
      f = (/ 1.0/3.0, 2.0/3.0/)
!determines dt
      dt = (t0(2)-t0(1))/N
!fills t vector
      Do i=1, N
        t(i)=t0(1)+dt*i
      End do
!initial values
      y(1) = yinit
      vy(1) = 0
!executes method
      Do i=1, N
!predictor step
        g = c(1)*b(1)+ c(2)*b(2)
        h = d(1)*b(1) +d(2)*b(2)
        vy(i+1) = vy(i)+ dt*g
        y(i+1) = y(i)+vy(i)*dt + 0.5*(dt**2)*h
!exact value step
        b(2)=b(1)
        b(1) = -y(i+1)
        g = e(1)*b(1) + e(2)*b(2)
        h = f(1)*b(1) + f(2)*b(2)
        vy(i+1) = vy(i) + dt*g
        y(i+1) = y(i) + vy(i)*dt + 0.5*(dt**2)*h
      End do
!writes values to file
      Do i=1, N
        Write(u, *) t(i), y(i)
      End do
!calculates max error
      maxerror=-100
      maxverror=-100
      Do i=1, N
        merror=log(abs(y(i)-0.01*cos(t(i-1)))*10**(2))
        mverror=log(abs(vy(i)+0.01*sin(t(i-1)))*10**(2))
        if(merror>maxerror) then
          maxerror= merror
        end if
        if(mverror>maxverror) then
          maxverror =mverror
        end if
      End Do
!outputs max error to file
      Write(9,*) log(dt), maxerror, maxverror
      End subroutine predictor
      End Program Problem2
