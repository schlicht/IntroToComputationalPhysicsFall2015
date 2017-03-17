!####################################
! Name: Problem3
! Purpose: Determine the digits of precision for single 
! and double precision
! History: Made by schli242 on 9/14/15
! Notes: Using a Dell Optiplex 3020 (from Keller 1-200)
!####################################

      Program Problem3
      Implicit None
! Declares the iterator, and the x,y,z, and epsilon values for
! single and double precision.
      Integer:: i=1
      Real*4 :: xsingle, ysingle, zsingle, esingle
      Real*8 :: xdouble, ydouble, zdouble, edouble
! Starts epsine at 1
      esingle = 1.0
! Halves epsilon until it becomes zero. Subtracts y from x and puts
! value into z. Then checks to see if z is zero.
      Do
      esingle = esingle*0.5
      xsingle = 1.0+esingle
      ysingle = 1.0-esingle
      zsingle = xsingle - ysingle
      Write(*, *)  i,  zsingle
      If(zsingle==0) Exit
      i = i +1
      End do
! Writes the number of digits of precision
      Write(*,*) 'Therefore, single precision has 8 digits of precision'
! Repeats the procedure for double precision
      edouble = 1.0
      i = 1
      Do
      edouble = edouble*0.5
      xdouble = 1.0+edouble
      ydouble = 1.0-edouble
      zdouble = xdouble - ydouble
      Write(*,*) i, zdouble
      If(zdouble==0) Exit
      i=i+1
      End do

      Write(*,*) 'Therefore, double precision has 16 digits of precison'
      End Program
