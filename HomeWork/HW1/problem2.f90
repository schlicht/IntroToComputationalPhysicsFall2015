!################################
! Name: Problem2
! Purpose:Determines the smallest single
! and double point floats which can be stored 
! History: made by schli242 on 9/13/15
! Notes: Running on a Optiplex3020 (from Keller 1-200)
!################################

      Program Problem2
      Implicit None

!The variables used are defined.
      Real*4 :: b4 = 1, lastb4
      Real*8 :: b8 = 1, lastb8

! Loop halves the value of the single precision number in  each iteration
! until the value becomes zero.
! The smallest value larger than zero is printed.
      Do
      If(b4 == 0) Exit
      lastb4 = b4
      b4 = b4*0.5
      End Do
      Write(*,*) 'Smallest 4 byte float is ', lastb4

!The same routine is run except with double precision.
      Do
      If(b8 == 0) Exit
      lastb8 = b8
      b8 = b8*0.5
      End DO
      Write(*,*) 'Smallest 8 byte float is ', lastb8
      End Program
