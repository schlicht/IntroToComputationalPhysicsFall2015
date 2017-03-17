!###################################
! Name: Proplem1
! Purpose: Prints the largest single and 
! double precision numbers and the largest 
! integer
! History: Made by schli242 on 9/14/15
!###################################

      Program Problem1
      Implicit None

! Defines the iterator and final answer for single and double precision
! and integer
      Real*4 :: numberb4= 1, lastnumberb4 
      Real*8 :: numberb8 = 1, lastnumberb8
      Integer :: i=1, lasti

! Multiplies by 2 until numberb4 becomes infinity. Returns next largest
! number.
      Do
      numberb4 = numberb4*2
      If(numberb4/numberb4 /= 1) Exit
      lastnumberb4 = numberb4
      End Do
      Write(*,*) 'The largest single precision number is ', lastnumberb4

! Repeats previous procedure for doulbe precision
      Do
      numberb8 = numberb8*2
      If(numberb8/numberb8 /= 1) Exit
      lastnumberb8 = numberb8
      End Do
      Write(*,*) 'The largest double precision number is ', lastnumberb8

! Repeats previous procedure for single precision 
      Do
      i = i*2
      If(i <= 0) Exit
      lasti = i
      End Do
      Write(*,*) 'The largest integer is ', lasti

      End Program
