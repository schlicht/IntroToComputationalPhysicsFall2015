!######################################
! Name: Problem2
! Purpose: Determine Decay Rate
! History: Made by schli242 on 10/15/15 
!######################################

      Program Problem2
      Implicit None
      Real*8 , parameter :: prob=0.01
      Integer, parameter :: atoms=1000000
      open(unit=10, file="decay.dat")

      Call decay(atoms,prob)

      Contains
!Simulates the decay of the substance
      Subroutine decay (atoms, prob)
        Implicit None
        Integer, Intent(in) :: atoms
        Real*8, intent(in) :: prob
        Integer :: atomsleft, remaining, i=0, j=0
        Real*8 :: x
        atomsleft=atoms
!Repeats decay until there is only 1 atom left
! The j ensures that the loop doesn't go on forever
        Do while(atomsleft>1 .AND. j<10000)
! remaining is the number of atoms that i needs to iterate over
!during 1 unit time
          remaining = atomsleft
          i=0
          Do while(i<=remaining .And. atomsleft>0)
!Generates random number which, if less than prob, will
! decay an atom
            Call Random_Number(x)
            if(x<=prob) then
              atomsleft=atomsleft - 1
            end if
            i=i+1
          End do
          j=j+1
!Writes the number of atoms left after 1 unit time
          Write(10,*) j, Log(DBLE(atomsleft))
        End do

      End subroutine decay
      End Program Problem2
