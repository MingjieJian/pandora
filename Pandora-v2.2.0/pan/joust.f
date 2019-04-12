      subroutine JOUST
     $(LIM,LUM)
C
C     Rudolf Loeser, 1980 Feb 29
C---- Sets up tabulating increments.
C     (This is version 2 of JOUST.)
C     !DASH
      save
C     !DASH
      integer I, J, LIM, LUM
C     !DASH
      external HI, BYE
C
C               LUM(3)
      dimension LUM(*)
C
      call HI ('JOUST')
C     !BEG
      I = LIM/3
      J = 3*I
C
      LUM(1) = I
      if((LIM-J).ge.1) then
        LUM(1) = I+1
      end if
C
      LUM(2) = I
      if((LIM-J).eq.2) then
        LUM(2) = I+1
      end if
C
      LUM(3) = I
C     !END
      call BYE ('JOUST')
C
      return
      end
