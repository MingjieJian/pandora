      subroutine JAVELIN
     $(LIM,LUM)
C
C     Rudolf Loeser, 1980 Feb 29
C---- Sets up tabulating increments, for EGRET.
C     (This is version 2 of JAVELIN.)
C     !DASH
      save
C     !DASH
      integer J, L, LIM, LUM
C     !DASH
      external HI, BYE
C
      dimension LUM(2)
C
      call HI ('JAVELIN')
C     !BEG
      L = LIM/2
      J = 2*L
C
      LUM(1) = L
      if((LIM-J).eq.1) then
        LUM(1) = L+1
      end if
C
      LUM(2) = L
C     !END
      call BYE ('JAVELIN')
C
      return
      end
