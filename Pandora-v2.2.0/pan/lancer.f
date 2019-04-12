      subroutine LANCER
     $(LEVEL,MODE)
C
C     Rudolf Loeser, 1980 Mar 04
C---- Determines whether the current level is the KSHELL or not,
C     for CORIAND.
C     Returns with MODE=2 if yes, =1 if no.
C     (This is version 2 of LANCER.)
C     !DASH
      save
C     !DASH
      integer LEVEL, MODE, NSL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(40),NSL)
C     !DASH
      external HI, BYE
C
      call HI ('LANCER')
C     !BEG
      MODE = 1
      if(LEVEL.gt.NSL) then
        MODE = 2
      end if
C     !END
      call BYE ('LANCER')
C
      return
      end
