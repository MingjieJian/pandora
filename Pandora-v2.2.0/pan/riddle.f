      subroutine RIDDLE
     $(I,RJ)
C
C     Rudolf Loeser, 1987 Feb 23
C---- Dumps, for SINEW.
C     (This is version 2 of RIDDLE.)
C     !DASH
      save
C     !DASH
      real*8 RJ
      integer I, LUEO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external ABJECT, LINER, HI, BYE
C
      call HI ('RIDDLE')
C     !BEG
      call LINER (2, LUEO)
      write (LUEO,100) I,RJ
  100 format(' ','I =',I3,4X,'RJ =',1PE14.6)
      call LINER (1, LUEO)
C     !END
      call BYE ('RIDDLE')
C
      return
      end
