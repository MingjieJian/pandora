      subroutine RUNE
     $(DIFMX,JMX,IMX)
C
C     Rudolf Loeser, 1983 Nov 01
C---- Prints error message for MOREL.
C     (This is version 2 of RUNE.)
C     !DASH
      save
C     !DASH
      real*8 DIFMX
      integer IMX, JMX, NO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C     !DASH
      external LINER, HI, BYE
C
      call HI ('RUNE')
C     !BEG
      call LINER (5,NO)
      write (NO,100) DIFMX,JMX,IMX
  100 format(' ','BD iteration did not converge'/
     $       ' ','DIFMX=',1PE12.4,', for level',I3,', at depth',I3)
C     !END
      call BYE ('RUNE')
C
      return
      end
