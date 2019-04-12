      subroutine BROCAS
     $(LU,KSR,CRIT,MXKOUNT)
C
C     Rudolf Loeser, 1985 Aug 22
C---- Prints error message, for SHIVA.
C     (This is version 2 of BROCAS.)
C     !DASH
      save
C     !DASH
      real*8 CRIT
      integer KSR, LU, MXKOUNT
C     !DASH
      external LINER, HI, BYE
C
      call HI ('BROCAS')
C     !BEG
      if((KSR.gt.0).and.(LU.gt.0)) then
        call LINER (1,LU)
        write (LU,100) CRIT,MXKOUNT
  100   format(' ',9X,'$ indicates that the search did not converge to',
     $             1PE10.2,' in',I3,' iterations.')
      end if
C     !END
      call BYE ('BROCAS')
C
      return
      end
