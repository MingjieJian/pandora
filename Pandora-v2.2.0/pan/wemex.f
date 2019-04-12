      subroutine WEMEX
     $(NO,DAMPY,YCONT,MWNSV)
C
C     Rudolf Loeser, 2004 Apr 17
C---- Prints explanation of weight matrix method.
C     !DASH
      save
C     !DASH
      real*8 DAMPY, YCONT
      integer MWNSV, NO
C     !DASH
      external LINER, PLANE, HI, BYE
C
      call HI ('WEMEX')
C     !BEG
C---- Full or direct solution
      if(MWNSV.eq.0) then
        call PLANE (NO, DAMPY)
      else
        call PLANE (NO, YCONT)
        call LINER (1, NO)
        write (NO,101) MWNSV
  101   format(' ',I6,' Weight Matrices from the Continuum JNU ',
     $             'Calculations were saved, for use here in the line ',
     $             'source function calculation.')
      end if
C     !END
      call BYE ('WEMEX')
C
      return
      end
