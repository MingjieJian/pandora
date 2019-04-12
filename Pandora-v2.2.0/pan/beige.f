      subroutine BEIGE
     $(IMAGE,X,Y,SYM,LINC)
C
C     Rudolf Loeser, 1978 Jan 27
C---- Checks and enters a plot point.
C     (Third, completely new and revised version.)
C     !DASH
      save
C     !DASH
      real*8 X, Y
      integer JX, JY, LINC
      logical IS_INSIDE
      character IMAGE*(*), SYM*1
C     !COM
C---- BASH        as of 1984 Apr 19
      integer            JXLOBA,JXHIBA,JYLOBA,JYHIBA,NTHRBA,NPOIBA
      common      /BASH/ JXLOBA,JXHIBA,JYLOBA,JYHIBA,NTHRBA,NPOIBA
C     Control data for "BEIGE".
C     .
C     !DASH
      external KWHERE, LINK, HI, BYE
C
      call HI ('BEIGE')
C     !BEG
      call KWHERE (IMAGE,X,Y,JX,JY)
C
      IS_INSIDE = (JX.gt.JXLOBA).and.(JX.lt.JXHIBA).and.
     $            (JY.gt.JYLOBA).and.(JY.lt.JYHIBA)
      if(IS_INSIDE) then
        NPOIBA = NPOIBA+1
      end if
C
      call LINK   (IMAGE,X,Y,SYM,LINC)
C     !END
      call BYE ('BEIGE')
C
      return
      end
