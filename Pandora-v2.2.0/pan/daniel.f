      subroutine DANIEL
     $(NO,LINE,YUL,YLL)
C
C     Rudolf Loeser, 2006 Jul 27
C---- Writes a plot error message.
C     (This is version 2 of DANIEL.)
C     !DASH
      save
C     !DASH
      real*8 YLL, YUL
      integer NO
      character LINE*(*)
C     !DASH
      external GABOR, LINER, HI, BYE
C
      call HI ('DANIEL')
C     !BEG
      if(NO.gt.0) then
        call GABOR (NO, 3, 1, LINE)
        call LINER (1, NO)
        write (NO,100) 'Lower', YLL
  100   format(' ',A,' abscissa limit =',1PE14.6)
        write (NO,100) 'Upper', YUL
      end if
C     !END
      call BYE ('DANIEL')
C
      return
      end
