      subroutine GABOR
     $(LU,KODE,MODE,LINE)
C
C     Rudolf Loeser, 2000 Jun 21
C---- Prints "omitted plot" message.
C     !DASH
      save
C     !DASH
      integer KNT, KODE, LU, MODE
      character LINE*(*)
C     !DASH
      external  ABJECT, LINER, HI, BYE
      intrinsic min, abs
C
      call HI ('GABOR')
C     !BEG
      if(LU.gt.0) then
        if(KODE.eq.0) then
          call ABJECT (LU)
        else
          KNT = min(abs(KODE),10)
          call LINER  (KNT,LU)
        end if
C
        if(MODE.eq.0) then
          write (LU,100) LINE, ' it is uninformative.'
  100     format(' ','The plot labelled:'//
     $           ' ',2X,A//
     $           ' ','which usually comes next, is omitted because',A)
        else
          write (LU,100) LINE, ' of abscissa problems.'
        end if
      end if
C     !END
      call BYE ('GABOR')
C
      return
      end
