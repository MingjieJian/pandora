      subroutine IMGNCOD
     $(JS,JE,LEGEND)
C     Rudolf Loeser, 1997 Apr 14
C---- Encodes a legend, for IMGPRNT.
C     !DASH
      save
C     !DASH
      integer JE, JS
      character BLANK*1, DASH*1, LABE*6, LABS*6, LEGEND*13, POOF*6
C     !DASH
      data BLANK, DASH, POOF /' ', '-', '******'/
C
C     !BEG
      LABS = POOF
      LABE = POOF
C
  100 format(I6)
      if((JS.ge.0).and.(JS.lt.1000000)) then
        write (LABS,100) JS
      end if
      if((JE.gt.0).and.(JE.lt.1000000)) then
        write (LABE,100) JE
      end if
C
  101 continue
        if(LABE(1:1).eq.BLANK) then
          LABE = LABE(2:)//BLANK
          goto 101
        end if
C
      LEGEND = LABS//DASH//LABE
C     !END
C
      return
      end
