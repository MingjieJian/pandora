      subroutine ELENA
     $(NO,LINE,LINES,LINIT,IJECT,LFB,LFBV)
C
C     Rudolf Loeser, 1980 Nov 14
C---- Prints a line, for LEDGER.
C     !DASH
      save
C     !DASH
      integer IJECT, LFB, LFBV, LINES, LINIT, NO
      character FACELAB*10, LINE*127
C     !DASH
      external LINER, ABJECT, TUMBLE, HI, BYE
C
      call HI ('ELENA')
C     !BEG
      if(LINES.gt.50) then
        if(IJECT.eq.1) then
          call ABJECT (NO)
        end if
C
        if(LFBV.eq.2) then
          call TUMBLE (LFB,FACELAB)
          write (NO,100) FACELAB
  100     format(' ',5('***********'),3X,A10,4X,5('***********'))
          call LINER  (1,NO)
        end if
C
        write (NO,101)
  101   format(' ',5X,'Depth',4X,'( F   KS  KI)',6X,'Lambda',10X,'TE',
     $            10X,'TB',5X,'Intensity')
        call LINER    (1,NO)
C
        LINES = LINIT
        LINIT = 0
        IJECT = 1
      end if
C
      write (NO,102) LINE
  102 format(' ',A)
C
      LINES = LINES+1
C     !END
      call BYE ('ELENA')
C
      return
      end
