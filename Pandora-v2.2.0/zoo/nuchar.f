      subroutine NUCHAR
     $(BUFFER,LAST,CHAR)
C     Rudolf Loeser, 1978 Nov 29
C---- Obtains CHAR, the next character out of BUFFER, for NUDEAL.
C     !DASH
      save
C     !DASH
      integer EOB, IPR, IRD, KARD, KART, LAST, LUIN, LUOUT
      character BLANK*1, BUFFER*80, CHAR*1, ESC*1, MINUS*1, PLUS*1,
     $          POINT*1, XF*1
C     !COM
      common /NUDEEL/ LUIN,IRD,IPR,LUOUT,KARD,KART
      common /NUCONT/ BLANK,POINT,PLUS,MINUS,ESC,XF
C     !DASH
      external NUCARD
C
      dimension XF(4)
C
      data EOB /-1/
C
C     !BEG
      if((IRD.gt.0).or.(EOB.gt.0)) then
C----   Read next line
        call NUCARD (BUFFER,LAST)
C----   Reset read-switch
        IRD = 0
C----   Reset end-of-buffer switch
        EOB = 0
C----   Reset character-scan-pointer
        LAST = 0
      end if
C
      LAST = LAST+1
C
      if(LAST.gt.80) then
C----   End of buffer: set switch, and return dummy character
        EOB  = 1
        CHAR = BLANK
      else
C----   Return current valid character
        CHAR = BUFFER(LAST:LAST)
      end if
C     !END
C
      return
      end
