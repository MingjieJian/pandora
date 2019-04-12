      subroutine NUCARD
     $(BUFFER,LAST)
C     Rudolf Loeser, 1978 Nov 29
C---- Reads a new line, for NUDEAL.
C     !DASH
      save
C     !DASH
      integer IPR, IRD, KARD, KART, LAST, LUIN, LUOUT
      character BUFFER*80
C     !COM
      common /NUDEEL/ LUIN,IRD,IPR,LUOUT,KARD,KART
C
C     !BEG
      read (LUIN,100) BUFFER
  100 format(A80)
C
C---- Increment lines counters
      KARD = KARD+1
      KART = KART+1
C
      if(IPR.gt.0) then
C----   Dump this new line
        write (LUOUT,101) KART,KARD,BUFFER
  101   format(' (',2I6,') ',A80)
      end if
C
C     !END
C
      return
      end
