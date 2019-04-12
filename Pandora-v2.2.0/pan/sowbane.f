      subroutine SOWBANE
     $(FACT,FUJ)
C
C     Rudolf Loeser, 1995 Apr 24
C---- Encodes opacity multiplier, for BLURT.
C     (This is version 2 of SOWBANE.)
C     !DASH
      save
C     !DASH
      real*8 C01, C100, FACT
      character FUJ*7
C     !DASH
      external HI, BYE
C
      data C100, C01 /1.D+2, 1.D-2/
C
      call HI ('SOWBANE')
C     !BEG
      if((FACT.ge.C100).or.(FACT.le.C01)) then
        FUJ = '   ----'
      else
        write (FUJ,100) FACT
  100   format(F7.3)
      end if
C     !END
      call BYE ('SOWBANE')
C
      return
      end
