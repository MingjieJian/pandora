      subroutine PLAY
     $(XLAM,YNT,FAN)
C
C     Rudolf Loeser, 1973 Apr 03
C---- Computes intensity /A  from intensity /Hz.
C     !DASH
      save
C     !DASH
      real*8 CON28, FAN, HNDRD, TWO, XLA, XLAM, XLI, YNT, ZERO
      integer LUEO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external DIVIDE, RIGEL, MESHED, MASHED, HI, BYE
C
      data HNDRD /1.D2/
C
      call HI ('PLAY')
C     !BEG
      if((YNT.le.ZERO).or.(XLAM.le.ZERO)) then
        FAN = ZERO
      else
C
        XLI = log10(YNT)
        XLA = log10(XLAM)
C
        if((XLI-TWO*XLA).gt.HNDRD) then
          call MESHED ('PLAY', 3)
          write (LUEO,100) XLAM,YNT
  100     format(' ','PLAY: F /cm',1P2E20.12)
          call MASHED ('PLAY')
C
          FAN = ZERO
        else
C
          call RIGEL  (28, CON28)
          call DIVIDE ((CON28*YNT), (XLAM**2), FAN)
        end if
      end if
C     !END
      call BYE ('PLAY')
C
      return
      end
