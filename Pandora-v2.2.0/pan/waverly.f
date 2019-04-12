      subroutine WAVERLY
     $(IX,XJBAR,KRJ)
C
C     Rudolf Loeser, 1996 Apr 05
C---- Sets selection switch for statistical equilibrium terms.
C     !DASH
      save
C     !DASH
      real*8 XJBAR
      integer IX, KRJ, MRA
      logical GOOD
C     !DASH
      external MUSTY, PINYON, HI, BYE
C
      dimension IX(*)
C
C               XJBAR(N,NT)
      dimension XJBAR(*)
C
      call HI ('WAVERLY')
C     !BEG
      call PINYON (XJBAR,GOOD)
      call MUSTY  (IX, MRA)
C
C---- Use "from Jbar" if possible
      KRJ = 1
      if((.not.GOOD).and.(MRA.eq.2)) then
C----   Since Jbar is bad, use "from Rho" instead
        KRJ = 3
      end if
C     !END
      call BYE ('WAVERLY')
C
      return
      end
