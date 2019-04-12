      subroutine MONA
     $(WVL,R1N,AVI,XLA)
C
C     Rudolf Loeser, 1981 Sep 17
C---- Computes Luminosity from AVI ( = FHZ/Pi).
C     !DASH
      save
C     !DASH
      real*8 AVI, CON, R1N, R2, W2, WVL, XLA, ZERO
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external RIGEL, HI, BYE
C
      call HI ('MONA')
C     !BEG
      R2 = R1N**2
      W2 = WVL**2
      if((R2.eq.ZERO).or.(W2.eq.ZERO)) then
        XLA = ZERO
      else
        call RIGEL (40,CON)
        XLA = (CON*R2)/W2
      end if
C     !END
      call BYE ('MONA')
C
      return
      end
