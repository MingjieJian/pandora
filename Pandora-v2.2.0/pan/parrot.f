      subroutine PARROT
     $(TE,AMASS,V2,RT,TERM)
C
C     Rudolf Loeser, 2004 Jun 14
C---- Computes the frequency-independent TERM in the expression
C     for Doppler Width, and
C     also returns separately the value of the squareroot term, RT.
C
C---- Given:  TE    - temperature (K)
C             V2    - square of velocity ([km/s]**2)
C             AMASS - mass (relative to Hydrogen)
C     !DASH
      save
C     !DASH
      real*8 AMASS, CON26, CON48, RT, TE, TERM, V2, XM
C     !DASH
      external RIGEL, HI, BYE
C
      call HI ('PARROT')
C     !BEG
      call RIGEL (48, CON48)
      call RIGEL (26, CON26)
      XM = CON26*(TE/AMASS)
      RT = sqrt(XM+V2)
C
      TERM = CON48*RT
C     !END
      call BYE ('PARROT')
C
      return
      end
