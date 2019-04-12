      subroutine LUMMET
     $(XLM,XL,H1,N,W,D,AN,SA,STKFN,XX,AA,PN,T,G,STOP)
C
C     Rudolf Loeser, 2003 Jan 09
C---- Adds the next term to the sum "g", for Mullet.
C     !DASH
      save
C     !DASH
      real*8 AA, AN, ANGPCM, CLIGHT, CLN, CRIT, D, EIGHT, EN, FOUR, G,
     $       H1, ONE, PI, PN, ROOTPI, SA, STKFN, T, THREE, TT, W, XL,
     $       XLM, XX, ZERO
      integer N
      logical STOP
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 3),CLIGHT)
      equivalence (TUNI( 6),ANGPCM)
      equivalence (TUNI( 2),ROOTPI)
      equivalence (TUNI( 1),PI    )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 4),THREE )
      equivalence (DLIT( 5),FOUR  )
      equivalence (DLIT( 9),EIGHT )
C     !DASH
      external  DVOIGT, HI, BYE
      intrinsic abs
C
      data CRIT /1.D-03/
C     !EJECT
C
      call HI ('LUMMET')
C     !BEG
      CLN = XLM/ANGPCM
      EN  = N
      TT  = (THREE*AN*(CLN**3)*EN*H1)/(EIGHT*(PI**2))
C
      XX = (abs(XLM-W)/XLM)*(CLIGHT/XL)
      AA = (((ONE/(FOUR*PI))*(SA+TT))+STKFN)/(XL/CLN)
      call DVOIGT (XX, AA, PN)
C
      T = (ROOTPI*PN)*D
      G = G+T
C
      STOP = (T/G).lt.CRIT
C     !END
      call BYE ('LUMMET')
C
      return
      end
