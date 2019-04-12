      subroutine NIOBE
     $(IU,IL,TE,AIJ,P,XNU,IONST,CEIJ)
C
C     Rudolf Loeser, 1992 Jan 14
C---- Computes CEIJ, the collisional excitation coefficient for
C     transitions from level IU to level IL, IU > IL, according to
C
C     H .   v a n   R e g e m o r t e r
C
C     (1962), ApJ, 136, 906.
C
C---- Input:
C
C     TE    - electron temperature
C     AIJ   - Einstein A coefficient for this transition
C     P     - statistical weights
C     XNU   - level frequencies
C     IONST - stage of ionization
C
C---- Output:
C
C     CEIJ  - collisional excitation parameter
C     !DASH
      save
C     !DASH
      real*8 AIJ, ANGPCM, CEIJ, CON4, DNU, FAC, FIJ, P, PRAT, PT, RT,
     $       TE, WORT, XLM, XNU
      integer IL, IONST, IU
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 6),ANGPCM)
C     !DASH
C     !EJECT
      external DENIA, ANGIE, RIGEL, DIVIDE, HI, BYE
C
C               XNU(NSL), P(NSL)
      dimension XNU(*),   P(*)
C
      data FAC /2.06D1/
C
      call HI ('NIOBE')
C     !BEG
      DNU = XNU(IU)-XNU(IL)
      call ANGIE  (DNU, XLM)
      XLM = XLM/ANGPCM
      call DIVIDE (P(IU), P(IL), PRAT)
      call RIGEL  (4, CON4)
      call DIVIDE (CON4, TE, PT)
      call DENIA  ((DNU*PT), IONST, FIJ)
      RT  = sqrt(TE)
      call DIVIDE ((XLM**3), RT, WORT)
C
      CEIJ = FAC*WORT*PRAT*AIJ*FIJ
C     !END
      call BYE ('NIOBE')
C
      return
      end
