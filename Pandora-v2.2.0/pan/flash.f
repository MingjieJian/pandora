      subroutine FLASH
     $(T,XLM,LIMP,F)
C
C     Rudolf Loeser, 1973 Jun 06
C---- Intermediate term for Hydrogen bound-free opacity.
C     !DASH
      save
C     !DASH
      real*8 AT, B, CON58, D, E, ELL2, F, FX, FY, ONE, OZ, RYDBRG, T, X,
     $       XLM, Y, Z, dummy
      integer LIMP, LSMP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 4),RYDBRG)
C     !DASH
C     !EJECT
      external  RIGEL, HUNK, QEXP1, DIVIDE, HI, BYE
      intrinsic max
C
      data LSMP /0/
C
      call HI ('FLASH')
C     !BEG
      if(LIMP.ne.LSMP) then
        LSMP = LIMP
        ELL2 = LIMP**2
        B    = ONE/ELL2
        D    = (LIMP+1)**2
      end if
C
      Z = max((XLM/RYDBRG),D)
      call HUNK   (T, XLM, 2, X)
      call QEXP1  (X, dummy, 2, FX)
      call RIGEL  (58, CON58)
      call DIVIDE (CON58, (T*Z), Y)
      call QEXP1  (Y, dummy, 2, FY)
      call DIVIDE (CON58, T, AT)
      call DIVIDE (ONE, Z, OZ)
      E = exp(-(AT*(B-OZ)))
C
      F = FX*FY*E
C     !END
      call BYE ('FLASH')
C
      return
      end
