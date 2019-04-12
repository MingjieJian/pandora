      subroutine TITI
     $(N,Z,TE,TE4,DTE4,ROSSK,TEFF)
C
C     Rudolf Loeser, 1986 Mar 17
C---- Computes effective temperature.
C     !DASH
      save
C     !DASH
      real*8 CMPKM, CWARTR, DTE4, FAC, FOUR, RAT, ROSSK, TE, TE4, TEFF,
     $       THREE, Z, ZERO
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(14),CWARTR)
      equivalence (DLIT( 4),THREE )
      equivalence (DLIT( 5),FOUR  )
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 5),CMPKM )
C     !DASH
C     !EJECT
      external DERIV1, DIVIDE, HI, BYE
C
C               Z(N), TE(N), TE4(N), DTE4(N), ROSSK(N), TEFF(N)
      dimension Z(*), TE(*), TE4(*), DTE4(*), ROSSK(*), TEFF(*)
C
      call HI ('TITI')
C     !BEG
      do 100 I = 1,N
        TE4(I) = TE(I)**4
  100 continue
      call DERIV1   (Z, TE4, DTE4, N)
C
      FAC = FOUR/THREE
      do 101 I = 1,N
        call DIVIDE ((DTE4(I)/CMPKM),ROSSK(I),RAT)
C
        if(RAT.eq.ZERO) then
          TEFF(I) = ZERO
        else if(RAT.gt.ZERO) then
          TEFF(I) = (FAC*RAT)**CWARTR
        else
          TEFF(I) = -FAC
        end if
  101 continue
C     !END
      call BYE ('TITI')
C
      return
      end
