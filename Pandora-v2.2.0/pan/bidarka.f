      subroutine BIDARKA
     $(INDX,XLM,N,NOPAC,H2N,CONT)
C
C     Rudolf Loeser, 1988 Oct 25
C---- Computes a set of H2 Rayleigh scattering opacity values.
C     (This is version 2 of BIDARKA.)
C     !DASH
      save
C     !DASH
      real*8 CONT, FAC, H2N, SIGMA, XLM, XMBARN
      integer INDX, J, N, NOPAC
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 4),XMBARN)
C     !DASH
      external SIKA, HI, BYE
C
C               H2N(N), CONT(Nopac,N)
      dimension H2N(*), CONT(NOPAC,*)
C
      call HI ('BIDARKA')
C     !BEG
      call SIKA (XLM, SIGMA)
      FAC = SIGMA*XMBARN
C
      do 100 J = 1,N
        CONT(INDX,J) = FAC*H2N(J)
  100 continue
C     !END
      call BYE ('BIDARKA')
C
      return
      end
