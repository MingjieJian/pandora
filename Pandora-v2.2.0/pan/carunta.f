      subroutine CARUNTA
     $(INDX,XLM,HE1N,N,NOPAC,CONT)
C
C     Rudolf Loeser, 1988 Feb 04
C---- Computes a set of He Rayleigh scattering opacity values (29).
C     (This is version 2 of CARUNTA.)
C     !DASH
      save
C     !DASH
      real*8 CONT, F, HE1N, SIGMA, XLM, XMBARN
      integer I, INDX, N, NOPAC
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
      external AMBA, HI, BYE
C
C               HE1N(N,Limp), CONT(Nopac,N)
      dimension HE1N(N,*),    CONT(NOPAC,*)
C
      call HI ('CARUNTA')
C     !BEG
      call AMBA (XLM, SIGMA)
      F = SIGMA*XMBARN
C
      do 100 I = 1,N
        CONT(INDX,I) = F*HE1N(I,1)
  100 continue
C     !END
      call BYE ('CARUNTA')
C
      return
      end
