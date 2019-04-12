      subroutine GALAGO
     $(N,WHY,ELL,GEE,Z,HEABD,EFF,ZR,FR,YR,IHEAB)
C
C     Rudolf Loeser, 1991 Jan 04
C---- Computes F and G for THALIA.
C     !DASH
      save
C     !DASH
      real*8 CMPKM, CON, EFF, ELL, EY, FR, GEE, HEABD, WHY, YR, Z, ZR
      integer I, IHEAB, N
C     !COM
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
      external LIMEXP, WATER, CONMUL, HI, BYE
C
C               WHY(N), ELL(N), GEE(N), EFF(N), ZR(N), FR(N), YR(N),
      dimension WHY(*), ELL(*), GEE(*), EFF(*), ZR(*), FR(*), YR(*),
C
C               Z(N)
     $          Z(*)
C
      call HI ('GALAGO')
C     !BEG
      do 100 I = 1,N
        call LIMEXP (WHY(I),EY)
        GEE(I) = EY*ELL(I)
  100 continue
      call WATER    (Z,GEE,EFF,N,ZR,FR,YR,IHEAB)
C
      CON = CMPKM/HEABD
      call CONMUL   (CON,EFF,N)
C     !END
      call BYE ('GALAGO')
C
      return
      end
