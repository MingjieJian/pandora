      subroutine COLOBA
     $(N,NPQ,LRQ,CXX,CXXP,G,TE,CCHX,ICXDP,J,DUMP)
C
C     Rudolf Loeser, 1990 Nov 29
C---- Computes CXX and CXXP for a particular level
C     of the ion-of-the-run, which has n = NPQ and l = LRQ,
C     for upper-level charge exchange.
C     !DASH
      save
C     !DASH
      real*8 BOLZMN, CCHX, CXX, CXXP, DLNL, ELL, ELP, EMD, EN2, ENN,
     $       FAC, FX, FXP, G, ONE, RCNL, RT, SEVEN, SX, SXP, TE, TWO,
     $       XNINE
      integer I, ICXDP, J, LRQ, N, NPQ
      logical DMPI, DUMP, SMELL
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 2),BOLZMN)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
      equivalence (DLIT( 8),SEVEN )
      equivalence (DLIT(10),XNINE )
C
C---- XINGU       as of 1999 Sep 21
      real*8      AXED,BXED,RCHX,DELCHX
      character   NAMXED*3
      integer     NXI,NPQLM,NPQMX
      parameter   (NXI=10)
C     (Remember to change all users when changing NXI)
      parameter   (NPQLM=15)
C     (Maximum permitted value of principal quantum number n)
C     (NPQLM must not exceed LIMDAT(1) [in popdata.inc], the
C     number of levels in the Hydrogen population ion model.)
      dimension   AXED(NXI), BXED(NXI), NAMXED(NXI)
      dimension   RCHX(NPQLM,NPQLM), DELCHX(NPQLM,NPQLM)
      common      /XINGU1/ AXED,BXED,RCHX,DELCHX
      common      /XINGU2/ NAMXED
      common      /XINGU3/ NPQMX
C---- Charge Exchange data tables
C     .
C     !DASH
C     !EJECT
      external GULA, HI, BYE
C
C               CXXP(N), G(N), CXX(N), TE(N)
      dimension CXXP(*), G(*), CXX(*), TE(*)
C
      data FAC /5.75D-11/
C
      call HI ('COLOBA')
C     !BEG
      ELL   = LRQ
      ENN   = NPQ
      EN2   = ENN**2
      ELP   = TWO*ELL+ONE
      RCNL  = RCHX(NPQ,LRQ)
      DLNL  = DELCHX(NPQ,LRQ)
      SMELL = LRQ.eq.(-1)
      if(SMELL) then
        SXP = CCHX*FAC*EN2*(EN2-XNINE)
        if(ENN.le.SEVEN) then
          SX = EN2/(EN2-XNINE)
        else
          SX = ONE
        end if
      else
        FXP = CCHX*FAC*EN2*ELP
        FX  = EN2/ELP
      end if
C
      do 100 I = 1,N
        DMPI = DUMP.and.(I.eq.ICXDP)
        call GULA  (G(I), TE(I), RCNL, DLNL, ENN, ELL, ICXDP, J, DMPI)
        RT  = sqrt(TE(I))
        EMD = exp(-DLNL/(BOLZMN*TE(I)))
C
        if(SMELL) then
          CXXP(I) = SXP*RT*G(I)
          CXX (I) = SX*CXXP(I)
        else
          CXXP(I) = FXP*RT*G(I)
          CXX (I) = FX*EMD*CXXP(I)
        end if
  100 continue
C     !END
      call BYE ('COLOBA')
C
      return
      end
