      subroutine HEXAGON
     $(N,LDL,CRD,XNU,GMMA,DP,GMA)
C
C     Rudolf Loeser, 1978 Apr 22
C---- Computes GMA, a PRD term.
C     (This is version 2 of HEXAGON.)
C     !DASH
      save
C     !DASH
      real*8 CMA, CRD, DP, GMA, GMMA, XNU, ZERO, ZMA
      integer I, KALTG, L, LDL, N, jummy
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(74),KALTG)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external  SET1, MINMAXD, DIVIDE, OCTAGON, HI, BYE
      intrinsic min
C
C               DP(N,LDL), GMA(N), XNU(NSL), CRD(LDL)
      dimension DP(N,*),   GMA(*), XNU(*),   CRD(*)
C
      call HI ('HEXAGON')
C     !BEG
      if(KALTG.gt.0) then
C       (Special case for H Lyman alpha or beta)
        call OCTAGON     (N, LDL, CRD, XNU, DP, GMA)
C
      else
        if(GMMA.ge.ZERO) then
          call SET1      (GMA, N, GMMA)
        else
C
          if(LDL.le.1) then
            L = 1
          else
            call MINMAXD (CRD, 1, LDL, jummy, L)
          end if
C
          CMA = -GMMA
          do 100 I = 1,N
            call DIVIDE  (CRD(L), DP(I,L), ZMA)
            GMA(I) = min(CMA,ZMA)
  100     continue
        end if
      end if
C     !END
      call BYE ('HEXAGON')
C
      return
      end
