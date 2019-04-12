      subroutine SOFFIT
     $(NFB,NVI,WFB,CVXM,CVX,WT,WTP)
C
C     Rudolf Loeser, 2005 Jun 30
C---- Sets up flow-broadening parameter tables.
C     !DASH
      save
C     !DASH
      real*8 ANG, CVX, CVXM, DEL, DIV, DPR, HALF, NINETY, ONE, WEND,
     $       WFB, WT, WTP, WTPZ
      integer I, J, NFB, NVI
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
      equivalence (REST( 1),WTPZ )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external HI, BYE
C
C               CVX(NVX), WT(NVX), WTP(NVX)
      dimension CVX(*),   WT(*),   WTP(*)
C
      data DPR,NINETY /57.2957795D0, 9.D1/
C
      call HI ('SOFFIT')
C     !BEG
      DIV = NFB
      DEL = NINETY/DIV
      ANG = NINETY
C
      J = NVI+1
      do 100 I = 1,NFB
        J      = J-1
        WT(I)  = sin(ANG/DPR)
        CVX(I) = CVXM*WT(I)
        WT(J)  = -WT(I)
        CVX(J) = -CVX(I)
        ANG    = ANG-DEL
  100 continue
C
      DIV    = NVI
      WTPZ   = (ONE-WFB)/DIV
      WEND   = HALF*WTPZ
      WTP(1) = WEND
C
      do 101 I = 2,(NVI-1)
        WTP(I) = WTPZ
  101 continue
C
      WTP(NVI) = WEND
C     !END
      call BYE ('SOFFIT')
C
      return
      end
