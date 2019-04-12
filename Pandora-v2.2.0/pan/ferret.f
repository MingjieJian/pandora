      subroutine FERRET
     $(WS,IS,IL,TMU,N)
C
C     Rudolf Loeser, 1980 Dec 31
C---- Modifies intensity integration weights for the case REFLECT=ON,
C     for WEASEL.
C     !DASH
      save
C     !DASH
      real*8 EC, EP, SHIFT, TMU, TWO, WS, XM, XQ, dummy
      integer I, IL, IS, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external QEXP2, QEXP3, HI, BYE
C
C               WS(N), TMU(N)
      dimension WS(*), TMU(*)
C
      call HI ('FERRET')
C     !BEG
      SHIFT = TWO*TMU(N)
C
      call QEXP3   (TMU(IS),dummy,2,XQ)
      EC    = exp(TMU(IS)-SHIFT)
      WS(1) = WS(1)+EC*XQ
C
      do 100 I = IS,(IL-1)
        call QEXP3 ((TMU(I+1)-TMU(I)),dummy,2,XQ)
        call QEXP2 ((TMU(I)-TMU(I-1)),dummy,2,XM)
        EP = EC
        EC = exp(TMU(I)-SHIFT)
        WS(I) = WS(I)+EC*XM+EP*XQ
  100 continue
C
      call QEXP2   ((TMU(IL)-TMU(IL-1)),dummy,2,XM)
      EC     = exp(TMU(IL)-SHIFT)
      WS(IL) = WS(IL)+EC*XM
C     !END
      call BYE ('FERRET')
C
      return
      end
