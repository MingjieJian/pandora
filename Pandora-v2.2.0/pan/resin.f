      subroutine RESIN
     $(CEQHH,HND,HNK,HNI,LIMP,CHN,OHN,RSH,RNH1,H2N,RABD,KRAB)
C
C     Rudolf Loeser, 1982 May 28
C---- Computes H2 Molecule abundance and, KRAB=1, abundance correction
C     for Atomic Hydrogen.
C     (This is version 2 of RESIN.)
C     !DASH
      save
C     !DASH
      real*8 CEQHH, CHN, CORM, H2N, HN1, HND, HNI, HNK, OHN, ONE, RABD,
     $       RNH1, RSH
      integer I, KRAB, LIMP, N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external DIVIDE, ROWSUM, BOUNDUP, OVANDO, HI, BYE
C
C               CEQHH(N), HNK(N), HND(N), HNI(N,LIMP), H2N(N), RABD(N),
      dimension CEQHH(*), HNK(*), HND(*), HNI(*),      H2N(*), RABD(*),
C
C               RSH(N), RNH1(N), CHN(N), OHN(N)
     $          RSH(*), RNH1(*), CHN(*), OHN(*)
C
      call HI ('RESIN')
C     !BEG
C---- Compute RSH
      call ROWSUM     (HNI, N, N, 2, LIMP, RSH)
      do 100 I = 1,N
        call DIVIDE   ((RSH(I)+HNK(I)), HND(I), RSH(I))
  100 continue
      call BOUNDUP    (N, RSH, ONE)
C---- Compute RNH1
      call OVANDO     (CEQHH, HND, RSH, RNH1, N)
C---- Compute Molecular Hydrogen number density
      do 101 I = 1,N
        HN1 = RNH1(I)*HND(I)
        H2N(I) = CEQHH(I)*(HN1**2)
  101 continue
      if(KRAB.eq.1) then
C----   Compute abundance correction factor
        do 102 I = 1,N
          call DIVIDE ((CHN(I)+OHN(I)), HND(I), CORM)
          RABD(I) = RNH1(I)+RSH(I)-CORM
  102   continue
      end if
C     !END
      call BYE ('RESIN')
C
      return
      end
