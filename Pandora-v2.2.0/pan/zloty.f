      subroutine ZLOTY
     $(X,W,N,XNE,HND,RAB,SAB,SUM,ZME,ETA,ZRN,U,HNP,ZHEL,ZTRM)
C
C     Rudolf Loeser, 1978 Aug 18
C     RL/SGK revised Apr  9 2014 
C---- Computes new XNE, and intermediates, for SHERBET.
C     (This is version 2 of ZLOTY.)
C     !DASH
      save
C     !DASH
      real*8 ETA, HND, HNP, ONE, RAB, SAB, SUM, TWO, U, W, X, XNE, ZHEL,
     $       ZME, ZRN, ZTRM
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external PEARL, ANGRY, PEANUT, HI, BYE
C
      dimension X(*), W(*)
C
C               ZRN(N), ZME(N), RAB(N), HNP(N), HND(N), SUM(N), SAB(N),
      dimension ZRN(*), ZME(*), RAB(*), HNP(*), HND(*), SUM(*), SAB(*),
C
C               ETA(N,NMT), XNE(N), ZHEL(N), U(N), ZTRM(N,NMT)
     $          ETA(*),     XNE(*), ZHEL(*), U(*), ZTRM(N,*)
C
      call HI ('ZLOTY')
C     !BEG
C---- Compute ZME, metal electrons
      call PEARL  (X,W,XNE,ZME,ETA,ZTRM)
C---- Compute ZRN, ions other than protons
      call ANGRY  (X,ZME,ZHEL,ZRN)
C---- Compute intermediate functions
      do 100 I = 1,N
        U(I)   = ONE-TWO*ZRN(I)*SAB(I)
        HNP(I) = HND(I)*RAB(I)+ZRN(I)-SUM(I)
  100 continue
C
C---- Compute new XNE
      call PEANUT (N,XNE,SAB,U,HNP)
C     !END
      call BYE ('ZLOTY')
C
      return
      end
