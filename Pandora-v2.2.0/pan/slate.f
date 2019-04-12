      subroutine SLATE
     $(N,K,GMA,FJJ,FJN,FAB,SLF,EP,BS,XQSF,IMG,FO)
C
C     Rudolf Loeser, 1978 Apr 23
C---- Computes modified source function, QSF, a PRD term.
C     (This is version 3 of SLATE.)
C     !DASH
      save
C     !DASH
      real*8 BS, EBI, EP, FAB, FJJ, FJN, FO, GMA, GMAIR, ONE, RAT, SLF,
     $       T1, T2, XQSF
      integer I, IMG, J, K, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, BEZANT, HI, BYE
C
C               FAB(N,K), SLF(N,K), GMA(N), FJN(N,K), EP(N), XQSF(N,K),
      dimension FAB(N,*), SLF(N,*), GMA(*), FJN(N,*), EP(*), XQSF(N,*),
C
C               FJJ(N,K), BS(N), IMG(N), FO(N)
     $          FJJ(N,*), BS(*), IMG(*), FO(*)
C
      call HI ('SLATE')
C     !BEG
      do 101 I = 1,N
        call DIVIDE (ONE, (ONE+EP(I)), RAT)
        EBI   = EP(I)*BS(I)
        GMAIR = GMA(I)*RAT
C
        do 100 J = 1,K
          T1 = SLF(I,J)-GMAIR*FJN(I,J)
          T2 = (FJJ(I,J)+EBI)*RAT
          XQSF(I,J) = (ONE-FAB(I,J))*T1+FAB(I,J)*T2
  100   continue
  101 continue
C---- Edit out negatives, if any
      call BEZANT   (N, K, XQSF, 'QSF', IMG, FO)
C     !END
      call BYE ('SLATE')
C
      return
      end
