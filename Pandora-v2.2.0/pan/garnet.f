      subroutine GARNET
     $(N,X,R,BB,S,CQ,KNTMX,KNT,CSFCRIT)
C
C     Rudolf Loeser, 1978 Apr 05
C---- Computes S iteratively, for CECILIA.
C     (This is version 4 of GARNET.)
C     !DASH
      save
C     !DASH
      real*8 BB, CQ, CSFCRIT, R, S, SNEW, SUM, X, ZERO, dummy
      integer I, ITER, ITMX, KNT, KNTMX, N, jummy
      logical SAME
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  ZERO1, MOVE1, SUMPROD, CONVERD, HI, BYE
      intrinsic max
C
C               X(N,N), R(N), BB(N), S(N), CQ(N)
      dimension X(N,*), R(*), BB(*), S(*), CQ(*)
C
      call HI ('GARNET')
C     !BEG
      call ZERO1       (S,N)
C
      ITMX = KNTMX+1
      KNT  = -1
C
      do 101 ITER = 1,ITMX
C
        KNT = KNT+1
        call MOVE1     (S,N,CQ)
        do 100 I = 1,N
          call SUMPROD (SUM,X(I,1),N,S,1,N)
          SNEW = BB(I)+R(I)*SUM
          S(I) = max(SNEW,ZERO)
  100   continue
C
        call CONVERD   (S,1,N,CQ,1,N,CSFCRIT,dummy,jummy,SAME)
        if(SAME) then
          goto 102
        end if
  101 continue
C
  102 continue
C     !END
      call BYE ('GARNET')
C
      return
      end
