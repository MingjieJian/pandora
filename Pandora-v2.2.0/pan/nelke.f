      subroutine NELKE
     $(N,NL,MN1, XN1,XNK,HE1,HEK,ALFOLD,ALFNEW,ALFNSM,ALFNED,Z,HEND,
     $ RABD,ALFINAL,FACTOR,HE1P,NO,W,IW)
C
C     Rudolf Loeser, 1998 Dec 18
C---- New Helium normalization, for simultaneous He-II solution.
C     !DASH
      save
C     !DASH
      real*8 ALFINAL, ALFNED, ALFNEW, ALFNSM, ALFOLD, FACTOR, HE1, HE1P,
     $       HEK, HEND, RABD, SUM, W, XN1, XNK, Z
      integer I, IW, MN1, N, NL, NO
C     !DASH
      external ARRMUL, ARRDIV, BILE, MOVE1, LINER, ANCHOVY, HI, BYE
C
      dimension W(*), IW(*)
C
C               ALFINAL(N), FACTOR(N), HEND(N), XN1(N), HE1(N), XNK(N),
      dimension ALFINAL(*), FACTOR(*), HEND(*), XN1(*), HE1(*), XNK(*),
C
C               ALFNEW(N), RABD(N), ALFOLD(N), ALFNSM(N), ALFNED(N),
     $          ALFNEW(*), RABD(*), ALFOLD(*), ALFNSM(*), ALFNED(*),
C
C               HE1P(N), HEK(N), Z(N)
     $          HE1P(*), HEK(*), Z(*)
C     !EJECT
C
      call HI ('NELKE')
C     !BEG
C---- Compute ALFINAL and new HE1
      call MOVE1   (HE1,N,HE1P)
      call ARRDIV  (HE1,HEND,ALFOLD,N)
      call BILE    (MN1,Z,ALFOLD,ALFNEW,ALFNED,ALFNSM,ALFINAL,W,IW,
     $              'alpha',NO)
      call ARRMUL  (ALFINAL,HEND,HE1P,MN1)
C
C---- Compute normalization factor . . .
      do 100 I = 1,N
        SUM       = HE1P(I)+XN1(I)+XNK(I)
        FACTOR(I) = HEND(I)/SUM
  100 continue
C
C---- . . . and obtain final results
      call ARRMUL  (XN1,FACTOR,XN1,N)
      call ARRMUL  (XNK,FACTOR,XNK,N)
      call ARRMUL  (HE1P,FACTOR,HE1,N)
      call MOVE1   (XN1,N,HEK)
C
C---- Recompute RABD
      do 101 I = 1,N
        RABD(I) = (XN1(I)+XNK(I))/HEND(I)
  101 continue
C
C---- Print ( ?)
      call ANCHOVY (NO,N,MN1,ALFINAL,HE1P,FACTOR,HE1,XN1,HEK,XNK,RABD)
C     !END
      call BYE ('NELKE')
C
      return
      end
