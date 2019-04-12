      subroutine TUPELO
     $(N,XN1,XN1N,XNO,XNK,XNKU,IMG,FO,IN1)
C
C     Rudolf Loeser, 1990 Jan 02
C---- Edits N1 and NK, for CARAMBA.
C     (This is version 3 or TUPELO.)
C     !DASH
      save
C     !DASH
      real*8 FO, XN1, XN1N, XNK, XNKU, XNO, ZERO
      integer I, IMG, IN1, IOVER, KERM, N, NERM
      logical BAD
      character LABEL*70
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
      equivalence (LEST( 2),IOVER)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external KIESEL, EDITH, HI, BYE
C
C               XN1(N), XNK(N), XNO(N), XNKU(N), XN1N(N), IMG(N), FO(N)
      dimension XN1(*), XNK(*), XNO(*), XNKU(*), XN1N(*), IMG(*), FO(*)
C
      data KERM,NERM /0, 1000/
C     !EJECT
C
      call HI ('TUPELO')
C     !BEG
      write (LABEL,100) IN1,IOVER
  100 format(3X,', N1-iteration #',I3,', overall iteration #',I4)
C
C---- Edit out NK .le. 0
      LABEL(1:3) = ' NK'
      call EDITH  (XNK,N,ZERO,2,2,1,LABEL,IMG,FO,KERM,NERM,BAD)
C---- Make edited NK "bad" where N1N is "bad"
      do 101 I = 1,N
        if(XN1N(I).le.ZERO) then
          XNK(I) = ZERO
        end if
  101 continue
C---- Now edit out these "artificially bad" values
      call KIESEL (XNK,N,ZERO,2,XNKU,1,LABEL,IMG,FO,KERM,NERM,BAD)
C
C---- Edit out N1 .le. 0
      LABEL(1:3) = ' N1'
      call EDITH  (XN1,N,ZERO,2,2,1,LABEL,IMG,FO,KERM,NERM,BAD)
C---- Make edited N1 "bad" where N1N is "bad"
      do 102 I = 1,N
        if(XN1N(I).le.ZERO) then
          XN1(I) = ZERO
        end if
  102 continue
C---- Now edit out these "artificially bad" valuse
      call KIESEL (XN1,N,ZERO,2,XNO ,1,LABEL,IMG,FO,KERM,NERM,BAD)
C     !END
      call BYE ('TUPELO')
C
      return
      end
