      subroutine STUCK
     $(N,MN1,KAMB,DUMP,HND,HEND,XN1O,XNKO,XN1N,ZION,XN1,XNK,RS,RSP,
     $ HE21,XNKU,IMG,FO,ITER)
C
C     Rudolf Loeser, 1998 Feb 03
C---- Gets, and prints (?), final N1 & NK, for PYRAMID.
C     !DASH
      save
C     !DASH
      real*8 FO, HE21, HEND, HND, RS, RSP, XN1, XN1N, XN1O, XNK, XNKO,
     $       XNKU, ZION
      integer IMG, ITER, KAMB, MN1, N
      logical DUMP
C     !DASH
      external TEILO, NEMAHA, TUPELO, TEAK, HI, BYE
C
C               XN1O(N), XNK(N), HND(N), RS(N), XN1(N), IMG(N), RSP(N),
      dimension XN1O(*), XNK(*), HND(*), RS(*), XN1(*), IMG(*), RSP(*),
C
C               XNKO(N), HEND(N), XNKU(N), XN1N(N), ZION(N), HE21(N),
     $          XNKO(*), HEND(*), XNKU(*), XN1N(*), ZION(*), HE21(*),
C
C               FO(N)
     $          FO(*)
C
      call HI ('STUCK')
C     !BEG
C---- Normalize to get final N1 (into XND !) and NK (into XNK !)
      call TEILO  (KAMB, MN1, HND, HEND, XN1O, XNKO, XN1N, ZION, XN1,
     $             XNK, RS, RSP)
C---- Edit out bad values (i.e.  .le. 0)
      call NEMAHA (KAMB, N, XNKO, XNKO, HE21, XNKU)
      call TUPELO (MN1, XN1, XN1N, XN1O, XNK, XNKU, IMG, FO, ITER)
C
      if(DUMP) then
        call TEAK (MN1, ITER, KAMB, XN1O, XNKO, XN1N, XN1, XNK, RS, RSP)
      end if
C     !END
      call BYE ('STUCK')
C
      return
      end
