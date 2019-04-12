      subroutine MACABAR
     $(N,NL,MN1, NO,KAMB,HE2SIM,XND,XNK,Z,HND,HEND,HE1,HEK,ALFA,RABD,
     $ XN1O,XNKO,W,IW)
C
C     Rudolf Loeser, 1998 May 22
C---- Normalization of the uppler-level number densities in the
C     "Special N1" calculations of the diffusion calculations.
C     !DASH
      save
C     !DASH
      real*8 ALFA, HE1, HEK, HEND, HND, RABD, W, XN1O, XND, XNK, XNKO,
     $       Z
      integer IALED, IALFO, IALSM, IHE1P, IN, IRN1, IRRNT, IS, IW,
     $        IXNNT, IXNTI, KAMB, MN1, MOX, N, NL, NO
      logical HE2SIM
C     !DASH
      external BARAC, RABAM, WGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               XND(N,NL), XNKO(N), XN1O(N), RABD(N), ALFA(N), HEND(N),
      dimension XND(*),    XNKO(*), XN1O(*), RABD(*), ALFA(*), HEND(*),
C
C               XNK(N), HND(N), Z(N), HE1(N), HEK(N)
     $          XNK(*), HND(*), Z(*), HE1(*), HEK(*)
C
      dimension IN(8)
      equivalence
     $(IN( 1),IRN1  ),(IN( 2),IXNTI ),(IN( 3),IXNNT ),(IN( 4),IRRNT ),
     $(IN( 5),IALFO ),(IN( 6),IALSM ),(IN( 7),IALED ),(IN( 8),IHE1P )
C
      call HI ('MACABAR')
C     !BEG
C     (Get, and allocate, W allotment)
      call BARAC (IN,IS,MOX,'MACABAR')
C
      call RABAM (N,NL,MN1,NO,KAMB,HE2SIM,XND,XNK,Z,HND,HEND,HE1,HEK,
     $            ALFA,RABD,XN1O,W(IRN1),W(IXNTI),W(IXNNT),W(IRRNT),
     $            W(IALFO),W(IALSM),W(IALED),W(IHE1P),W,IW)
C
C     (Give back W allotment)
      call WGIVE (W,'MACABAR')
C     !END
      call BYE ('MACABAR')
C
      return
      end
