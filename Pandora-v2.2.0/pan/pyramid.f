      subroutine PYRAMID
     $(X,W,IW,KAMB,N,MN1,NL,DUMP,ITER,N1MET,KDIAG,KBNDS,I4DIO,I4DFM,
     $ I4DEQ,KINOUT,HND,PLK,RND,Z,XN1O,XNKO,HK,H1,HEK,HE1,HE2K,HE21,
     $ ZION,SPKL,DZB,BETA,HEND,RABD,PALBET,PBETAL,DEE,DELTA,IMG,XN1,
     $ XNK,G1,DIDG1,KDAMP,KZAUG,KZAS,NZAS,KZUNL)
C
C     Rudolf Loeser, 1998 Jun 29
C---- Computes populations, one ion at a time, for CARAMBA.
C     !DASH
      save
C     !DASH
      real*8 BETA, DEE, DELTA, DZB, G1, H1, HE1, HE21, HE2K, HEK, HEND,
     $       HK, HND, PALBET, PBETAL, PLK, RABD, RND, SPKL, W, X, XN1,
     $       XN1O, XNK, XNKO, Z, ZION
      integer I4DEQ, I4DFM, I4DIO, IADDR, IADDS, ICHK, IFF, IFO, IGG,
     $        IH, IKZANX, IMG, IN, IN1N, INKU, IRR, IRS, IRSP, IS, ISS,
     $        ITER, IVEC, IW, IWS, IXN, IY, IYRAT, IZXH, JN, KAMB,
     $        KBNDS, KDAMP, KDIAG, KINOUT, KZAS, KZAUG, KZUNL, MN1, MOX,
     $        MUX, N, N1MET, NL, NZAS
      logical DIDG1, DUMP
C     !DASH
      external ASHLAR, RALASH, CARROT, STUCK, GONE, WGIVE, IGIVE,
     $         HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               JTMX = ITN1R*ITKZA+1
C
C               SPKL(N), XN1O(N), HE2K(N), ZION(N), PBETAL(N), XNKO(N),
      dimension SPKL(*), XN1O(*), HE2K(*), ZION(*), PBETAL(*), XNKO(*),
C
C               HK(N), HE1(N), Z(N), PALBET(N), IMG(N), DZB(N), XNK(N),
     $          HK(*), HE1(*), Z(*), PALBET(*), IMG(*), DZB(*), XNK(*),
C
C               RND(N,NL), HEND(N), PLK(N,NL), RABD(N), G1(N), HE21(N),
     $          RND(*),    HEND(*), PLK(*),    RABD(*), G1(*), HE21(*),
C
C               DELTA(7,N), BETA(N), HND(N), H1(N), HEK(N), DEE(4,5,N),
     $          DELTA(*),   BETA(*), HND(*), H1(*), HEK(*), DEE(*),
C
C               XN1(N), KZAS(N,JTMX), KZAUG(N), KZUNL(N,IOMX)
     $          XN1(*), KZAS(*),      KZAUG(*), KZUNL(*)
C
      dimension IN(18)
      equivalence
     $(IN( 1),IN1N  ),(IN( 2),IVEC  ),(IN( 3),IFF   ),(IN( 4),IGG   ),
     $(IN( 5),IRR   ),(IN( 6),ISS   ),(IN( 7),ICHK  ),(IN( 8),IFO   ),
     $(IN( 9),IYRAT ),(IN(10),IXN   ),(IN(11),IZXH  ),(IN(12),IRS   ),
     $(IN(13),IRSP  ),(IN(14),IH    ),(IN(15),IADDS ),(IN(16),IY    ),
     $(IN(17),IADDR ),(IN(18),INKU  )
C
      dimension JN(1)
      equivalence
     $(JN( 1),IKZANX)
C     !EJECT
C
      call HI ('PYRAMID')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call ASHLAR (IN, IS,  MOX, 'PYRAMID')
      call RALASH (JN, IWS, MUX, 'PYRAMID')
C
C---- Recompute N1 into N1N (dump ?)
      call CARROT (X, W, IW, KAMB, N1MET, KDIAG, KBNDS, I4DIO, I4DFM,
     $             I4DEQ, KINOUT, N, MN1, NL, DUMP, ITER, HND, PLK,
     $             RND, Z, XN1O, XNKO, HK, H1, HEK, HE1, HE2K, HE21,
     $             W(IN1N), DEE, DELTA, W(IFF), W(IGG), W(IRR), W(ISS),
     $             W(IVEC), W(IXN), SPKL, DZB, BETA, W(IZXH), W(IH),
     $             HEND, W(IY), RABD, PALBET, PBETAL, W(IADDR),
     $             W(IADDS), KDAMP, W(IYRAT), W(ICHK), KZAUG, KZAS,
     $             NZAS, IW(IKZANX), KZUNL)
C---- Compute G1
      call GONE   (KAMB, HND, HEND, W(IN1N), W(ISS), W(IRR), W(IYRAT),
     $             N, MN1, G1, DIDG1)
C---- Normalize, edit (and dump ?) final values of N1 and NK
      call STUCK  (N, MN1, KAMB, DUMP, HND, HEND, XN1O, XNKO, W(IN1N),
     $             ZION, XN1, XNK, W(IRS), W(IRSP), HE21, W(INKU), IMG,
     $             W(IFO), ITER)
C
C     (Give back W & IW allotments)
      call WGIVE  (W,  'PYRAMID')
      call IGIVE  (IW, 'PYRAMID')
C     !END
      call BYE ('PYRAMID')
C
      return
      end
