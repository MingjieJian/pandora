      subroutine CARROT
     $(X,W,IW,KAMB,N1MET,KDIAG,KBNDS,I4DIO,I4DFM,I4DEQ,KINOUT,N,MN1,NL,
     $ DUMP,ITER,HND,PLK,RND,Z,XN1O,XNKO,HK,H1,HEK,HE1,HE2K,HE21,XN1N,
     $ DEE,DELTA,F,G,R,S,VEC,XN,SPKL,DZB,BETA,ZXH,H,HEND,Y,RABD,PALBET,
     $ PBETAL,ADDR,ADDS,KDAMP,YRAT,CHK,KZAUG,KZAS,NZAS,KZANX,KZUNL)
C
C     Rudolf Loeser, 1989 Sep 21
C---- Recomputes N1 & NK for PYRAMID.
C     (This is version 5 of CARROT.)
C     !DASH
      save
C     !DASH
      real*8 ADDR, ADDS, BETA, CHK, DEE, DELTA, DZB, F, G, H, H1, HE1,
     $       HE21, HE2K, HEK, HEND, HK, HND, PALBET, PBETAL, PLK, R,
     $       RABD, RND, S, SPKL, VEC, W, X, XN, XN1N, XN1O, XNKO, Y,
     $       YRAT, Z, ZXH
      integer I4DEQ, I4DFM, I4DIO, ICHKA, IFA, IGA, IHA, IIMG, IN, IPNT,
     $        IRA, IS, ISA, ITER, ITKZA, ITZA, IVEC, IVLSW, IW, IWS,
     $        IYA, IZA, IZXHA, JN, KAMB, KBNDS, KDAMP, KDIAG, KINOUT,
     $        KZANX, KZAS, KZAUG, KZUNL, M, MN1, MOX, MUX, N, N1MET, NL,
     $        NZAS
      logical CHANGE, DUMP
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(202),ITKZA)
C     !DASH
C     !EJECT
      external FETTER, KURBIS, TANAMA, STRABO, TROCAR, KANTOR, SUMMIT,
     $         SWISH, RUMBA, ORLOP, RUEBLI, BURLY, IGIVE, WGIVE,
     $         HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               JTMX = ITN1R*ITKZA+1
C
C               PLK(N,NL), CHK(N), XNKO(N), ZXH(N), XN1O(N), PBETAL(N),
      dimension PLK(N,*),  CHK(*), XNKO(*), ZXH(*), XN1O(*), PBETAL(*),
C
C               HE21(N), HK(N), H1(N), ADDR(N), HE2K(N), ADDS(N), H(N),
     $          HE21(*), HK(*), H1(*), ADDR(*), HE2K(*), ADDS(*), H(*),
C
C               Z(N), F(N), G(N), R(N), S(N), BETA(N), PALBET(N), Y(N),
     $          Z(*), F(*), G(*), R(*), S(*), BETA(*), PALBET(*), Y(*),
C
C               XN(N), HND(N), RND(N,NL), HEK(N), VEC(N), KZAS(N,JTMX),
     $          XN(*), HND(*), RND(N,*),  HEK(*), VEC(*), KZAS(*),
C
C               DZB(N), HE1(N), DELTA(7,N), HEND(N), RABD(N), KZANX(N),
     $          DZB(*), HE1(*), DELTA(7,*), HEND(*), RABD(*), KZANX(*),
C
C               DEE(4,5,N), KZUNL(N,IOMX), XN1N(N), YRAT(N), KZAUG(N),
     $          DEE(4,5,*), KZUNL(*),      XN1N(*), YRAT(*), KZAUG(*),
C
C               SPKL(N)
     $          SPKL(*)
C
      dimension IN(10)
      equivalence
     $(IN( 1),IZA   ),(IN( 2),IFA   ),(IN( 3),IGA   ),(IN( 4),IRA   ),
     $(IN( 5),ISA   ),(IN( 6),IHA   ),(IN( 7),IZXHA ),(IN( 8),IYA   ),
     $(IN( 9),ICHKA ),(IN(10),IVEC  )
C
      dimension JN(2)
      equivalence
     $(JN( 1),IIMG  ),(JN( 2),IPNT  )
C     !EJECT
C
      call HI ('CARROT')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call RUEBLI   (IN, IS,  MOX, 'CARROT')
      call BURLY    (JN, IWS, MUX, 'CARROT')
C
C---- Compute f, g, r and s
      call FETTER   (N, NL, KAMB, Z, HND, HK, RABD, HEND, HE1, BETA,
     $               HE2K, PLK, RND, PALBET, PBETAL, DEE, DELTA, F,
     $               G, R, S, XN, SPKL, ADDR, ADDS, HEK, HE21)
C---- Compute h, and VELSW ( = IVLSW)
      call SWISH    (KAMB, MN1, HND, H1, HEND, HE1, HE21, Z, G, F, ZXH,
     $               H, N1MET, IVLSW, VEC, W, IW, DUMP)
      if(DUMP) then
        call TANAMA (MN1, KAMB, IVLSW, KDIAG, KBNDS, KDAMP, I4DIO,
     $               I4DFM, I4DEQ, KINOUT)
      end if
C
      ITZA = 0
  100 continue
        ITZA = ITZA+1
C----   Set up expanded tables (if needed)
        call KANTOR (KZAUG, KZANX, IVLSW, MN1, Z, F, G, R, S, H, ZXH,
     $               M, W(IZA), W(IFA), W(IGA), W(IRA), W(ISA), W(IHA),
     $               W(IZXHA), W(IVEC))
C----   Set up and solve the systems of equations for Y, and get CHK
        call KURBIS (IVLSW, KDIAG, KBNDS, KINOUT, I4DIO, I4DFM, I4DEQ,
     $               ITER, DUMP, W, IW, IW(IIMG), M, W(IZA), W(IFA),
     $               W(IGA), W(IRA), W(ISA), W(IHA), W(IZXHA), W(IYA),
     $               W(ICHKA), KZANX)
C----   Update augmentation counters
        call SUMMIT (M, W(ISA), W(ICHKA), W(IVEC), IW(IPNT), ITZA, MN1,
     $               N, KZAUG, KZANX, KZAS, NZAS, KZUNL, CHANGE, DUMP)
        if(CHANGE.and.(ITZA.lt.ITKZA)) then
          goto 100
        end if
C
C---- Collapse Y and CHK back to regular Z-scale (if needed)
      call ORLOP    (M, W(IYA), W(ICHKA), MN1, KZANX, Y, CHK)
C---- Damp any oscillations in y
      call TROCAR   (KDAMP, Y, VEC, YRAT, MN1, 'y', W, IW, DUMP)
C---- Get N1-new from y
      call STRABO   (KAMB, MN1, HND, HEND, Y, XN1N)
C
      if(DUMP) then
        call RUMBA  (ITER, MN1, KAMB, XN1O, XNKO, XN1N, F, G, R, S,
     $               DEE, DELTA, XN, ADDR, ADDS)
      end if
C
C     (Give back W & IW allotments)
      call WGIVE    (W,  'CARROT')
      call IGIVE    (IW, 'CARROT')
C     !END
      call BYE ('CARROT')
C
      return
      end
