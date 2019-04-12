      subroutine ARMABAC
     $(X,W,IW,KION,N,MN1,NL,DUMP,ITER,KINOUT,KVLG,Z,TE,ZT,XNE,HND,
     $ RHAB,H2N,HEND,RHEAB,RABD,VM,HK,H1,HEK,HE1,HE2K,HE21,ALFA,BETA,
     $ ZI,ZION,XION,XN1,XNK,XN1O,XNKO,RND,PLK,SPKL,PALBET,PBETAL,
     $ PBETGM,PGMBET,VAMB,VBMB,VCMB,VDMB,G1,DIDG1,BETAR,DZB,IMG,SHE,
     $ SHE2,PNF,KZAUG,KZAS,NZAS,KZUNL)
C
C     Rudolf Loeser, 2001 May 17
C---- Computes N1, NK, and velocities, for CARAMBA.
C     !DASH
      save
C     !DASH
      real*8 ALFA, BETA, BETAR, DZB, G1, H1, H2N, HE1, HE21, HE2K, HEK,
     $       HEND, HK, HND, PALBET, PBETAL, PBETGM, PGMBET, PLK, PNF,
     $       RABD, RHAB, RHEAB, RND, SHE, SHE2, SPKL, TE, VAMB, VBMB,
     $       VCMB, VDMB, VM, W, X, XION, XN1, XN1O, XNE, XNK, XNKO, Z,
     $       ZI, ZION, ZT
      integer I4DEQ, I4DFM, I4DIO, IBETAP, IDEE, IDELTA, IELA, IF1AC,
     $        IF1C, IF2C, IMG, IN, IS, ITER, IVEMB, IW, KBNDS, KDAMP,
     $        KDIAG, KINOUT, KION, KVLG, KZAS, KZAUG, KZUNL, MN1, MOX,
     $        MSSPR, N, N1MET, NL, NZAS
      logical DIDG1, DUMP, SMGMMA
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
      equivalence (KZQ( 91),N1MET)
      equivalence (KZQ(169),KDAMP)
      equivalence (KZQ( 90),KDIAG)
      equivalence (KZQ(161),I4DIO)
      equivalence (KZQ(159),I4DFM)
      equivalence (KZQ(160),I4DEQ)
      equivalence (KZQ(170),KBNDS)
      equivalence (KZQ(162),MSSPR)
C     !DASH
C     !EJECT
      external MARCA, WGIVE, MEULAN, SODA, OBELISK, PYRAMID, PYROLA,
     $         HI, BYE
C
C
      dimension X(*), W(*), IW(*)
C
C               JTMX = ITN1R*ITKZA+1
C
C               VCMB(N), H2N(N), HEK(N), HE1(N), Z(N), HE21(N), IMG(N),
      dimension VCMB(*), H2N(*), HEK(*), HE1(*), Z(*), HE21(*), IMG(*),
C
C               PLK(N,NL), PALBET(N), BETA(N), VDMB(N), PNF(N), HND(N),
     $          PLK(*),    PALBET(*), BETA(*), VDMB(*), PNF(*), HND(*),
C
C               RHAB(N), XNE(N), XION(N), RHEAB(N), RND(N,NL), HEND(N),
     $          RHAB(*), XNE(*), XION(*), RHEAB(*), RND(*),    HEND(*),
C
C               VAMB(N), VBMB(N), PBETGM(N), HE2K(N), ZION(N), RABD(N),
     $          VAMB(*), VBMB(*), PBETGM(*), HE2K(*), ZION(*), RABD(*),
C
C               SPKL(N), DZB(N), G1(N), ZT(N), H1(N), VM(N), PGMBET(N),
     $          SPKL(*), DZB(*), G1(*), ZT(*), H1(*), VM(*), PGMBET(*),
C
C               XNK(N), XNKO(N), XN1O(N), SHE(N), HK(N), XN1(N), TE(N),
     $          XNK(*), XNKO(*), XN1O(*), SHE(*), HK(*), XN1(*), TE(*),
C
C               PBETAL(N), ZI(N), SHE2(N), ALFA(N), BETAR(N), KZAUG(N),
     $          PBETAL(*), ZI(*), SHE2(*), ALFA(*), BETAR(*), KZAUG(*),
C
C               KZAS(N,JTMX), KZUNL(N,IOMX)
     $          KZAS(*),      KZUNL(*)
C
      dimension IN(8)
      equivalence
     $(IN( 1),IF2C  ),(IN( 2),IELA  ),(IN( 3),IF1AC ),(IN( 4),IF1C  ),
     $(IN( 5),IDEE  ),(IN( 6),IDELTA),(IN( 7),IVEMB ),(IN( 8),IBETAP)
C
      data SMGMMA /.true./
C                  2003 Jun 26
C     !EJECT
C
      call HI ('ARMABAC')
C     !BEG
C     (Get, and allocate, W allotment)
      call MARCA     (IN, IS, MOX, 'ARMABAC')
C
      if(DUMP) then
C----   Printout of starting values
        call MEULAN  (N, ITER, HND, H1, HK, RHAB, HEND, HE1, HEK, BETA,
     $                HE21, HE2K, RHEAB, PNF, SHE, SHE2, BETAR, DZB)
      end if
C---- Compute DEE, DELTA, and velocities
      call SODA      (X, W, IW, W(IDEE), W(IDELTA), VAMB, VBMB, VCMB,
     $                VDMB, W(IVEMB), N, Z, TE, ZT, XNE, HND, H2N,
     $                HEND, RHEAB, VM, HK, H1, HEK, HE1, HE2K, HE21,
     $                BETA, ZI, ZION, XION, KVLG, DUMP)
C
C---- Compute N1, NK, and G1 (print ?)
      if(N1MET.eq.3) then
C----   All ions simultaneously
        call OBELISK (X, W, IW, KION, N, MN1, NL, DUMP, ITER, KINOUT,
     $                Z, XN1, XNK, G1, DIDG1, HEND, ALFA, BETA, HE2K,
     $                HE1, KBNDS, MSSPR, W(IDEE), W(IDELTA), RND,
     $                PLK, SPKL, PALBET, PBETAL, PBETGM, PGMBET,
     $                KDAMP, SMGMMA)
      else
C----   One ion at a time
        call PYRAMID (X, W, IW, KION, N, MN1, NL, DUMP, ITER, N1MET,
     $                KDIAG, KBNDS, I4DIO, I4DFM, I4DEQ, KINOUT, HND,
     $                PLK, RND, Z, XN1O, XNKO, HK, H1, HEK, HE1, HE2K,
     $                HE21, ZION, SPKL, DZB, BETA, HEND, RABD, PALBET,
     $                PBETAL, W(IDEE), W(IDELTA), IMG, XN1, XNK, G1,
     $                DIDG1, KDAMP, KZAUG, KZAS, NZAS, KZUNL)
      end if
C
C---- "Additional" normalization ( ? [HENORM] ) (including all N's)
      call PYROLA    (X, KION, N, NL, XNK, XN1, HND, HEK, HE1, HE2K,
     $                HE21, SHE, SHE2, RHEAB, W(IELA), W(IF1C),
     $                W(IF1AC), W(IF2C), PNF, W(IBETAP), DUMP)
C
C     (Give back W allotment)
      call WGIVE (W, 'ARMABAC')
C     !END
      call BYE ('ARMABAC')
C
      return
      end
