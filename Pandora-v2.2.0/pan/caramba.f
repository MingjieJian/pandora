      subroutine CARAMBA
     $(X,W,IW,KAMB,KVLG,ITN1R,LUNP,DMPN,MN1,N,NL,XNK,XND,RND,G1,Z,ZT,
     $ HND,ZI,ZION,XION,TE,XNE,H2N,RHEAB,VM,PLK,SPKL,HK,H1,HEK,HE1,
     $ HE2K,HE21,BETA,IMG,DZB,SN1V,SNKV,VEC,RABD,HEND,SHE,SHE2,VAMB,
     $ VBMB,VCMB,VDMB,PNF,PALBET,PBETAL,PBETGM,PGMBET,RHAB,BETAR,XPBL,
     $ KINOUT,HE2SIM,DIDG1,KZAUG,KZAS,KZUNL)
C
C     Rudolf Loeser, 1988 Jun 16
C---- Computes new N1, NK, and all other populations, and also G1
C     (for possible later use), for diffusion or mass-motion.
C     !DASH
      save
C     !DASH
      real*8 BETA, BETAR, DN1, DZB, G1, H1, H2N, HE1, HE21, HE2K, HEK,
     $       HEND, HK, HND, PALBET, PBETAL, PBETGM, PGMBET, PLK, PNF,
     $       RABD, RHAB, RHEAB, RND, SHE, SHE2, SN1CC, SN1V, SNKV, SPKL,
     $       TE, VAMB, VBMB, VCMB, VDMB, VEC, VM, W, WSN1D, X, XION,
     $       XND, XNE, XNK, XPBL, Z, ZI, ZION, ZT
      integer IALFA, IMG, IN, IN1, IN1O, IN1P, INKO, INKP, IS, ITER,
     $        ITN1R, IW, KAMB, KINOUT, KION, KVLG, KZAS, KZAUG, KZUNL,
     $        LIMIT, LUNP, MN1, MOX, N, N1NUP, NL, NZAS
      logical DIDG1, DMPN, DUMP, HE2SIM, HEAD, MORE, RESET, SAME, lummy
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
      equivalence (KZQ(171),N1NUP)
      equivalence (RZQ(153),SN1CC)
      equivalence (RZQ(155),WSN1D)
C     !DASH
C     !EJECT
      external  BARLEY, CONVERD, MACABAR, WGIVE, POPINAC, CURVY, STOCK,
     $          STICK, MASHED, BAMBA, SALSIFY, STENKA, YURABE, ARMABAC,
     $          CRAMA, DOLO, FEST, MOVE1, KASTEN, STACK, HI, BYE
      intrinsic max
C
      dimension X(*), W(*), IW(*)
C
C               ITMX = ITN1R+1               JTMX = ITN1R*ITKZA+1
C
C               VAMB(N), VBMB(N), XND(N,NL), HE2K(N), ZION(N), RABD(N),
      dimension VAMB(*), VBMB(*), XND(N,*),  HE2K(*), ZION(*), RABD(*),
C
C               VCMB(N), H2N(N), HEK(N), HE1(N), Z(N), HE21(N), IMG(N),
     $          VCMB(*), H2N(*), HEK(*), HE1(*), Z(*), HE21(*), IMG(*),
C
C               PLK(N,NL), PALBET(N), BETA(N), VDMB(N), PNF(N), HND(N),
     $          PLK(*),    PALBET(*), BETA(*), VDMB(*), PNF(*), HND(*),
C
C               RHAB(N), XNE(N), XION(N), RHEAB(N), RND(N,NL), HEND(N),
     $          RHAB(*), XNE(*), XION(*), RHEAB(*), RND(*),    HEND(*),
C
C               SPKL(N), SN1V(N,ITMX), DZB(N), G1(N), ZT(N), PBETGM(N),
     $          SPKL(*), SN1V(*),      DZB(*), G1(*), ZT(*), PBETGM(*),
C
C               XNK(N), HK(N), ZI(N), TE(N), SHE(N), PBETAL(N), VEC(N),
     $          XNK(*), HK(*), ZI(*), TE(*), SHE(*), PBETAL(*), VEC(*),
C
C               XPBL(Lenpbl), H1(N), PGMBET(N), SNKV(N,ITMX), BETAR(N),
     $          XPBL(*),      H1(*), PGMBET(*), SNKV(*),      BETAR(*),
C
C               VM(N), SHE2(N), KZAUG(N), KZAS(N,JTMX), KZUNL(N,20)
     $          VM(*), SHE2(*), KZAUG(*), KZAS(*),      KZUNL(*)
C
      dimension IN(5)
      equivalence
     $(IN( 1),IN1O  ),(IN( 2),INKO  ),(IN( 3),IN1P  ),(IN( 4),INKP  ),
     $(IN( 5),IALFA )
C
      call HI ('CARAMBA')
C     !BEG
C     (Get, and allocate, W allotment)
      call BAMBA  (IN, IS, MOX, 'CARAMBA')
C
C---- Save input values
      call MOVE1  (XND(1,1), N, W(IN1P))
      call MOVE1  (XNK     , N, W(INKP))
C
C---- Initialize iterative analysis of N1,NK and of KZAUG,KZUNL
      call STENKA (XND(1,1), SN1V, XNK, SNKV)
      call KASTEN (N, KZAUG, KZAS, NZAS, KZUNL)
C     !EJECT
      LIMIT = ITN1R
      RESET = .false.
      KION  = max(KAMB,KVLG)
      ITER  = 0
      HEAD  = .false.
  100 continue
        ITER = ITER+1
        call MOVE1   (XND(1,1), N, W(IN1O))
        call MOVE1   (XNK     , N, W(INKO))
        call BARLEY  (LUNP, DMPN, ITER, LIMIT, HEAD, 'CARAMBA', DUMP)
        call CRAMA   (DUMP, ITER, LIMIT, 1)
C----   Compute N1 & NK, and G1
        call ARMABAC (X, W, IW, KION, N, MN1, NL, DUMP, ITER, KINOUT,
     $                KVLG, Z, TE, ZT, XNE, HND, RHAB, H2N, HEND,
     $                RHEAB, RABD, VM, HK, H1, HEK, HE1, HE2K, HE21,
     $                W(IALFA), BETA, ZI, ZION, XION, XND(1,1), XNK,
     $                W(IN1O), W(INKO), RND, PLK, SPKL, PALBET,
     $                PBETAL, PBETGM, PGMBET, VAMB, VBMB, VCMB, VDMB,
     $                G1, DIDG1, BETAR, DZB, IMG, SHE, SHE2, PNF,
     $                KZAUG, KZAS, NZAS, KZUNL)
C----   Weight new N1, NK with previous values
        call DOLO    (WSN1D, VEC, N, W(IN1O), XND(1,1), W(INKO), XNK,
     $                DUMP)
C----   Update population extracts
        call POPINAC (KION, N, XND(1,1), XNK, HND, H1, HK, HEND, HE1,
     $                HEK, BETA, HE21, HE2K, RHEAB, SHE, SHE2, XPBL,
     $                PNF, 0, ITER, DUMP)
        call YURABE  (N, Z, HEND, BETA, BETAR, DZB)
C----   Save for analysis
        call STICK   (3, XND(1,1), SN1V, XNK, SNKV)
C----   Print output markers
        call FEST    (DUMP, ITER)
        call CRAMA   (DUMP, ITER, LIMIT, 2)
C----   Converged ?
        call CONVERD (W(IN1O), 1, MN1, XND(1,1), 1, MN1, SN1CC, DN1,
     $                IN1, SAME)
C----   Do another iteration ?
        call SALSIFY (SAME, RESET, DUMP, ITER, LIMIT, MORE)
      if(MORE) goto 100
      if(HEAD) then
        call MASHED  ('CARAMBA')
      end if
C     !EJECT
C---- Normalization and adjustment of all number densities
      call MACABAR   (N, NL, MN1, LUNP, KION, HE2SIM, XND, XNK, Z,
     $                HND, HEND, HE1, HEK, W(IALFA), RABD, W(IN1P),
     $                W(INKP), W, IW)
C---- Update population extracts
      call POPINAC   (KION, N, XND(1,1), XNK, HND, H1, HK, HEND, HE1,
     $                HEK, BETA, HE21, HE2K, RHEAB, SHE, SHE2, XPBL,
     $                PNF, LUNP, 0, lummy)
C---- Iterative analysis of N1,NK, and of KZAUG,KZUNL
      call STOCK     (LUNP, SN1V, SNKV, VEC)
      call STACK     (LUNP, MN1, N, Z, TE, ZT, KZAS, NZAS, KZUNL)
      if(N1NUP) then
C----   Save for iterative summary
        call CURVY   (N, NL, XND, XNK)
      end if
C
C     (Give back W allotment)
      call WGIVE     (W, 'CARAMBA')
C     !END
      call BYE ('CARAMBA')
C
      return
      end
