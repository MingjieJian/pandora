      subroutine SIZZLE
     $(LURR,LUMR,LUPR,X,XLB1,XPBL,Z,HND,BDHM,XNE,RKI,RKW,RLI,EP1,EP2,
     $ XNK,XND,BDI,TDST,HE304,CQOUT,GMASS,TE,PGS,PTO,VT,FION,FLVS,XJBN,
     $ XK,H2N,CON,VAMB,VBMB,VCMB,VDMB,VH,VP,VE,V1,V2,V3,VM,GD,T5,XRK,
     $ XRL,RHEAB,TR,AIJ,CP,CII,CEIJ,WRAT,RRCP,MRJ,XNU,P,ZME,PALBET,
     $ PBETAL,V,PBETGM,PGMBET,XNC,GVL,VEC,DIONL,DLVSL,VR,TCO,FCE,PCE)
C     Rudolf Loeser, 1980 Jan 04
C---- Writes restart data.
C     (This is version 2 of SIZZLE.)
C     !DASH
      save
C     !DASH
      real*8 AIJ, BDHM, BDI, CEIJ, CII, CON, CP, CQOUT, DIONL, DLVSL,
     $       EP1, EP2, FCE, FION, FLVS, GD, GMASS, GVL, H2N, HE304, HND,
     $       P, PALBET, PBETAL, PBETGM, PCE, PGMBET, PGS, PTO, RHEAB,
     $       RKI, RKW, RLI, RRCP, T5, TCO, TDST, TE, TR, V, V1, V2, V3,
     $       VAMB, VBMB, VCMB, VDMB, VE, VEC, VH, VM, VP, VR, VT, WRAT,
     $       X, XJBN, XK, XLB1, XNC, XND, XNE, XNK, XNU, XPBL, XRK, XRL,
     $       Z, ZME
      integer LUMR, LUPR, LURR, MRJ, N, NOION
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 94),NOION)
C     !EJECT
      external GUSHER, WARLOCK, BUNT, WIZARD, REYNARD, ANATINI, HI, BYE
C
      dimension X(*)
C
C               XLB1(Li1len), XPBL(Lenpop), CQOUT(N), FION(N), BDHM(N),
      dimension XLB1(*),      XPBL(*),      CQOUT(*), FION(*), BDHM(*),
C
C               RKI(N,NL), RKW(N), RLI(N,NL), EP1(N), EP2(N), HE304(N),
     $          RKI(*),    RKW(*), RLI(*),    EP1(*), EP2(*), HE304(*),
C
C               XND(N,NL), BDI(N,NL), TDST(N), XNK(N), HND(N), VAMB(N),
     $          XND(*),    BDI(*),    TDST(*), XNK(*), HND(*), VAMB(*),
C
C               GMASS(N), TE(N), PGS(N), PTO(N), VT(N), CON(N), ZME(N),
     $          GMASS(*), TE(*), PGS(*), PTO(*), VT(*), CON(*), ZME(*),
C
C               FLVS(N), XJBN(N,KK), XK(KK), PBETAL(N), CII(NTE,NSL+1),
     $          FLVS(*), XJBN(*),    XK(*),  PBETAL(*), CII(*),
C
C               VBMB(N), VCMB(N), VDMB(N), VH(N), VP(N), VE(N), XNE(N),
     $          VBMB(*), VCMB(*), VDMB(*), VH(*), VP(*), VE(*), XNE(*),
C
C               V1(N), V2(N), Z(N), MRJ(NSL+1), RHEAB(N), XRL(N,NPQNX),
     $          V1(*), V2(*), Z(*), MRJ(*),     RHEAB(*), XRL(*),
C
C               XRK(N,NPQNX), H2N(N), CEIJ(NTE,MUL), RRCP(MRS), P(NSL),
     $          XRK(*),       H2N(*), CEIJ(*),       RRCP(*),   P(*),
C
C               TR(N,NSL), VEC(N), AIJ(NL,NL), CP(NSL+1), GD(N), VR(N),
     $          TR(*),     VEC(*), AIJ(*),     CP(*),     GD(*), VR(*),
C
C               PGMBET(N), VM(N), V(N), DIONL(N), PALBET(N), PBETGM(N),
     $          PGMBET(*), VM(*), V(*), DIONL(*), PALBET(*), PBETGM(*),
C
C               XNU(NSL), GVL(N,NL), DLVSL(N), T5(N), V3(N), WRAT(NSL),
     $          XNU(*),   GVL(*),    DLVSL(*), T5(*), V3(*), WRAT(*),
C
C               XNC(N), FCE(N,NT), PCE(NT), TCO(N)
     $          XNC(*), FCE(*),    PCE(*),  TCO(*)
C     !EJECT
C
      call HI ('SIZZLE')
C     !BEG
      write (LUMR,100) N
  100 format('N (',I4,' ) > ')
      call BUNT      (LUMR, Z,  'Z' )
      call BUNT      (LUMR, TE, 'TE')
C
C---- Atmosphere data
      call GUSHER    (X, N, GD, VEC)
      call WARLOCK   (LUPR, LUMR, HND, BDHM, XNE, ZME, XNC, GMASS, TE,
     $                PGS, PTO, VT, VM, GD, T5, VBMB, XPBL)
C
      if(NOION.le.0) then
C----   Iterations data
        call WIZARD  (LURR, XLB1, RKI, RKW, RLI, EP1, EP2, XNK, XND,
     $                BDI, FCE, PCE)
C
C----   Atomic data
        call ANATINI (LUMR, XLB1, AIJ, CP,CII, CEIJ, WRAT, RRCP, MRJ,
     $                XNU, P)
      end if
C
C---- Miscellaneous
      call REYNARD   (LUMR, TDST, Z, HE304, CQOUT, FION, FLVS, XJBN,
     $                XK, H2N, CON, VAMB, VBMB, VCMB, VDMB, VH, VP, VE,
     $                V1, V2, V3, VM, TE, XRK, XRL, RHEAB, TR, PALBET,
     $                PBETAL, XNE, HND, V, VT, BDHM, PBETGM, PGMBET,
     $                GVL, DIONL, DLVSL, VR, PGS, GMASS, BDI, TCO)
C----
      write (LURR,101)
  101 format('USE ( INPUT ) > ')
C     !END
      call BYE ('SIZZLE')
C
      return
      end
