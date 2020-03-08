      subroutine COOK
     $(X,IX,W,IW,XLB1,XCBL,HN1,POPK,KHED)
C
C     Rudolf Loeser, 1980 Aug 20
C---- Controls Passive Lines processing.
C     (This is version 3 of COOK.)
C     !DASH
      save
C     !DASH
      real*8 HN1, POPK, W, X, XCBL, XLB1
      integer IFBRSW, IFDDL, IGTO, IN, IPHI, IS, ITAUM, IW, IX, IXKL,
     $        IXKT, JJALF, JJBAT, JJBDI, JJBTR, JJSET, JJTE, JJTR,
     $        JJVXS, JJXND, JJXNE, JJXNU, JJZ, JPROM, KHED, LUA, LUD,
     $        LUP, LUT, MMB, MMBC, MMBTR, MMCDL, MMCOP, MMCSF, MMDDL,
     $        MMDP, MMDW, MMFXI, MMGTN, MMJBC, MMLAM, MMS, MMSTE, MMTAU,
     $        MOX, N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(  8),JJTR )
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ( 59),JJXND)
      equivalence (IZOQ( 44),JJBDI)
      equivalence (IZOQ(129),JJVXS)
      equivalence (IZOQ(253),JJSET)
      equivalence (IZOQ( 25),JJALF)
      equivalence (IZOQ( 39),JJBAT)
      equivalence (IZOQ( 43),JJBTR)
C     !EJECT
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML( 2),MMLAM)
      equivalence (MML(18),MMCSF)
      equivalence (MML(36),MMJBC)
      equivalence (MML(17),MMCOP)
      equivalence (MML(15),MMGTN)
      equivalence (MML(12),MMDP )
      equivalence (MML(13),MMDW )
      equivalence (MML(16),MMTAU)
      equivalence (MML(26),MMS  )
      equivalence (MML(22),MMB  )
      equivalence (MML(47),MMBTR)
      equivalence (MML(24),MMBC )
      equivalence (MML(34),MMDDL)
      equivalence (MML(31),MMCDL)
      equivalence (MML(48),MMFXI)
      equivalence (MML(61),MMSTE)
C     !DASH
      external WGIVE, POTATO, ABJECT, PLOUGH, CHIMNEY, NIMBLE, VULTURE,
     $         KOOK, NOOK, PANN, NUT, GOBY, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XLB1(Lizlen), XCBL(Miklen), POPK(N,NPOPS), HN1(N)
      dimension XLB1(*),      XCBL(*),      POPK(*),       HN1(*)
C
      dimension IN(7)
      equivalence
     $(IN( 1),IPHI  ),(IN( 2),IS    ),(IN( 3),IXKL  ),(IN( 4),IXKT  ),
     $(IN( 5),IFDDL ),(IN( 6),ITAUM ),(IN( 7),IGTO  )
C
      data IFBRSW /0/
C     !EJECT
C
      call HI ('COOK')
C     !BEG
C     (Get, and allocate, W allotment)
      call KOOK     (IN, IS, MOX, 'COOK')
C
C---- Set up output
      call NOOK     (KHED, LUD, LUA, LUT, LUP)
C---- Get Continuum Data
      call PLOUGH   (XCBL, XLB1(MMLAM), XLB1(MMCSF), XLB1(MMJBC),
     $               XLB1(MMCOP))
C---- Adjust Hydrogen Stark broadening switch
      call GOBY     (XLB1, JPROM)
C---- Compute Damping Parameter
      call CHIMNEY  (X(JJZ), X(JJTE), X(JJXNE), JPROM, X(JJXND),
     $               X(JJXNU), HN1, IFBRSW, LUD, XLB1, POPK, W)
      if(LUA.gt.0) then
C----   Print Profile Analysis
        call ABJECT (LUA)
        call NUT    (X, W, IW, XLB1, JPROM, LUA)
      end if
C---- Compute depth-dependent DDL multiplier
      call NIMBLE   (XLB1(MMSTE), X(JJXNE), W(IFDDL), N)
C---- Compute optical depth (also GTN, FR)
      call VULTURE  (X, IX, W, IW, X(JJXND), X(JJBDI), W(IGTO),
     $               XLB1(MMGTN), XLB1(MMFXI), XLB1(MMCOP),
     $               XLB1(MMDP), XLB1(MMDW), JPROM, X(JJXNE),
     $               XLB1(MMDDL), W(IFDDL), XLB1(MMCDL), X(JJVXS),
     $               XLB1(MMLAM), XLB1(MMTAU), W(ITAUM), W(IXKL),
     $               W(IXKT), LUT)
C---- Compute Line Source Function (also PHI, B, BC, and
C                                   Monochromatic Source Function)
      call POTATO   (W(IS), XLB1(MMS), XLB1(MMB), XLB1(MMBTR),
     $               XLB1(MMCSF), XLB1(MMBC), X(JJXNU), X(JJSET),
     $               XLB1(MMGTN), XLB1(MMCOP), XLB1(MMDP),
     $               XLB1(MMDW), JPROM, XLB1(MMDDL), W(IFDDL),
     $               XLB1(MMCDL), W(IPHI), X(JJALF), X(JJTE),
     $               X(JJXNE), X(JJBAT), X(JJTR), X(JJBTR), X(JJVXS),
     $               XLB1(MMLAM), W, IW)
C---- Print
      call PANN     (LUP, XLB1(MMCOP), XLB1(MMGTN), W(IPHI),
     $               XLB1(MMDP), XLB1(MMDW), XLB1(MMB), XLB1(MMBC),
     $               W(IS), XLB1(MMS), XLB1(MMTAU))
C
C     (Give back W allotment)
      call WGIVE    (W, 'COOK')
C     !END
      call BYE ('COOK')
C
      return
      end
