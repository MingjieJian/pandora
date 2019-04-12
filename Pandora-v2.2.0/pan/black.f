      subroutine BLACK
     $(X,IX,W,IW,JSAV,LEGEND,NO,XLB1,XLB2,XLB3,STRN,SSTRN,BCTRN,
     $ COPTRN,VEL,KVEL,FDDL,TCINT,TCINTA,TCFLX,IMG,FO,LYNC,WLYNC,
     $ XLYNC,LUA,LUG,IJECT)
C
C     Rudolf Loeser, 1977 Jan 27
C---- Controls emergent INTENSITY and FLUX profile calculations for the
C     current transition.
C     (This is version 2 of BLACK.)
C     !DASH
      save
C     !DASH
      real*8 BCTRN, COPTRN, FDDL, FO, SSTRN, STRN, TCFLX, TCINT, TCINTA,
     $       VEL, W, WLYNC, X, XLB1, XLB2, XLB3, XLYNC
      integer IJECT, IL, IMG, IPRO, ISB1, IU, IW, IX, JJFIW, JJFRR,
     $        JJMU, JJMUF, JJTE, JJV, JJVR, JJVSB, JJVXS, JJWTP, JJXNE,
     $        JJZ, JSAV, KTRN, KVEL, L, LDL, LINT, LUA, LUG, LYNC,
     $        MMAAB, MMB, MMBC, MMBTA, MMBTF, MMBTI, MMCAA, MMCAB,
     $        MMCDL, MMCFA, MMCFZ, MMCIA, MMCIZ, MMCND, MMCOP, MMCZA,
     $        MMCZB, MMDDL, MMDL, MMDP, MMDW, MMFAB, MMFZB, MMGTN,
     $        MMGTS, MMKCN, MMLAM, MMPGD, MMPGL, MMS, MMSB1, MMSCN,
     $        MMSLF, MMSLR, MMSNU, MMSS, MMSTE, MMSTI, MMTAB, MMTAU,
     $        MMTFB, MMTIB, MMZAB, MPROM, N, NO, NVX
      logical ECLI, FULL, LEGEND, PROF
      character QNAME*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(42),NVX)
      equivalence (JZQ( 7),L  )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ( 12),JJV  )
      equivalence (IZOQ(165),JJVR )
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ(129),JJVXS)
      equivalence (IZOQ(135),JJFRR)
      equivalence (IZOQ(  4),JJMU )
      equivalence (IZOQ(115),JJMUF)
      equivalence (IZOQ(172),JJVSB)
      equivalence (IZOQ(229),JJFIW)
      equivalence (IZOQ(120),JJWTP)
C     !EJECT
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MMP( 2),MMSNU)
      equivalence (MMP( 3),MMSCN)
      equivalence (MMP( 4),MMKCN)
      equivalence (MML( 2),MMLAM)
      equivalence (MML(22),MMB  )
      equivalence (MML(24),MMBC )
      equivalence (MML(12),MMDP )
      equivalence (MML(13),MMDW )
      equivalence (MML(15),MMGTN)
      equivalence (MML(35),MMGTS)
      equivalence (MML(33),MMSS )
      equivalence (MMP( 6),MMSLF)
      equivalence (MML(59),MMDL )
      equivalence (MML(16),MMTAU)
      equivalence (MML(25),MMCND)
      equivalence (MML(34),MMDDL)
      equivalence (MML(31),MMCDL)
      equivalence (MMT( 2),MMCIZ)
      equivalence (MMT( 3),MMCIA)
      equivalence (MMT( 6),MMBTI)
      equivalence (MMT( 8),MMCZA)
      equivalence (MMT( 9),MMCAA)
      equivalence (MMT(10),MMBTA)
      equivalence (MMT( 4),MMCFZ)
      equivalence (MMT( 5),MMCFA)
      equivalence (MMT( 7),MMBTF)
      equivalence (MMT(11),MMCZB)
      equivalence (MMT(12),MMCAB)
      equivalence (MMT(13),MMFZB)
      equivalence (MMT(14),MMFAB)
      equivalence (MMT(15),MMTIB)
      equivalence (MMT(16),MMTFB)
      equivalence (MMT(17),MMZAB)
      equivalence (MMT(18),MMAAB)
      equivalence (MMT(19),MMTAB)
      equivalence (MML(49),MMSB1)
      equivalence (MML(53),MMPGL)
      equivalence (MML(54),MMPGD)
      equivalence (MML(61),MMSTE)
      equivalence (MML(60),MMSTI)
      equivalence (MML(17),MMCOP)
      equivalence (MML(26),MMS  )
      equivalence (MMP( 7),MMSLR)
C     !EJECT
C---- LINUS       as of 2004 May 12
      integer     LINKDS
      dimension   LINKDS(22)
      common      /LINUS/ LINKDS
C     Line source function calculation control parameters for the
C     current transition as set up by "PET" (and printed by "LINSEED").
C     IU    - index of upper level
C     IL    - index of lower level
C     KLIN  - line "type" code (1: radiative, 2: passive, etc)
C     ICE   - PRD calculation control
C     IPRO  - emergent profiles calculation control
C     METSE - statistical equilibrium calculation method selector
C     METSF - LSF calculation method selector (QR, RT, GR)
C     IBRSW - damping components selector
C     INKSW - input opacity signal
C     LSFT  - LSF solution code (0: full, 1:direct, etc)
C     ILFLX - line flux calculation control
C     LDL   - number of line components
C     LINT  - frequency integration range (half vs. full profile)
C     LSFP  - LSF printout control
C     IFDB  - LSF background control (constant vs. varying)
C     ISBG  - blended line profile plot mode switch
C     KBT   - length of input table XIBLUT
C     KRT   - length of input table XIREDT
C     KST   - length of input table XISYMT
C     KTRN  - length of actual tables XI and DL
C     LOML  - "line-background-continuum-opacity" control
C     ....  - (available)
      equivalence (LINKDS( 1),IU   )
      equivalence (LINKDS( 2),IL   )
      equivalence (LINKDS( 5),IPRO )
      equivalence (LINKDS(12),LDL  )
      equivalence (LINKDS(13),LINT )
      equivalence (LINKDS(20),KTRN )
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
      equivalence (QZQ(  1),QNAME)
C     !EJECT
C---- LOOPER      as of 2006 May 05
      integer     NVEL,NVY,JVEL,LFBV,LFB,MF,MUK
      real*8      EMOO,WVLTRN
      logical     EXPAND,SPHERE,VXZERO,FLOBRD
      common      /LOOPER1/ NVEL,NVY,JVEL,LFBV,LFB,MF,MUK
      common      /LOOPER2/ EMOO,WVLTRN
      common      /LOOPER3/ EXPAND,SPHERE,VXZERO,FLOBRD
C
C     Emergent Profiles calculations control data (simplified version).
C
C     NVEL   : number of velocity tables
C     NVY    : current value of velocity-loop index, 1.le.NVY.le.NVEL
C              (i.e. index of current velocity set)
C     JVEL   : code describing current velocity set (i.e. KVEL(NVY) )
C              =     1 : VSB
C              =     2 : VXS
C              =     3 : VADD     (from AMDIFF and/or VELGRAD)
C              = 100+j : VXN(j)
C              = 200+j : VXN(j) + VADD
C              = 300+j : VAX(j)
C              = 400+j : VFB(j)
C              = 500+j : VFB(j) + VADD
C
C     LFBV   : number of viewing positions (front only, or back also)
C     LFB    : current value of views-loop index, 1 .le. LFB .le. LFBV
C              = 1 - front-face
C              = 2 - back-face
C
C     MF     : current value of look-angles-loop index, 1.le.MF.le.LF
C     MUK    : is .gt. 0 if line intensity profile must be printed
C              [when MUK > 0, then EMOO = EMU(MUK) ]
C     EMOO   : current value of look-angle
C     WVLTRN : wavelength (Angstroms) (i.e. at Delta-Lambda = 0).
C
C     VXZERO : tells whether the current velocity =0, or not
C     EXPAND : tells whether the procedures for expanding atmospheres
C              should be used (set up in SWEET)
C     SPHERE : tells whether this is a spherical, as opposed to
C              plane-parallel, atmosphere
C     FLOBRD : tells whether to use the flow-broadening procedure
C     .
C     !DASH
C     !EJECT
      external  ZAIRA, GRAU, ARLETTE, GRIS, NIMBLE, DANCER, GREY, GOBY,
     $          CRISA, OAK, HI, BYE
      intrinsic mod
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               BCTRN(N,KM), STRN(N,KM), COPTRN(N,KM), TCINTA(KM,LFBV),
      dimension BCTRN(*),    STRN(*),    COPTRN(*),    TCINTA(*),
C
C               SSTRN(N,KM), VEL(N,NVEL), TCFLX(KM,LFBV), XLB1(Li1len),
     $          SSTRN(*),    VEL(*),      TCFLX(*),       XLB1(*),
C
C               FDDL(N), KVEL(NVEL), XLB2(Li2len), XLB3(Li3len), FO(N),
     $          FDDL(*), KVEL(*),    XLB2(*),      XLB3(*),      FO(*),
C
C               TCINT(KM,L,LFBV), IMG(N), WLYNC(KLYNF), XLYNC(KLYNF)
     $          TCINT(*),         IMG(*), WLYNC(*),     XLYNC(*)
C
      call HI ('BLACK')
C     !BEG
C---- Set MPROM
      call OAK
C---- Set up "Sobolev" solution indicator
      ISB1 = XLB1(MMSB1)
C---- Set up "Line Center" wavelength
      WVLTRN = XLB1(MMLAM)
C---- Adjust Hydrogen Stark broadening switch
      call GOBY    (XLB1, MPROM)
C---- Modify velocity set (for Sobolev, if needed)
      call ARLETTE (N, VEL, KVEL, ISB1, X(JJVSB), X(JJVXS))
C---- Set up line data ARRAYS
      call DANCER  (N, KTRN, XLB1(MMSS), XLB1(MMBC), XLB1(MMCOP),
     $              XLB2(MMSLF), XLB2(MMSNU), XLB2(MMSLR),
     $              XLB2(MMSCN), XLB2(MMKCN), STRN, BCTRN, COPTRN,
     $              SSTRN, IMG, FO)
C---- Set up background data for residual calculation
      call CRISA   (KTRN, L, TCINT, TCINTA, TCFLX, XLB3(MMCIZ),
     $              XLB3(MMCZA), XLB3(MMCFZ), XLB3(MMCZB),
     $              XLB3(MMZAB), XLB3(MMFZB))
C     !EJECT
      PROF = mod(IPRO,2).eq.1
      if(PROF) then
C----   Compute Intensity and Flux profiles
C
C----   Plot S, B and TAU vs. Z
        call ZAIRA  (XLB1(MMS), X(JJZ), XLB1(MMTAU), XLB1(MMB),
     $               IU, IL, LUG, SPHERE, IJECT, W)
C----   Compute depth-dependent DDL multiplier
        call NIMBLE (XLB1(MMSTE), X(JJXNE), FDDL, N)
C----   Compute profiles
        if(SPHERE) then
          call GRAU (NO, KTRN, STRN, SSTRN, BCTRN, COPTRN, TCFLX,
     $               VEL, KVEL, FDDL, LDL, XLB1(MMDDL), XLB1(MMCDL),
     $               XLB1(MMDP), XLB1(MMDW), XLB1(MMGTN), XLB1(MMGTS),
     $               XLB1(MMDL), XLB1(MMPGL), XLB1(MMPGD), ISB1,
     $               X(JJV), X(JJVR), X(JJTE), X(JJZ), X(JJFRR),
     $               X(JJXNE), MPROM, W, IW, LUA, LUG, IJECT)
        else
          call GREY (JSAV, LEGEND, NO, KTRN, QNAME, STRN, SSTRN,
     $               BCTRN, COPTRN, TCINT, TCINTA, TCFLX, VEL, KVEL,
     $               FDDL, LDL, XLB1(MMDDL), XLB1(MMCDL), XLB1(MMDP),
     $               XLB1(MMDW), XLB1(MMGTN), XLB1(MMGTS),
     $               XLB1(MMCND), XLB1(MMDL), XLB1(MMPGL),
     $               XLB1(MMPGD), ISB1, X(JJZ), X(JJTE), X(JJV),
     $               X(JJVR), X(JJMU), X(JJMUF), X(JJFIW), X(JJXNE),
     $               X(JJWTP), MPROM, X, W, IW, LYNC, WLYNC, XLYNC,
     $               LUA, LUG, IJECT)
        end if
      end if
C
      ECLI = (IPRO/2).eq.1
      FULL = LINT.eq.1
      if(ECLI.and.FULL) then
C----   Calculate Intensity and Flux profiles again, using spherical
C       coordinates, and produce detailed "ECLIPSE" output.
        call GRIS   (KTRN, STRN, BCTRN, COPTRN, VEL, KVEL, FDDL,
     $               XLB1(MMGTN), XLB1(MMDP), XLB1(MMDW), XLB1(MMDDL),
     $               XLB1(MMCDL), XLB1(MMDL), ISB1, X(JJV), X(JJVR),
     $               X(JJTE), X(JJZ), X(JJFRR), X(JJXNE), MPROM, W, IW)
      end if
C     !END
      call BYE ('BLACK')
C
      return
      end
