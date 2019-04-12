      subroutine PLOVER
     $(X,W,IW,XLB1,XLB2,KHED)
C
C     Rudolf Loeser, 1992 MAR 20
C---- Controls calculation of SLF, frequency-dependent Line Source Fn.
C     (This is version 2 of PLOVER.)
C     !DASH
      save
C     !DASH
      real*8 W, X, XLB1, XLB2
      integer IFL, IFO, IFSAV, IFSLF, IIMG, IL, IN, IQSLG, IQSLP, IS,
     $        ISL, IU, IW, IWS, IZL, JJBDI, JJTE, JJZ, JN, KHED, KLAB,
     $        KTRN, MMCDW, MMDL, MMLAM, MMSLF, MMXI, MOX, MS, MUX, N,
     $        NDW, NO, NS
      logical EDITED
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
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ( 44),JJBDI)
C
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML( 2),MMLAM)
      equivalence (MML( 4),MMCDW)
      equivalence (MML( 3),MMXI )
      equivalence (MML(59),MMDL )
      equivalence (MMP( 6),MMSLF)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C     !EJECT
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(  2),MS   )
      equivalence (KZQ(  3),NS   )
      equivalence (KZQ(  1),NDW  )
C
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
      equivalence (LINKDS(20),KTRN )
C     !EJECT
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(295),IQSLP)
      equivalence (IQQ(296),IQSLG)
C     !DASH
C     !EJECT
      external ILDIR, SALMON, YANG, YING, DAGMAR, IMMAKE, IGIVE, WGIVE,
     $         BONBON, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               XLB1(Li1len), XLB2(Li2len)
      dimension XLB1(*),      XLB2(*)
C
      dimension IN(6)
      equivalence
     $(IN( 1),ISL   ),(IN( 2),IZL   ),(IN( 3),IFL   ),(IN( 4),IFSLF ),
     $(IN( 5),IFO   ),(IN( 6),IFSAV )
C
      dimension JN(1)
      equivalence
     $(JN( 1),IIMG  )
C
      data KLAB /3/
C
      call HI ('PLOVER')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call ILDIR      (IN, IS , MOX, 'PLOVER')
      call IMMAKE     (JN, IWS, MUX, 'PLOVER')
C
      call BONBON     (NO, IQSLP, KHED)
C
C---- Compute
      call SALMON     (IU, IL, XLB1(MMLAM), XLB1(MMCDW), KTRN,
     $                 XLB1(MMXI), N, X(JJTE), X(JJBDI), W(IFSLF),
     $                 W(IFSAV), IW(IIMG), W(IFO), EDITED, XLB2(MMSLF),
     $                 NO, KHED, KLAB)
C
C---- Output
      if((IU.eq.MS).and.(IL.eq.NS)) then
        if(IQSLP.gt.0) then
C----     Print
          call YANG   (NO, N, KTRN, XLB1(MMXI), XLB1(MMDL),
     $                 XLB1(MMCDW), XLB1(MMLAM), XLB2(MMSLF),
     $                 W(IFSAV), EDITED)
        else if(EDITED) then
          call DAGMAR (IU, IL)
        end if
      else if(EDITED) then
        call DAGMAR   (IU, IL)
      end if
C---- Plot
      call YING       (NO, N, KTRN, NDW, XLB1(MMDL), XLB2(MMSLF),
     $                 X(JJZ), W(ISL), W(IZL), W(IFL), IQSLG)
C
C     (Give back W & IW allotments)
      call WGIVE      (W , 'PLOVER')
      call IGIVE      (IW, 'PLOVER')
C     !END
      call BYE ('PLOVER')
C
      return
      end
