      subroutine TOBOL
     $(X,IX,W,IW,JLFLX,XLB1,XLB2,MPROM)
C
C     Rudolf Loeser, 1985 Jan 22
C---- Computes Line Flux Distribution, and Radiative Force,
C     for the current transition.
C     (This is version 2 of TOBOL.)
C     !DASH
      save
C     !DASH
      real*8 W, X, XLB1, XLB2
      integer IABC, IBL, IC, ICE, IFDDL, IH, IH1, IH1M, IL, IN, IPHI,
     $        IQEXA, IRF, IS, ISNU, IU, IW, IX, JJTE, JJVXS, JJXNE, JJZ,
     $        JLFLX, KTRN, LDL, MMA, MMCDL, MMCDW, MMDDL, MMDL, MMDP,
     $        MMDW, MMGTN, MMLAM, MMS, MMSNU, MMSTE, MOX, MPROM, N
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
      equivalence (IZOQ(129),JJVXS)
      equivalence (IZOQ(  9),JJXNE)
C
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
      equivalence (IQQ(169),IQEXA)
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
      equivalence (LINKDS(12),LDL  )
      equivalence (LINKDS(20),KTRN )
C     !EJECT
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML(26),MMS  )
      equivalence (MMP( 2),MMSNU)
      equivalence (MML( 2),MMLAM)
      equivalence (MML( 4),MMCDW)
      equivalence (MML(15),MMGTN)
      equivalence (MML(59),MMDL )
      equivalence (MML(14),MMA  )
      equivalence (MML(12),MMDP )
      equivalence (MML(13),MMDW )
      equivalence (MML(34),MMDDL)
      equivalence (MML(31),MMCDL)
      equivalence (MML(61),MMSTE)
C     !DASH
      external KORYAK, ALDAN, CHUNA, ISHIM, TUNGUS, TAIGA, LENA, WGIVE,
     $         ANABAR, OLENEK, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XLB1(Li1len), XLB2(Li2len)
      dimension XLB1(*),      XLB2(*)
C
      dimension IN(10)
      equivalence
     $(IN( 1),ISNU  ),(IN( 2),IH    ),(IN( 3),IBL   ),(IN( 4),IPHI  ),
     $(IN( 5),IRF   ),(IN( 6),IH1   ),(IN( 7),IH1M  ),(IN( 8),IC    ),
     $(IN( 9),IABC  ),(IN(10),IFDDL )
C     !EJECT
C
      call HI ('TOBOL')
C     !BEG
      if(JLFLX.gt.0) then
C
C       (Get, and allocate, W allotment)
        call KORYAK  (IN, IS, MOX, 'TOBOL')
C
C----   Get SNU
        call OLENEK  (N, KTRN, ICE, XLB1(MMS), XLB2(MMSNU), W(ISNU))
C----   Compute H, and get PHI
        if(IQEXA.le.0) then
C         Stationary case - use DIANA data
          call CHUNA (N, KTRN, W(IBL), W(IC), W(ISNU), W(IH), W(IPHI))
        else
C         Expaanding case - use ORION data
          call ISHIM (N, KTRN, W(ISNU), W(IBL), W(IH), W(IPHI),
     $                XLB1(MMDP), XLB1(MMDW), X(JJXNE), XLB1(MMSTE),
     $                MPROM, XLB1(MMDDL), W(IFDDL), XLB1(MMCDL), LDL,
     $                XLB1(MMDL), X(JJVXS), W, IW)
        end if
C----   Compute Line Absorption Coefficient
        call ANABAR  (N, KTRN, W(IPHI), XLB1(MMGTN), W(IABC))
C----   Compute Radiative Force
        call TUNGUS  (N, KTRN, XLB1(MMLAM), XLB1(MMCDW), XLB1(MMA),
     $                W(IABC), W(IH),W(IRF))
C----   Pull out surface H
        call LENA    (W(IH), N, KTRN, W(IH1), W(IH1M))
C----   Print and plot
        call ALDAN   ('TOBOL', ICE, N, KTRN, IU, IL, W(IH), W(IRF),
     $                W(IH1), W(IH1M), W(IABC), W(ISNU), XLB1(MMDL),
     $                X(JJZ), X(JJTE))
C----   Checksums
        call TAIGA   (N, KTRN, W(IH), W(IRF))
C
C       (Give back W allotment)
        call WGIVE   (W, 'TOBOL')
C
      end if
C     !END
      call BYE ('TOBOL')
C
      return
      end
