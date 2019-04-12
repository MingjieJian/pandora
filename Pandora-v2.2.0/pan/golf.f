      subroutine GOLF
     $(X,XCBL,XNU,YCONT,OML,JOPAC,JOPAT)
C
C     Rudolf Loeser, 1976 Aug 05
C---- Sets up data for line-core background calculations.
C     (This is version 3 of GOLF.)
C     !DASH
      save
C     !DASH
      real*8 OML, WAVES, X, XCBL, XLM, XNU, YCONT
      integer ICE, IFDB, IL, INKSW, IQCSF, ITYPE, IU, IUL, J, JOPAC,
     $        JOPAT, KLIN, KODE, KPRD, NT, NW
      logical DOIT, RADPSS, SKIP, SKIP1, SKIP2
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 5),NT )
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
      equivalence (LINKDS( 3),KLIN )
      equivalence (LINKDS( 9),INKSW)
      equivalence (LINKDS( 4),ICE  )
      equivalence (LINKDS(15),IFDB )
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
      equivalence (IQQ( 46),IQCSF)
C     !DASH
      external PET, ANGIE, DARINE, INTRANS, TRUDE, HI, BYE
C
      dimension X(*)
C
C               JOPAC(Nopac), JOPAT(Nopac), XCBL(Miklen), YCONT(NT),
      dimension JOPAC(*),     JOPAT(*),     XCBL(*),      YCONT(*),
C
C               XNU(NSL), OML(NT)
     $          XNU(*),   OML(*)
C
      dimension WAVES(1)
C
      data NW, KPRD, KODE /1, 0, 1/
C     !EJECT
C
      call HI ('GOLF')
C     !BEG
      do 100 J = 1,NT
        call PET       (J)
C
        RADPSS = (KLIN.eq.1).or.(KLIN.eq.2)
        SKIP1  = ((KLIN.eq.1).and.(INKSW.gt.0).and.(IQCSF.le.0))
        SKIP2  = (ICE.ne.0).or.(IFDB.gt.0)
        SKIP   = SKIP1.or.SKIP2
        DOIT   = RADPSS.and.(.not.SKIP)
C
        if(DOIT) then
          call ANGIE   ((XNU(IU)-XNU(IL)), XLM)
          WAVES(NW) = XLM
          call DARINE  (ITYPE)
          call INTRANS (IU, IL, 'GOLF', IUL)
C
          call TRUDE   (X, XCBL, NW, WAVES, XLM, YCONT(IUL), ITYPE,
     $                  KPRD, OML(IUL), KODE, JOPAC, JOPAT)
        end if
  100 continue
C     !END
      call BYE ('GOLF')
C
      return
      end
