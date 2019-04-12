      subroutine LEMON
     $(X,W,IW,ALF,XNU,TE,BATA,TR,BATAR,SET,B,BTR,CNDT,SN)
C
C     Rudolf Loeser, 1980 Apr 22
C---- Sets up some auxiliary functions
C     needed for the Line Source Function calculation.
C     (This is version 4 of LEMON.)
C     !DASH
      save
C     !DASH
      real*8 ALF, B, BATA, BATAR, BTR, CNDT, SET, SN, TE, TR, W, X, XNU
      integer IL, IOVER, IQCSF, IQCSW, ITER, IU, IW, LSFT, N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST( 2),IOVER)
      equivalence (LEST( 3),ITER )
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
      equivalence (IQQ( 46),IQCSF)
      equivalence (IQQ( 14),IQCSW)
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
      equivalence (LINKDS(10),LSFT )
C     !DASH
C     !EJECT
      external PLANK, REMULI, ESTOC, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               XNU(NSL), ALF(MUL), TE(N), TR(N,NL), SET(N,MUL), SN(N),
      dimension XNU(*),   ALF(*),   TE(*), TR(*),    SET(*),     SN(*),
C
C               BATA(N,MUL), BATAR(N,MUL), CNDT(N), B(N), BTR(N)
     $          BATA(*),     BATAR(*),     CNDT(*), B(*), BTR(*)
C
      call HI ('LEMON')
C     !BEG
      if((IOVER*ITER).eq.1) then
C----   Planck functions
        call PLANK   (IU, IL, N, XNU, ALF, TE, BATA, B)
        if((IQCSF.le.0).and.(IQCSW.gt.0)) then
          call PLANK (IU, IL, N, XNU, ALF, TR, BATAR, BTR)
        end if
C----   Incident radiation term
        call REMULI  (N, IU, IL, X, XNU, CNDT)
      end if
C
C---- S from number densities
      call ESTOC     (N, IU, IL, XNU, SET, B, SN, W, IW)
C     !END
      call BYE ('LEMON')
C
      return
      end
