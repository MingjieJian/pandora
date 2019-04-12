      subroutine VOGUL
     $(NO,DAMPY,YCONT,MSFT,ISB1,ISB2)
C
C     Rudolf Loeser, 1981 Sep 25
C---- Prints control parameters, for PSHAW.
C     !DASH
      save
C     !DASH
      real*8 DAMPY, EPTAU, YCONT
      integer ICE, IPEX, IQRED, IQSDI, IQSED, IQVES, ISB1, ISB2, LSFT,
     $        LUEO, METSE, MSFT, MWNSV, NO, jummy
      logical DIDIT
      character RED*3, SDI*3, SED*3, VES*3
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
      equivalence (RZQ(118),EPTAU)
      equivalence (KZQ( 18),IPEX )
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
      equivalence (IQQ(214),IQVES)
      equivalence (IQQ(263),IQSDI)
      equivalence (IQQ(149),IQSED)
      equivalence (IQQ(150),IQRED)
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
      equivalence (LINKDS( 6),METSE)
      equivalence (LINKDS(10),LSFT )
      equivalence (LINKDS( 4),ICE  )
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
      equivalence (LEST(67),MWNSV)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external ONOFF, LINER, ESCEX, SEMEX, WEMEX, MESHED, MASHED,
     $         HI, BYE
C
      call HI ('VOGUL')
C     !BEG
      if(NO.gt.0) then
        call ONOFF  (IQSDI, jummy, SDI)
        call ONOFF  (IQSED, jummy, SED)
        call ONOFF  (IQRED, jummy, RED)
        call ONOFF  (IQVES, jummy, VES)
C
        if((IPEX.lt.0).or.(IPEX.eq.24)) then
          call MESHED ('VOGUL', 2)
          write (LUEO,100) MSFT,ICE,DIDIT
  100     format(' ','MSFT =',I2,', ICE =',I2,', DIDIT =',L3)
          call MASHED ('VOGUL')
        end if
C
        if(MSFT.eq.2) then
C----     Escape probality solution
          call ESCEX  (NO, VES, ISB1, ISB2, MSFT, LSFT, EPTAU)
        else
C
          call LINER  (1, NO)
          write (NO,101) SED,RED,SDI
  101     format(' ','----- Option status: SEDIT = ',A,', RHEDIT = ',
     $               A,', and SDIRECT = ',A,'.')
C
C----     Statistical equilibrium method
          call SEMEX  (NO, METSE)
C
C----     Weight matrix method
          call WEMEX  (NO, DAMPY, YCONT, MWNSV)
        end if
      end if
C     !END
      call BYE ('VOGUL')
C
      return
      end
