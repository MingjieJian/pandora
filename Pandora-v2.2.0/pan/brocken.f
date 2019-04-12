      subroutine BROCKEN
     $(N,K,CALLER,DUMP,II,JJ)
C
C     Rudolf Loeser, 1988 Jul 20
C---- Initializes PRD frequency integrals debug output.
C     !DASH
      save
C     !DASH
      integer IDRDP, II, IL, IQDDP, IU, JJ, K, KDRDP, LUEO, MO, MS, N,
     $        NS
      logical DUMP
      character CALLER*(*), LAB*4
C     !COM
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
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
      equivalence (LUNITS( 6),LUEO )
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
      equivalence (KZQ(102),IDRDP)
      equivalence (KZQ(103),KDRDP)
      equivalence (KZQ(  2),MS   )
      equivalence (KZQ(  3),NS   )
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
      equivalence (IQQ( 30),IQDDP)
C     !DASH
C     !EJECT
      external MESHED, HI, BYE
C
      call HI ('BROCKEN')
C     !BEG
      II   = -1
      JJ   = -1
      DUMP = .false.
C
      if((IQDDP.gt.0).and.(MO.gt.0)) then
        DUMP = (IU.eq.MS).and.(IL.eq.NS)
C
        if(DUMP) then
C
          II = IDRDP
          if(II.le.0) then
            II = N/2
          end if
C
          JJ = KDRDP
          if(JJ.le.0) then
            JJ = K/2
          end if
C
          if(CALLER.eq.'JAFFA') then
            LAB = 'DR  '
          else
            LAB = 'GII '
          end if
C
          call MESHED (CALLER, 2)
          write (LUEO,100) LAB,IU,IL,II,JJ
  100     format(' ','Debug details of ',A,'calculations. Transition',
     $               I3,'/',I3,5X,'IDRDP =',I4,', KDRDP =',I4)
C
        end if
      end if
C     !END
      call BYE ('BROCKEN')
C
      return
      end
