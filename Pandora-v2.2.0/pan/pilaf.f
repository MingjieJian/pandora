      subroutine PILAF
     $(REAL,DMP0,DMP1,DMP2)
C
C     Rudolf Loeser, 1980 Jan 25
C---- Sets up dumps for BOTTOM.
C     !DASH
      save
C     !DASH
      integer IL, IPR01, IPR02, IPR03, IPR04, IQPD0, IQPD1, IQPD2, IU,
     $        LUEO, MO, MS, MULT, NS
      logical DMP0, DMP1, DMP2, DUMP, REAL
      character HEAD*80
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
      equivalence (KZQ(  2),MS   )
      equivalence (KZQ(  3),NS   )
      equivalence (KZQ( 57),IPR01)
      equivalence (KZQ( 58),IPR02)
      equivalence (KZQ( 59),IPR03)
      equivalence (KZQ( 60),IPR04)
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
      equivalence (IQQ(159),IQPD0)
      equivalence (IQQ(137),IQPD1)
      equivalence (IQQ(138),IQPD2)
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
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external GENIE, MESHED, LINSEED, LINER, MASHED, HI, BYE
C
      data MULT /100/
C     !EJECT
C
      call HI ('PILAF')
C     !BEG
      DMP0 = .false.
      DMP1 = .false.
      DMP2 = .false.
      if(MO.gt.0) then
        if(REAL.and.((IU*MULT+IL).eq.(MS*MULT+NS))) then
C
          DMP0 = IQPD0.gt.0
          DMP1 = IQPD1.gt.0
          DMP2 = IQPD2.gt.0
C
          if(((IPR02-IPR01).lt.0).or.((IPR01+IPR02).le.0)) then
            DMP1 = .false.
            DMP2 = .false.
          end if
C
          if(((IPR04-IPR03).lt.0).or.((IPR03+IPR04).le.0)) then
            DMP2 = .false.
          end if
C
          DUMP = DMP0.or.DMP1.or.DMP2
          if(DUMP) then
            call MESHED  ('PILAF', 2)
            call GENIE   (HEAD)
            write (LUEO,100) HEAD,IPR01,IPR02,IPR03,IPR04
  100       format(' ',A//
     $             ' ','Frequency/Ray index limits:',2I10,
     $                 '; Depth detail index limits: ',2I10)
            call LINSEED (LUEO)
            call LINER   (2, LUEO)
            write (LUEO, 101) DMP0,DMP1,DMP2
  101       format(' ','DMP0 =',L3,',   DMP1 =',L3,',   DMP2 =',L3)
            call MASHED  ('PILAF')
          end if
C
        end if
      end if
C     !END
      call BYE ('PILAF')
C
      return
      end
