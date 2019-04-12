      subroutine KRAUT
     $(MO,KSE,KSEDA)
C
C     Rudolf Loeser, 1998 Jul 31
C---- Prints a secondary header, for SWALLOW.
C     !DASH
      save
C     !DASH
      integer IL, IQLSG, IQLSP, IU, KSE, KSEDA, LSFP, MO
      logical GRAPH, TABLE
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
      equivalence (LINKDS(14),LSFP )
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
      equivalence (IQQ(287),IQLSP)
      equivalence (IQQ(250),IQLSG)
C     !DASH
      external LINER, DASHER, ABJECT, HI, BYE
C
      call HI ('KRAUT')
C     !BEG
      TABLE = (LSFP.gt.0).or.(IQLSP.gt.0)
      GRAPH = (LSFP.gt.0).or.(IQLSG.gt.0)
      if((MO.gt.0).and.(TABLE.or.GRAPH)) then
        if(KSEDA.eq.2) then
          if(KSE.eq.1) then
            call LINER  (2,MO)
            call DASHER (MO)
            call LINER  (1,MO)
            write (MO,100)
  100       format(' ','D i f f u s i o n   Analysis (option DIFFANA',
     $                 ' = on): Statistical Equilibrium Equations ',
     $                 'using TIJ (= "PIJ" computed with GVL = 0).')
          else
            call ABJECT (MO)
            call DASHER (MO)
            call LINER  (1,MO)
            write (MO,101) IU,IL
  101       format(' ','N o r m a l   Results for transition (',I2,
     $                 '/',I2,'): Statistical Equilibrium Equations ',
     $                 'using regular PIJ.')
          end if
          call LINER    (1,MO)
          call DASHER   (MO)
          call LINER    (2,MO)
        end if
      end if
C     !END
      call BYE ('KRAUT')
C
      return
      end
