      subroutine ORYX
     $(N,H,C,S,WH,CALLER,DUMP)
C
C     Rudolf Loeser, 1985 Jan 24
C---- Dumps, for CHULYM.
C     !DASH
      save
C     !DASH
      real*8 C, H, S, WH
      integer IHDMP, IL, IQEXA, IRAY, IU, LLXI, LUEO, MO, MS, N, NRAY,
     $        NS
      logical DUMP
      character CALLER*(*)
C     !COM
C---- ARCHER      as of 2004 May 12
      integer     NNKOD, NNKODS
      parameter   (NNKOD=3)
C     (Be sure to recompile POD when changing NNKOD.)
      dimension   NNKODS(NNKOD)
      common      /ARCHER/ NNKODS
C     Diana/Orion Data Blocks control parameters.
C
C     (These parameters are packed and unpacked by "POD".)
C       LLXI - frequency index.
C       IRAY - angle or ray index (Orion only: expanding atmosphere);
C              (in the spherical case, Shell rays are enumerated first,
C              followed by Disk rays).
C       NRAY - number of depth levels intersected by this ray;
C              (differs from N only for Shell rays).
C     .
      equivalence
     $(NNKODS( 1),LLXI  ),(NNKODS( 2),IRAY  ),(NNKODS( 3),NRAY  )
C     .
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
      equivalence (KZQ( 84),IHDMP)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
      equivalence (LUNITS( 8),MO   )
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
C     !DASH
      external MESHED, VECOUT, ARROUT, HI, BYE
C
C               H(N), C(N), S(N), WH(N,N)
      dimension H(*), C(*), S(*), WH(*)
C     !EJECT
C
      call HI ('ORYX')
C     !BEG
      DUMP = (MO.gt.0).and.(IHDMP.gt.0).and.((IU.eq.MS).and.(IL.eq.NS))
C
      if(DUMP) then
        call MESHED ('ORYX', 2)
        write (LUEO,100) IU,IL
  100   format(' ','Calculating H for transition (',I2,'/',I2,').')
C
        if(IQEXA.le.0) then
          write (LUEO,101) LLXI,IRAY
  101     format(' ','Frequency #',I4,5X,'Look-angle #',I4)
        else
          write (LUEO,102) LLXI,IRAY,NRAY
  102     format(' ','Frequency #',I4,5X,'Ray #',I5,5X,'NRAY',I5)
        end if
C
        call VECOUT (LUEO, H , N,    'Current H'                )
        call VECOUT (LUEO, C , N,    'Angle integration weights')
        call VECOUT (LUEO, S , N,    'Source Function'          )
        call ARROUT (LUEO, WH, N, N, 'WH = PHI-operator'        )
      end if
C     !END
      call BYE ('ORYX')
C
      return
      end
