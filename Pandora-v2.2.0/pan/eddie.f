      subroutine EDDIE
     $(S,SN,N,IMG,EDITS,SOFN,W)
C
C     Rudolf Loeser, 1984 Nov 01
C---- Controls S editing, for LINDEN.
C     (This is version 2 of EDDIE.)
C     !DASH
      save
C     !DASH
      real*8 S, SN, W
      integer ICE, IFO, IL, IMG, IN, IQSED, IQSRJ, IRAT, IRATE, IS,
     $        ISNE, ISO, IU, KLT, MO, MOX, N
      logical EDITS, PRINT, SOFN
      character LABEL*25
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
      equivalence (LINKDS( 4),ICE  )
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
      equivalence (IQQ(149),IQSED)
      equivalence (IQQ(246),IQSRJ)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external DIEDE, EDWINA, MINUSD, ABSVECD, WGIVE, HI, BYE
C
      dimension W(*)
C
C               S(N), SN(N), IMG(N)
      dimension S(*), SN(*), IMG(*)
C
      dimension IN(5)
      equivalence
     $(IN( 1),ISO   ),(IN( 2),IRAT  ),(IN( 3),IRATE ),(IN( 4),ISNE  ),
     $(IN( 5),IFO   )
C     !EJECT
C
      call HI ('EDDIE')
C     !BEG
      EDITS = .false.
      SOFN  = .false.
      if(IQSED.gt.0) then
        call MINUSD        (S,1,N,KLT)
        if(KLT.gt.0) then
          if(ICE.eq.0) then
            if(KLT.lt.N) then
C
C             (Get, and allocate, W allotment)
              call DIEDE   (IN,IS,MOX,'EDDIE')
C
              PRINT = (IQSRJ.gt.0).or.(MO.gt.0)
              write (LABEL,100) IU,IL
  100         format('S for transition',I4,'/',I2)
              call EDWINA  (S,SN,N,LABEL,PRINT,IMG,W(IFO),W(ISO),
     $                      W(IRAT),W(IRATE),W(ISNE))
C
C             (Give back W allotment)
              call WGIVE   (W,'EDDIE')
C
            else
              call ABSVECD (SN,1,N,S,1,N)
              SOFN = .true.
            end if
C
            EDITS = .true.
          end if
        end if
      end if
C     !END
      call BYE ('EDDIE')
C
      return
      end
