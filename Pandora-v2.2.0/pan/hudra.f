      subroutine HUDRA
     $(NT,ISB1,NPT,NOTA,NOTX,NONC,LFLX,KM,NTK,NLFDB,NVSB,NVSBP,MLSFP,
     $ JIBR)
C
C     Rudolf Loeser, 1976 Jun 03
C---- Computes summaries of Line Profile switches and Partial
C     Redistribution calculation control switches.
C
C---- NOTA=0 if no regular line profiles need be shown, >0 otherwise.
C     NOTX=0 if no eclipse line profiles need be shown, >0 otherwise.
C     JIBR=0 if no "ion broadening" calculation is needed, >0 otherwise.
C     NPT    is the number of passive transitions.
C     NONC   is the number of radiative transitions with
C            Partial Redistribution calculation.
C     NLFDB  is the number of transitions with frequency-dependent
C            background data.
C     NTK    is the maximum number of continuum wavelengths needed for
C            all P.R.D. continuum calculations.
C     LFLX   is the number of transitions with Line Flux Distribution
C            calculation.
C     NVSB   is the number of transitions for which the Sobolev escape
C            probability solution is used.
C     NVSBP  is that part of NVSB for which line profile calculations
C            will be done.
C     MLSFP  is the number of transitions with "Line Source Function"
C            output.
C
C     (This is version 3 of HUDRA.)
C     !DASH
      save
C     !DASH
      real*8 FF, ONE, ZERO
      integer I, ICE, IFDB, IL, ILFLX, IPEX, IPRO, IQHBR, IQLGT, ISB1,
     $        IU, IUL, JIBR, KLIN, KM, LFLX, LIBR, LSFP, LSFT, LUEO,
     $        MLSFP, NLFDB, NOION, NONC, NOTA, NOTX, NPT, NT, NTA, NTK,
     $        NTX, NVSB, NVSBP
      character QELSM*8
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
      equivalence (KZQ( 18),IPEX )
      equivalence (KZQ( 94),NOION)
      equivalence (QZQ(  2),QELSM)
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !EJECT
C---- THULE       as of 1999 Dec 07
      integer     MAXLV,MXTRA
      parameter   (MAXLV=50)
      parameter   (MXTRA=(MAXLV*(MAXLV-1))/2)
C     (Remember to recompile ADAM when changing MAXLV.)
      integer     LMTRA,NUMTRN,NUMBLK,LINNAM,LI1ADR,LI2ADR,LI3ADR
      dimension   LI1ADR(MXTRA),LI2ADR(MXTRA),LI3ADR(MXTRA)
      dimension   LINNAM(MXTRA)
      common      /THULE0/ LMTRA,NUMTRN,NUMBLK
      common      /THULE1/ LINNAM
      common      /THULE2/ LI1ADR
      common      /THULE3/ LI2ADR
      common      /THULE4/ LI3ADR
C
C     Indices and Names of the Line Intensity Data Blocks.
C     LMTRA  -    = MAXLV
C     NUMTRN -    number of transitions (i.e. AIJ .ne. 0), which is
C                 the number of Line Intensity Data Blocks allocated;
C     NUMBLK -    number of Line Intensity Data Blocks in actual use
C                 (i.e. number of radiative and passive transitions);
C     LINNAM -    block name, = 100*IU+IL;
C     LI1ADR -    block address in scratch memory, part 1;
C     LI2ADR -    block address in scratch memory, part 2;
C     LI3ADR -    block address in scratch memory, part 3.
C     .
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
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
      equivalence (IQQ(273),IQHBR)
      equivalence (IQQ(  9),IQLGT)
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
      equivalence (LINKDS( 4),ICE  )
      equivalence (LINKDS(11),ILFLX)
      equivalence (LINKDS( 5),IPRO )
      equivalence (LINKDS( 3),KLIN )
      equivalence (LINKDS(10),LSFT )
      equivalence (LINKDS(15),IFDB )
      equivalence (LINKDS(14),LSFP )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external  PET, GROUSE, INTRANS, MESHED, MASHED, HALT, HI, BYE
      intrinsic mod
C
C               ISB1(NT), FF(5)
      dimension ISB1(*),  FF(5)
C
      call HI ('HUDRA')
C     !BEG
      NOTA   = 0
      NOTX   = 0
      NONC   = 0
      NLFDB  = 0
      NPT    = 0
      LFLX   = 0
      NVSB   = 0
      NVSBP  = 0
      MLSFP  = 0
      NUMBLK = 0
      LIBR   = 0
      JIBR   = 0
      NTK    = 0
C     !EJECT
      if((NUMTRN.gt.0).and.(NOION.le.0)) then
        do 101 I = 1,NUMTRN
          call PET     (I)
          call INTRANS (IU, IL, 'HUDRA', IUL)
          if((IPRO.lt.0).or.(IPRO.gt.3)) then
            write (MSSLIN(1),100) IPRO
  100       format('IPRO =',I12,', which is 0, 1, 2, or  3.')
            call HALT  ('HUDRA', 1)
          end if
          if(ICE.ne.0) then
            NONC = NONC+1
          end if
          if(ILFLX.gt.0) then
            LFLX = LFLX+1
          end if
          if(KLIN.eq.2) then
            NPT = NPT+1
          end if
          if(IFDB.gt.0) then
            NLFDB = NLFDB+1
          end if
          if(LSFP.gt.0) then
            MLSFP = MLSFP+1
          end if
          NTA  = mod(IPRO,2)
          NOTA = NOTA+NTA
          NTX  = IPRO/2
          NOTX = NOTX+NTX
          if((KLIN.eq.1).or.(KLIN.eq.2)) then
            NUMBLK = NUMBLK+1
          end if
          if((LSFT.eq.2).and.(ISB1(IUL).gt.1)) then
            NVSB = NVSB+1
            if(IPRO.gt.0) then
              NVSBP = NVSBP+1
            end if
          end if
          call GROUSE  (0, ONE, ONE, ONE, ONE, ONE, FF)
          if(FF(5).ne.ZERO) then
            LIBR = LIBR+1
          end if
  101   continue
C     !EJECT
        NTK = KM*NONC
C
        JIBR = LIBR
        if(JIBR.gt.0) then
          if(.not.((IQHBR.gt.0).and.(QELSM(:3).eq.'H  '))) then
            JIBR = 0
          end if
        end if
C
        if((IPEX.lt.0).or.(IPEX.eq.8)) then
          call MESHED ('HUDRA', 2)
          write (LUEO,102) NUMTRN,NOTA,NOTX,NPT,IQLGT,NONC,LFLX,KM,
     $                     NTK,NLFDB,NVSB,NVSBP,LIBR,JIBR,QELSM
  102     format(' ','HUDRA',5X,'NUMTRN',I3,5X,'NOTA',I3,5X,
     $               'NOTX',I3,5X,'NPT',I3,5X,'LIGHT',I2/
     $           ' ','HUDRA',5X,'NONC',I3,5X,'LFLX',I3,5X,'KM',I3,
     $               5X,'NTK',I3,5X,'NLFDB',I3/
     $           ' ','HUDRA',5X,'NVSB',I3,5X,'NVSBP',I3,5X,'LIBR',I3,
     $               5X,'JIBR',I3,5X,'QELSM [',A3,']')
          call MASHED ('HUDRA')
        end if
C
        if(IQLGT.le.0) then
          NOTA = 0
          NOTX = 0
        end if
      end if
C     !END
      call BYE ('HUDRA')
C
      return
      end
