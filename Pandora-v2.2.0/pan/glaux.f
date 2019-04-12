      subroutine GLAUX
     $(Z,TE,VXS,FRR,VXN,EMU,EMUF,BANDL,BANDU)
C
C     Rudolf Loeser, 1983 Nov 04
C---- Initializes special spectrum save file.
C     !DASH
      save
C     !DASH
      real*8 ADS, BANDL, BANDU, EMU, EMUF, FRR, R1N, TE, VXN, VXS, Z
      integer I, IL, IONST, IQCOT, IQEBI, IQENH, IQEXA, IQFIN, IQIFF,
     $        IQINC, IQSFO, IQSFS, IQWNM, IU, IZOPT, KLIN, KM, L, LDL,
     $        LF, LINT, LUEO, LUSO, MODE, MRR, N, NAB, NCP, NGRL, NGRR,
     $        NL, NT, NVX
      character QELSM*8, QNAME*8
C     !DASH
C
C---- Markers in the file
C
C     ----1  LINE PROFILE INT  (Darius)
C     ----1  LINE PROFILE FLX  (Darius)
C     ----2  CONTINUUM INTENSITY  (Tar)
C     ----3  CONTINUUM FLUX  (Feather)
C     ----4  TRANSITION  (Myron)
C     ---=5  TRANSITION  (Darius)
C     ----6  VELOCITIES  (White)
C     ----7  CONTINUUM INTENSITY /HZ FOR SELECTED BEAMS  (Graham)
C     ----8  DI/DH  (Cremy)
C     ----9  DI/DH  (Marcy)
C     !COM
C     !EJECT
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 5),NT )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(49),KM )
      equivalence (JZQ(42),NVX)
      equivalence (JZQ( 7),L  )
      equivalence (JZQ(19),LF )
      equivalence (JZQ(15),MRR)
      equivalence (JZQ(45),NAB)
      equivalence (JZQ(44),NCP)
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
      equivalence (IQQ( 31),IQSFS)
      equivalence (IQQ(  8),IQSFO)
      equivalence (IQQ(169),IQEXA)
      equivalence (IQQ( 38),IQENH)
      equivalence (IQQ( 51),IQINC)
      equivalence (IQQ( 49),IQFIN)
      equivalence (IQQ( 84),IQCOT)
      equivalence (IQQ(141),IQIFF)
      equivalence (IQQ(208),IQEBI)
      equivalence (IQQ(290),IQWNM)
C     !EJECT
C---- HEADER      as of 1984 Apr 23
      character   HEAD*80
      common      /HEADER/ HEAD
C     Copy of the "HEADER" line for this run.
C     .
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
      equivalence (LINKDS(12),LDL  )
      equivalence (LINKDS(13),LINT )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(28),LUSO )
      equivalence (LUNITS( 6),LUEO )
C     !EJECT
C---- TABLET      as of 2007 Feb 21
      integer     KONLIM,KONWAL
      parameter   (KONLIM=20000)
      real*8      CONTIT,CONWAV
      integer     NUMKON,NUMTRU,NMKUSE,KONADR,KONTYP,KONLIC,KONNSH
      dimension   CONTIT(KONLIM),CONWAV(KONLIM),KONADR(KONLIM),
     $            KONTYP(KONLIM),KONLIC(KONLIM),KONNSH(KONLIM)
      common      /TABLET0/ KONWAL,NUMKON,NUMTRU,NMKUSE
      common      /TABLET1/ CONWAV
      common      /TABLET2/ CONTIT
      common      /TABLET3/ KONADR
      common      /TABLET4/ KONTYP
      common      /TABLET5/ KONLIC
      common      /TABLET6/ KONNSH
C
C     Index, and other data, for Continuum Data Blocks.
C
C     KONWAL - (= KONLIM)
C     NUMKON - total number of Blocks
C     NUMTRU - number of line-specific Blocks ( .le. NUMKON)
C     NMKUSE - number of Blocks to be used for SIAM scratch storage
C
C     CONTIT - Block name (also called "Header Code" or XLTIT,SLTIT)
C     CONWAV - wavelength (Angstroms)
C     KONADR - file address of Block
C     KONTYP - Block code (labelled-common "kwack" via subroutine BEECH)
C     KONNSH - number of supplementary headers (shared blocks only)
C     KONLIC - line transition descriptor, = 100*iu+il (if needed)
C     .
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (QZQ(  1),QNAME)
      equivalence (QZQ(  2),QELSM)
      equivalence (KZQ( 56),IONST)
      equivalence (RZQ( 23),R1N  )
      equivalence (RZQ(  1),ADS  )
      equivalence (KZQ( 36),IZOPT)
      equivalence (KZQ( 21),NGRL )
      equivalence (KZQ( 22),NGRR )
C     !DASH
C     !EJECT
      external PET, BUNT, PANT, PUNT, DUCK, HI, BYE
C
C               VXN(N,NVX), Z(N), BANDU(NAB), VXS(N), FRR(MRR), EMU(L),
      dimension VXN(N,*),   Z(*), BANDU(*),   VXS(*), FRR(*),   EMU(*),
C
C               TE(N), EMUF(LF), BANDL(NAB)
     $          TE(*), EMUF(*),  BANDL(*)
C
      data MODE /1/
C
      call HI ('GLAUX')
C     !BEG
C---- Open file
      call DUCK (LUSO, LUEO)
C---- Write data
      write (LUSO,100) HEAD,QNAME,QELSM,IONST,IQSFS,IQSFO,IQEXA,IQENH,
     $                 IQINC,IQFIN,IQCOT,IQIFF,IQEBI,IQWNM
  100 format('04--PANDORA spectrum calculations save file for:'/
     $       A80/
     $       'This file contains data in the same order as in Printout'/
     $       'It can best be understood by comparing it to Printout'/
     $       3X,A8,2X,'Name of Ion of run'/
     $       3X,A8,2X,'Element symbol'/
     $       I10,2X,'Stage of ionization'/
     $       I10,2X,'SPHERE option'/
     $       I10,2X,'SPHOUT option'/
     $       I10,2X,'EXPAND option'/
     $       I10,2X,'ENHANCE option'/
     $       I10,2X,'INCIDNT option'/
     $       I10,2X,'FINITE option'/
     $       I10,2X,'COLTEMP option'/
     $       I10,2X,'INCIFRNT option'/
     $       I10,2X,'EMERBACK option'/
     $       I10,2X,'WAVENUMB option')
      write (LUSO,101) N,NL,KM,NVX,L,LF,MRR,NUMKON,NAB,NCP
  101 format(I10,2X,'N'/
     $       I10,2X,'NL'/
     $       I10,2X,'KM, = max. # of profile wavelengths'/
     $       I10,2X,'NVX'/
     $       I10,2X,'L'/
     $       I10,2X,'LF'/
     $       I10,2X,'MRR'/
     $       I10,2X,'NW, = # of continuum wavelengths'/
     $       I10,2X,'NAB'/
     $       I10,2X,'NCP')
      write (LUSO,102) R1N,ADS,IZOPT,NGRL,NGRR
  102 format(1PE15.7,2X,'R1N'/
     $         E15.7,2X,'ADS'/
     $       I10,2X,'IZOPT'/
     $       I10,2X,'NGRL'/
     $       I10,2X,'NGRR')
C     !EJECT
      call BUNT    (LUSO, Z,   'Z')
      call BUNT    (LUSO, TE,  'TE')
      call BUNT    (LUSO, VXS, 'VXS')
      if(NVX.gt.0) then
        call PANT  (LUSO, VXN, N, NVX, MODE, 'VXN')
      end if
      call PUNT    (LUSO, EMU,   L,   MODE, 'MU')
      call PUNT    (LUSO, EMUF,  LF,  MODE, 'MUF')
      if(MRR.gt.0) then
        call PUNT  (LUSO, FRR,   MRR, MODE, 'FRR')
      end if
      if(NAB.gt.0) then
        call PUNT  (LUSO, BANDL, NAB, MODE, 'BANDL')
        call PUNT  (LUSO, BANDU, NAB, MODE, 'BANDU')
      end if
C
      if(NT.gt.0) then
        write (LUSO,103) NT
  103   format(I10,2X,'# of line transitions, = # of lines in the ',
     $                'following table:'/
     $         7X,'Each line has: IU,IL,KLIN,LDL,LINT'/
     $         7X,'IU,IL = transition indices'/
     $         7X,'KLIN =1 for radiative, =2 for passive'/
     $         7X,'LDL = # of blended components'/
     $         7X,'LINT =0 means: a (symmetric) half profile is ',
     $            'computed, =1 means: a whole profile is computed')
C
        do 105 I = 1,NT
          call PET (I)
          write (LUSO,104) IU,IL,KLIN,LDL,LINT
  104     format(2I10,5X,3I5)
  105   continue
      end if
C     !END
      call BYE ('GLAUX')
C
      return
      end
