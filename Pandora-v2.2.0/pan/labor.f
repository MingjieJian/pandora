      subroutine LABOR
C
C     Rudolf Loeser, 2005 Nov 03
C---- Sets the signal KLYNF > 0 to indicate that normalization
C     factors, to be used with the simulated background H Ly lines,
C     may need to be computed.
C     When KLYNF has been set > 0, then its values is a conservative
C     estimate of the number of emergent continuum intensity values
C     needed for this computation.
C     (This is version 5 of LABOR.)
C     !DASH
      save
C     !DASH
      real*8 DELTA, WHI, WLO
      integer IQLNC, IQUWT, JSTCN, KHI, KLO, KLYNF, LOOK, NOION, NOTE
      logical DOIT
      character QELSM*8
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(81),KLYNF)
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
      equivalence (IQQ(339),IQLNC)
      equivalence (IQQ( 77),IQUWT)
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
      equivalence (QZQ(  2),QELSM)
      equivalence (KZQ( 94),NOION)
      equivalence (KZQ( 35),JSTCN)
C
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
C     !DASH
C     !EJECT
      external LOOKSD, HI, BYE
C
      data WLO,WHI,DELTA /1.0D3, 1.3D3, 0.D0/
C
      call HI ('LABOR')
C     !BEG
      KLYNF = 0
C
      DOIT = (QELSM(1:3).eq.'H  ').and.(IQLNC.gt.0).and.
     $       (IQUWT.gt.0).and.(NOION.le.0).and.(JSTCN.eq.0)
C
      if(DOIT) then
        call LOOKSD (CONWAV, NUMKON, DELTA, WLO, KLO, NOTE, LOOK)
        call LOOKSD (CONWAV, NUMKON, DELTA, WHI, KHI, NOTE, LOOK)
        if(KHI.gt.KLO) then
          KLYNF = (KHI-KLO)+10
        end if
      end if
C     !END
      call BYE ('LABOR')
C
      return
      end
