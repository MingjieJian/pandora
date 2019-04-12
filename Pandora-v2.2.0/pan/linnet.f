      subroutine LINNET
C
C     Rudolf Loeser, 1986 Mar 07
C---- Computes NXRW, the number of wavelengths available for the X-rays
C     cooling rates calculation.
C     !DASH
      save
C     !DASH
      real*8 WHI, WLO
      integer IHI, ILO, IPEX, IQXRC, LUEO, NXRW
      logical DOIT
      character QNAME*8
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
      equivalence (QZQ(  1),QNAME)
      equivalence (KZQ( 18),IPEX )
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
      equivalence (LEST(28),NXRW )
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
      equivalence (IQQ(196),IQXRC)
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
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
C---- XRAYLIM     as of 1986 Mar 06
      real*8      XRAYLO,XRAYHI
      common      /XRAYLIM/ XRAYLO,XRAYHI
C     Wavelength limits (Angstroms) for X-ray opacity.
C     .
C     !DASH
C     !EJECT
      external  ZEREN, MESHED, MASHED, HI, BYE
      intrinsic max
C
      call HI ('LINNET')
C     !BEG
      NXRW = 0
      DOIT = (QNAME.eq.'HYDROGEN').and.(IQXRC.gt.0)
C
      if(DOIT) then
        call ZEREN    (XRAYLO, ILO, WLO, XRAYHI, IHI, WHI)
C
        NXRW = max(((IHI-ILO)+1),0)
C
        if((IPEX.lt.0).or.(IPEX.eq.10)) then
          call MESHED ('LINNET', 2)
          write (LUEO,100) NUMKON,ILO,IHI,NXRW,XRAYLO,WLO,WHI,XRAYHI
  100     format(' ',3I10,5X,'NXRW=',I10/
     $           ' ',1P4E20.12)
          call MASHED ('LINNET')
        end if
      end if
C     !END
      call BYE ('LINNET')
C
      return
      end
