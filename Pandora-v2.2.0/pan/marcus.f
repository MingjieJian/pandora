      subroutine MARCUS
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1971 Apr 14
C---- Allocates scratch storage for RAJA.
C     !DASH
      save
C     !DASH
      integer IN, IQAVK, IS, MNM, MNW, MRR, MUX, N, NNW, NP, NRPMX
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(15),MRR)
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
      equivalence (IQQ(303),IQAVK)
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
      equivalence (LEST( 8),NRPMX)
C     !DASH
C     !EJECT
      external  WLCK, WGET, HI, BYE
      intrinsic max
C
      dimension IN(*)
C
      call HI ('MARCUS')
C     !BEG
      call WGET (IS,  CALLER)
C
      NNW = 0
      MNW = 0
      if(IQAVK.gt.0) then
        NNW = NMKUSE*N
        MNW = NMKUSE*MRR
      end if
      MNM = max(N,MRR)
      NP  = N+1
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+NRPMX
      IN( 3) = IN( 2)+NRPMX
      IN( 4) = IN( 3)+NRPMX
      IN( 5) = IN( 4)+N
      IN( 6) = IN( 5)+NP
      IN( 7) = IN( 6)+MNM
      IN( 8) = IN( 7)+N
      IN( 9) = IN( 8)+N
      IN(10) = IN( 9)+MRR
      IN(11) = IN(10)+MRR
C
      IN(12) = IN(11)+MRR
      IN(13) = IN(12)+MRR
      IN(14) = IN(13)+MRR
      IN(15) = IN(14)+MNW
      IN(16) = IN(15)+NRPMX
      IN(17) = IN(16)+NNW
      IN(18) = IN(17)+NNW
      IN(19) = IN(18)+NNW
      MUX    = IN(19)+MNW
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('MARCUS')
C
      return
      end
