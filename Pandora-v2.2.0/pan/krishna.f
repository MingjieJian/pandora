      subroutine KRISHNA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1970 Jan 20
C---- Allocates scratch storage for WHEEL.
C     !DASH
      save
C     !DASH
      integer IN, IQDIC, IQORI, IS, L, LNW, MUX, N, NLNW, NLNWI, NLNWO
      character CALLER*(*)
C     !COM
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
      equivalence (IQQ(101),IQORI)
      equivalence (IQQ(288),IQDIC)
C
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 7),L  )
C     !DASH
C     !EJECT
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('KRISHNA')
C     !BEG
      call WGET (IS,  CALLER)
C
      LNW  = L*NMKUSE
      NLNW = N*LNW
C
      NLNWO = 0
      if(IQORI.gt.0) then
        NLNWO = NLNW
      end if
C
      NLNWI = 0
      if(IQDIC.gt.0) then
        NLNWI = NLNW
      end if
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+NLNWI
      IN( 3) = IN( 2)+L*N
      IN( 4) = IN( 3)+L
      IN( 5) = IN( 4)+LNW
      IN( 6) = IN( 5)+LNW
      IN( 7) = IN( 6)+LNW
      IN( 8) = IN( 7)+LNW
      IN( 9) = IN( 8)+NMKUSE
      IN(10) = IN( 9)+NMKUSE
C
      IN(11) = IN(10)+NMKUSE
      IN(12) = IN(11)+NLNWO
      MUX    = IN(12)+NLNWO
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('KRISHNA')
C
      return
      end
