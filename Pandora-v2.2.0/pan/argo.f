      subroutine ARGO
     $(RHMFF,RHFF,XCBL,TE,W,KHFF)
C
C     Rudolf Loeser, 1980 Feb 19
C---- Controls the computation of net radiative cooling rates for
C     H- free-free and H free-free.
C---- XCBL is the buffer for Continuum Data blocks.
C     (This is version 2 of ARGO.)
C     !DASH
      save
C     !DASH
      real*8 RHFF, RHMFF, TE, W, XCBL
      integer IB, IBJL, IHBJ, IJBR, IMBJ, IN, IQFCD, IS, IXL, KHFF, LU,
     $        MOX, N, NO
      logical DOIT
      character QNAME*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (IQQ(139),IQFCD)
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
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
      external ZERO1, MARILYN, ZEUS, SALINA, WGIVE, HI, BYE
C
      dimension W(*)
C
C               XCBL(Miklen), RHMFF(N), RHFF(N), TE(N)
      dimension XCBL(*),      RHMFF(*), RHFF(*), TE(*)
C
      dimension IN(6)
      equivalence
     $(IN( 1),IB    ),(IN( 2),IJBR  ),(IN( 3),IBJL  ),(IN( 4),IXL   ),
     $(IN( 5),IMBJ  ),(IN( 6),IHBJ  )
C
      call HI ('ARGO')
C     !BEG
      call ZERO1     (RHMFF, N)
      call ZERO1     (RHFF,  N)
C
      DOIT = (QNAME.eq.'HYDROGEN').and.(NUMKON.gt.1)
C
      if(DOIT) then
C       (Get, and allocate, W allotment)
        call MARILYN (IN, IS, MOX, 'ARGO')
C
        call ZEUS    (NO, IQFCD, LU)
        call SALINA  (N, LU, XCBL, TE, W(IJBR), W(IB), W(IBJL),
     $                W(IXL), W(IMBJ), W(IHBJ), RHMFF, RHFF)
C
C       (Give back W allotment)
        call WGIVE   (W, 'ARGO')
C
        KHFF = 1
      else
        KHFF = 0
      end if
C     !END
      call BYE ('ARGO')
C
      return
      end
