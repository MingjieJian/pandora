      subroutine ROHU
     $(XCBL,W,XRAY,KRAY)
C
C     Rudolf Loeser, 1986 Mar 07
C---- Supervises calculation of X-rays cooling rate.
C     !DASH
      save
C     !DASH
      real*8 W, XCBL, XRAY
      integer IFREQ, IINTG, IN, IS, IVEC, IWAVE, KRAY, MOX, N, NXRW
      logical DOIT
C     !COM
C---- OPACITY     as of 2007 Jan 12
C     Paraphernalia for background absorption/emission contributors.
C
C     (Must recompile BARE, BRACE, FORAGER & SHARI when changing NABS!)
      parameter   (NABS=45)
C
      integer     NABS,NOPAC,KOPAC,TOPAC,LOPAC
      character   CNAME*24,SYMID*1,SHNAM*6
      dimension   KOPAC(NABS),LOPAC(NABS),SYMID(NABS)
      dimension   CNAME(NABS),SHNAM(NABS)
C
      common      /OPAC1/ NOPAC
      common      /OPAC2/ KOPAC
      common      /OPAC3/ LOPAC
      common      /OPAC4/ CNAME
      common      /OPAC5/ SYMID
      common      /OPAC6/ SHNAM
C
C     NOPAC = number of contributors
C     KOPAC = contributor status switch: 0 = don't use, 1 = use
C     CNAME = name (description) of contributor
C             NOTE: This sequence of names establishes the indices or
C                   ordinals by which the contributors are also known.
C     SHNAM = "short" name of contributor
C     LOPAC = "printout order" list of contributor ordinals
C     SYMID = scratch space for symbolic identifiers
C     .
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
C     !DASH
C     !EJECT
      external KALONG, WISENT, ZERO1, WGIVE, HI, BYE
C
      dimension W(*)
C
C               XCBL(Miklen), XRAY(N)
      dimension XCBL(*),      XRAY(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IFREQ ),(IN( 2),IWAVE ),(IN( 3),IINTG ),(IN( 4),IVEC  )
C
      call HI ('ROHU')
C     !BEG
      DOIT = (KOPAC(26).gt.0).and.(NXRW.gt.0)
C
      if(DOIT) then
C       (Get, and allocate, W allotment)
        call KALONG (IN,IS,MOX,'ROHU')
C
        call WISENT (XRAY,N,XCBL,W(IFREQ),W(IWAVE),W(IVEC),W(IINTG))
C
C       (Give back W allotment)
        call WGIVE  (W,'ROHU')
C
        KRAY = 1
      else
        KRAY = 0
        call ZERO1  (XRAY,N)
      end if
C     !END
      call BYE ('ROHU')
C
      return
      end
