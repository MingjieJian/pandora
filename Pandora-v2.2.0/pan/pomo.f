      subroutine POMO
     $(XCBL,W,COL,KCOL)
C
C     Rudolf Loeser, 1987 Nov 18
C---- Supervises calculation of actual CO-lines cooling rate.
C     !DASH
      save
C     !DASH
      real*8 COL, W, XCBL
      integer IFREQ, IINTG, IN, IS, IVEC, IWAVE, KCOL, MOX, N, NCB,
     $        NCOW
      logical DOIT
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(55),NCB)
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
      equivalence (LEST(40),NCOW )
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
C     !DASH
C     !EJECT
      external KAMUI, WADDY, ZERO1, WGIVE, HI, BYE
C
      dimension W(*)
C
C               XCBL(Miklen), COL(N)
      dimension XCBL(*),      COL(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IFREQ ),(IN( 2),IWAVE ),(IN( 3),IINTG ),(IN( 4),IVEC  )
C
      call HI ('POMO')
C     !BEG
      DOIT = (KOPAC(27).gt.0).and.(NCOW.gt.0).and.(NCB.eq.1)
C
      if(DOIT) then
C       (Get, and allocate, W allotment)
        call KAMUI (IN,IS,MOX,'POMO')
C
        call WADDY (COL,N,XCBL,W(IFREQ),W(IWAVE),W(IVEC),W(IINTG))
C
C       (Give back W allotment)
        call WGIVE (W,'POMO')
C
        KCOL = 1
      else
        KCOL = 0
        call ZERO1 (COL,N)
      end if
C     !END
      call BYE ('POMO')
C
      return
      end
