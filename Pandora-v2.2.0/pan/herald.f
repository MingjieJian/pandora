      subroutine HERALD
     $(N,BB,BJBAR,BCO,XJBAR,B,BJL,XL,XMBJL,XHBJL,TE,XLM)
C
C     Rudolf Loeser, 1980 Feb 19
C---- Sets up integrands, and intermediates to be printed, for SALINA.
C     (This is version 2 of HERALD.)
C     !DASH
      save
C     !DASH
      real*8 B, BB, BCO, BJBAR, BJL, TE, X, XHBJL, XJBAR, XL, XLM,
     $       XMBJL, dummy
      integer I, N
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
C     !DASH
C     !EJECT
      external MOVE1, HUNK, QEXP1, HI, BYE
C
C               BCO(Nopac,N), XMBJL(N), XL(N), BJL(N), BB(N), XJBAR(N),
      dimension BCO(NOPAC,*), XMBJL(*), XL(*), BJL(*), BB(*), XJBAR(*),
C
C               XHBJL(N), BJBAR(N), TE(N), B(N)
     $          XHBJL(*), BJBAR(*), TE(*), B(*)
C
      call HI ('HERALD')
C     !BEG
      call MOVE1   (BJBAR,N,XJBAR)
      call MOVE1   (BB   ,N,B    )
      do 100 I = 1,N
        call HUNK  (TE(I),XLM,2,X)
        call QEXP1 (X,dummy,2,XL(I))
        BJL(I) = (B(I)-XJBAR(I))*XL(I)
C
        XMBJL(I) = BCO( 2,I)*BJL(I)
        XHBJL(I) = BCO(13,I)*BJL(I)
  100 continue
C     !END
      call BYE ('HERALD')
C
      return
      end
