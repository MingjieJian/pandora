      subroutine SALINA
     $(N,NO,XCBL,TE,XJBAR,B,BJL,XL,XMBJL,XHBJL,RHMFF,RHFF)
C
C     Rudolf Loeser, 1980 Feb 19
C---- Computes net cooling rates for H- free-free and H free-free.
C     (This is version 2 of SALINA.)
C     !DASH
      save
C     !DASH
      real*8 B, BJL, RHFF, RHMFF, TE, XCBL, XHBJL, XJBAR, XL, XMBJL, XW
      integer I, J, KKB, KKCO, KKJNU, N, NO
      logical KILROY
C     !COM
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK(15),KKB   )
      equivalence (KKK(19),KKCO  )
      equivalence (KKK(13),KKJNU )
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
      external LEYTE, HERALD, BLISS, ADVENT, HI, BYE
C
C               XJBAR(N,2), BJL(N,2), XCBL(Miklen), B(N,2), XHBJL(N,2),
      dimension XJBAR(N,*), BJL(N,*), XCBL(*),      B(N,*), XHBJL(N,*),
C
C               XMBJL(N,2), TE(N), RHMFF(N), RHFF(N), XL(N,2)
     $          XMBJL(N,*), TE(*), RHMFF(*), RHFF(*), XL(N,*)
C
      dimension XW(2)
C
      call HI ('SALINA')
C     !BEG
      KILROY = .true.
C
      J = 0
      do 100 I = 1,NUMKON
        J = J+1
        XW(J) = CONWAV(I)
        call LEYTE    (XCBL, MIKLEN, KONADR(I))
        call HERALD   (N, XCBL(KKB), XCBL(KKJNU), XCBL(KKCO),
     $                 XJBAR(1,J), B(1,J), BJL(1,J), XL(1,J),
     $                 XMBJL(1,J), XHBJL(1,J), TE, CONWAV(I))
        call BLISS    (N, XMBJL(1,J), RHMFF, XHBJL(1,J), RHFF,I)
        if(J.eq.2) then
          call ADVENT (KILROY, NO, N, J, XW, XJBAR, BJL, B, XL, XMBJL,
     $                 XHBJL)
          J = 0
        end if
  100 continue
      if(J.gt.0) then
        call ADVENT   (KILROY, NO, N, J, XW, XJBAR, BJL, B, XL, XMBJL,
     $                 XHBJL)
      end if
C     !END
      call BYE ('SALINA')
C
      return
      end
