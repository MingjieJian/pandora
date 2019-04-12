      subroutine RIPPLE
     $(X,W,IW,WVL,LTYPE,XCBL,KILROY,OPAC,DUMP)
C
C     Rudolf Loeser, 1980 Jul 21
C---- Computes the opacity at WVL Angstroms,
C     (avoiding a full-fledged Continuum calculation).
C
C     XCBL is the Continuum Data Block.
C
C     (This is version 2 of RIPPLE.)
C     !DASH
      save
C     !DASH
      real*8 ONE, OPAC, W, WVL, X, XCBL, XLTIT, ZERO
      integer IISWA, IJOP, IN, IS, IW, IWS, IXLYB, IXPBL, JN, KISLV,
     $        KKCO, KKISWA, KKKONT, KKLAMD, KKOPAC, KRESN, LTYPE, LUEO,
     $        MOX, MUX, N, jummy
      logical DUMP, KILROY
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK(54),KKLAMD)
      equivalence (KKK( 6),KKOPAC)
      equivalence (KKK(43),KKISWA)
      equivalence (KKK(19),KKCO  )
      equivalence (KKK( 3),KKKONT)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !EJECT
C---- KONOUT      as of 2004 Jan 08
      integer     KONLUN,KONLUR,KONLUD,KONHED
      common      /KONOUT/ KONLUN,KONLUR,KONLUD,KONHED
C     Logical output unit numbers for Continuum Calculations.
C             *** Initialized in PARLOR. ***
C
C     KONLUN: potential output unit number
C     KONLUR: unit number for regular output
C
C     KONLUD: =1 if dump output is authorized
C     KONHED: =1 if wavelength header has already been printed
C     .
C---- LYSTER      as of 2006 Nov 01
      integer     NLL,NLLY,ILB,LENLYB
      logical     INIHLL,INDPTH
      parameter   (NLLY=500)
      dimension   ILB(18)
      common      /LYSTER1/ NLL,LENLYB,ILB
      common      /LYSTER2/ INIHLL,INDPTH
C     Hydrogen Lyman lines background absorber paraphernalia
C
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
      external NICOBAR, MELANIA, JUDITH, POPIO, MOVE1, MORTAIN, CONTDI,
     $         GRID, LIMAN, EMEER, MOVEI, WGIVE, IGIVE, MESHED, MASHED,
     $         FRIGG, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               XCBL(Miklen), OPAC(N)
      dimension XCBL(*),      OPAC(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),IXPBL ),(IN( 2),IXLYB )
C
      dimension JN(2)
      equivalence
     $(JN( 1),IISWA ),(JN( 2),IJOP  )
C
      call HI ('RIPPLE')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call NICOBAR   (IN, IS , MOX, 'RIPPLE')
      call MORTAIN   (JN, IWS, MUX, 'RIPPLE')
C     (Initialize populations buffer)
      call POPIO     ('INIT', jummy, W(IXPBL))
C     !EJECT
      if(KILROY) then
C----   Initialize Continuum Block
        KILROY = .false.
        INIHLL = .true.
        call LIMAN   (0, KRESN, KISLV)
        call JUDITH  (0, 0, 0, LTYPE, XLTIT)
        call MOVEI   (KOPAC, 1, NOPAC, IW(IJOP), 1, NOPAC)
        call MELANIA (X, ZERO, IW(IJOP))
        call GRID    (XCBL, XLTIT, WVL, ZERO, ONE, ONE, KRESN, KISLV,
     $                IW(IJOP), IW(IJOP), NOPAC)
      end if
C---- Compute opacity
      KONLUD = 1
      call FRIGG     (X, W, IW, W(IXPBL), XCBL, W(IXLYB), 1, 0, 1, 0)
C---- Cull out opacity
      call MOVE1     (XCBL(KKOPAC), N, OPAC)
C
      if(DUMP) then
C----   Optional printout
        call MESHED  ('RIPPLE', 2)
        call CONTDI  (XCBL(KKISWA), 1, NOPAC, IW(IISWA), 1, NOPAC)
        call CONTDI  (XCBL(KKKONT), 1, NOPAC, IW(IJOP ), 1, NOPAC)
        call EMEER   (XCBL(KKLAMD), IW(IISWA), IW(IJOP), NOPAC,
     $                XCBL(KKCO), N, LUEO, 'Absorbers')
        call MASHED  ('RIPPLE')
      end if
C
C     (Give back W & IW allotments)
      call WGIVE     (W , 'RIPPLE')
      call IGIVE     (IW, 'RIPPLE')
C     !END
      call BYE ('RIPPLE')
C
      return
      end
