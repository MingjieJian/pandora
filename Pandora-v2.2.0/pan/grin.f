      subroutine GRIN
     $(N,Z,ZL,C,CL,IMAGE,TIT,TAUK,LYM,R1,RR,KOUNT,PLTID)
C
C     Rudolf Loeser, 1973 Feb 06
C---- Makes the image of a graph of contributions vs. depth.
C     (This is version 2 of GRIN.)
C     !DASH
      save
C     !DASH
      real*8 BLIMG, C, CL, R1, RR, SIG, TAUK, TLIMG, YL, YU, Z, ZL
      integer IBEG, IEND, KOUNT, KOUNTA, KOUNTB, N, NH, NV
      logical GOOD, LYM
      character IMAGE*(*), NUMERO*1, PLTID*1, TIT*(*)
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
      equivalence (RZQ( 39),BLIMG)
      equivalence (RZQ( 38),TLIMG)
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(38),NUMERO)
C
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C     !EJECT
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
      external LOGO, TRNSPS, KINIT, KKOUNT, SHRIMP, RATTLE, KRIGIA,
     $         ZED, HI, BYE
C
C               ZL(N), R1(N), C(Nopac,N), CL(N,Nopac), RR(N), PLTID(4),
      dimension ZL(*), R1(*), C(*),       CL(*),       RR(*), PLTID(*),
C
C               Z(N), TAUK(N)
     $          Z(*), TAUK(*)
C
      data NV,NH /56, 117/
C
      call HI ('GRIN')
C     !BEG
      SIG = ZL10SMA
C---- Transpose
      call TRNSPS   (C, NOPAC, N, CL)
C---- Compute logs
      call LOGO     (CL, (N*NOPAC), 1, SIG, CL)
C---- Get graph limits
      call ZED      (Z, N, TAUK, N, 0, IBEG, IEND, ZL, TIT, 'GRIN')
      YL = BLIMG
      YU = TLIMG
C---- Initialize plot image
      call KINIT    (IMAGE, ZL(IBEG), ZL(IEND), YL, YU, NV, NH, NUMERO,
     $               GOOD)
      if(.not.GOOD) then
        call KRIGIA (ZL(IBEG), ZL(IEND), YL, YU, NV, NH)
      end if
C---- Enter points
      call KKOUNT   (IMAGE, KOUNTA)
C
      call SHRIMP   (ZL, N, IBEG, IEND, CL, NOPAC, SYMID, NOPAC, SIG,
     $               2, IMAGE)
      call RATTLE   (LYM, N, IBEG, IEND, R1, SIG, ZL, CL, IMAGE,
     $               PLTID(4))
      call RATTLE   (LYM, N, IBEG, IEND, RR, SIG, ZL, CL, IMAGE,
     $               PLTID(3))
C
      call KKOUNT   (IMAGE, KOUNTB)
      KOUNT = KOUNTB-KOUNTA
C     !END
      call BYE ('GRIN')
C
      return
      end
