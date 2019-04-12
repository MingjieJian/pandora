      subroutine MIIKO
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1972 Jul 19
C---- Allocates scratch storage for FRIGG.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NC, NDT, NNC
      character CALLER*(*)
C     !COM
C---- IONDATA     as of 2007 Jan 12
C     Data tables for the built-in models of "non-LTE" ions; these
C     models are used to compute bound-free absorption and emission.
C     Hydrogen (of course!) is treated as a special case.
      integer     NPION,NIONL,NDPNT
C
      parameter   (NPION=14)
C     NPION     = number of ion models, as follows:
C     01: H      02: C-I    03: Si-I   04: He-I   05: He-II  06: Al-I
C     07: Mg-I   08: Fe-I   09: Na-I   10: Ca-I   11: O-I    12: S-I
C     13: O-II   14: O-III
C
      parameter   (NIONL=15)
C     NIONL     = maximum number of levels in each model
C
      parameter   (NDPNT=150)
C     NDPNT     = maximum length of tables specifying the wavelength-
C     dependence of the absorption in each continuum, by
C     piece-wise linear approximation.
C
C     REMEMBER to recompile all users when changing NPION, NIONL, NDPNT.
C
      real*8      PILEVL,XLMTHR,CCPLEV,SCPLEV,XLMTAB,RCPTAB
      integer     LIMDAT,MAXDATL,LEND,NPTABL
      character   LLABEL*16
      logical     LLPRNT
      dimension   LIMDAT(            NPION), LLPRNT(            NPION),
     $            PILEVL(      NIONL,NPION), XLMTHR(      NIONL,NPION),
     $            NPTABL(      NIONL,NPION), CCPLEV(      NIONL,NPION),
     $            SCPLEV(      NIONL,NPION), LLABEL(      NIONL,NPION),
     $            XLMTAB(NDPNT,NIONL,NPION), RCPTAB(NDPNT,NIONL,NPION)
C
C     LIMDAT    = actual number of levels in each model (LIMDAT should
C                 equal LIMPOP in labelled common POPDATA)
C     MAXDATL   = maximum value of LIMDAT
C     LEND      = sum of LIMDAT
C     LLABEL    = "term designation" of each level of each model
C     LLPRNT    = data tables print switch
C     PILEVL    = statistical weight of each level of each model
C     XLMTHR    = threshhold wavelengths of continua
C     NPTABL    = actual number of data points in absorption data table
C                 of each level of each model (used only when > 0)
C     CCPLEV    = threshhold absorption factors
C     SCPLEV    = exponent of power-law wavelength dependence of
C                 absorption of each level of each model (used only
C                 when > 0; should be > 0 when corresponding NPTABL = 0)
C     XLMTAB    = wavelength values for which RCPTAB is specified
C     RCPTAB    = absorption in the continuum of a level of a model
C                 (At wavelengths < XLMTAB(NPTABL), a power law
C                  with exponent = 3 is used.)
C
      common      /IODAT01/ MAXDATL,LEND
      common      /IODAT02/ LIMDAT
      common      /IODAT03/ LLPRNT
      common      /IODAT04/ PILEVL
      common      /IODAT05/ XLMTHR
      common      /IODAT06/ NPTABL
      common      /IODAT07/ CCPLEV
      common      /IODAT08/ SCPLEV
      common      /IODAT09/ LLABEL
      common      /IODAT10/ XLMTAB
      common      /IODAT11/ RCPTAB
C     .
C---- FARGO       as of 2004 Jul 19
      parameter   (MHEL=7, LHEL=7)
      integer     MHEL, LHEL, IUHE, ILHE, LDLHE
      real*8      HEMAS, HESKE, HEWVL, HEWLO, HEWHI, HENUU, HENUL, HEAUL
      real*8      HEPU,  HEPL,  HEDDL, HECDL, HECRD, HECVW, HECSK
      dimension   HEWVL(MHEL), HEWLO(MHEL), HEWHI(MHEL), HENUU(MHEL),
     $            HENUL(MHEL), HEPU(MHEL),  HEPL(MHEL),  HEAUL(MHEL),
     $            IUHE(MHEL),  ILHE(MHEL),  LDLHE(MHEL)
      dimension   HEDDL(LHEL,MHEL), HECDL(LHEL,MHEL),
     $            HECRD(LHEL,MHEL), HECVW(LHEL,MHEL), HECSK(LHEL,MHEL)
      common      /FARGO0/ HEMAS,HESKE
      common      /FARGO1/ HEWVL,HEWLO,HEWHI
      common      /FARGO2/ HENUU,HENUL,HEPU,HEPL
      common      /FARGO3/ HEAUL,HEDDL,HECDL,HECRD,HECVW,HECSK
      common      /FARGO4/ IUHE,ILHE,LDLHE
C     Data for Helium-II lines in the background.
C     .
C---- FURGO       as of 2004 Jun 11
      parameter   (MOXL=11)
      integer     MOXL, IUOX, ILOX
      real*8      OXMAS, OXSKE, OXWVL, OXWLO, OXWHI, OXNUU, OXNUL
      real*8      OXPU,  OXPL,  OXAUL, OXCRD, OXCVW, OXCSK
      dimension   OXWVL(MOXL), OXWLO(MOXL), OXWHI(MOXL), OXNUU(MOXL),
     $            OXNUL(MOXL), OXPU(MOXL),  OXPL(MOXL),  OXAUL(MOXL),
     $            OXCRD(MOXL), OXCVW(MOXL), OXCSK(MOXL),
     $            IUOX(MOXL),  ILOX(MOXL)
      common      /FURGO0/ OXMAS,OXSKE
      common      /FURGO1/ OXWVL,OXWLO,OXWHI
      common      /FURGO2/ OXNUU,OXNUL,OXPU,OXPL
      common      /FURGO3/ OXAUL,OXCRD,OXCVW,OXCSK
      common      /FURGO4/ IUOX,ILOX
C     Data for Oxygen-I lines in the background.
C     .
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(21),NDT)
C     !EJECT
C---- FIRGO       as of 2005 Jul 07
      parameter   (MHEE=4)
      integer     MHEE, IUHEE, ILHEE
      real*8      HEEMAS, HEEWVL, HEEWLO, HEEWHI, HEENUU, HEENUL
      real*8      HEEAUL, HEEPU,  HEEPL,  HEECRD, HEECVW, HEECSK
      real*8      HEESKE
      dimension   HEEWVL(MHEE), HEEWLO(MHEE), HEEWHI(MHEE),
     $            HEENUU(MHEE), HEENUL(MHEE), HEEPU(MHEE),
     $            HEEPL(MHEE),  HEEAUL(MHEE), HEECRD(MHEE),
     $            HEECVW(MHEE), HEECSK(MHEE),
     $            IUHEE(MHEE),  ILHEE(MHEE)
      common      /FIRGO0/ HEEMAS,HEESKE
      common      /FIRGO1/ HEEWVL,HEEWLO,HEEWHI
      common      /FIRGO2/ HEENUU,HEENUL,HEEPU,HEEPL
      common      /FIRGO3/ HEEAUL,HEECRD,HEECVW,HEECSK
      common      /FIRGO4/ IUHEE,ILHEE
C     Data for Helium lines in the background.
C     .
C---- WURGO       as of 2007 Jan 25
      parameter   (MX2L=1, LX2L=3)
      integer     MX2L, LX2L, IUX2, ILX2, LDLX2
      real*8      X2MAS, X2SKE, X2WVL, X2WLO, X2WHI, X2NUU, X2NUL, X2AUL
      real*8      X2PU,  X2PL,  X2DDL, X2CDL, X2CRD, X2CVW, X2CSK
      dimension   X2WVL(MX2L), X2WLO(MX2L), X2WHI(MX2L), X2NUU(MX2L),
     $            X2NUL(MX2L), X2PU(MX2L),  X2PL(MX2L),  X2AUL(MX2L),
     $            IUX2(MX2L),  ILX2(MX2L),  LDLX2(MX2L)
      dimension   X2DDL(LX2L,MX2L), X2CDL(LX2L,MX2L),
     $            X2CRD(LX2L,MX2L), X2CVW(LX2L,MX2L), X2CSK(LX2L,MX2L)
      common      /WURGO0/ X2MAS,X2SKE
      common      /WURGO1/ X2WVL,X2WLO,X2WHI
      common      /WURGO2/ X2NUU,X2NUL,X2PU,X2PL
      common      /WURGO3/ X2AUL,X2DDL,X2CDL,X2CRD,X2CVW,X2CSK
      common      /WURGO4/ IUX2,ILX2,LDLX2
C     Data for Oxygen-II lines in the background.
C     .
C---- WARGO       as of 2007 Jan 18
      parameter   (MX3L=2, LX3L=6)
      integer     MX3L, LX3L, IUX3, ILX3, LDLX3
      real*8      X3MAS, X3SKE, X3WVL, X3WLO, X3WHI, X3NUU, X3NUL, X3AUL
      real*8      X3PU,  X3PL,  X3DDL, X3CDL, X3CRD, X3CVW, X3CSK
      dimension   X3WVL(MX3L), X3WLO(MX3L), X3WHI(MX3L), X3NUU(MX3L),
     $            X3NUL(MX3L), X3PU(MX3L),  X3PL(MX3L),  X3AUL(MX3L),
     $            IUX3(MX3L),  ILX3(MX3L),  LDLX3(MX3L)
      dimension   X3DDL(LX3L,MX3L), X3CDL(LX3L,MX3L),
     $            X3CRD(LX3L,MX3L), X3CVW(LX3L,MX3L), X3CSK(LX3L,MX3L)
      common      /WARGO0/ X3MAS,X3SKE
      common      /WARGO1/ X3WVL,X3WLO,X3WHI
      common      /WARGO2/ X3NUU,X3NUL,X3PU,X3PL
      common      /WARGO3/ X3AUL,X3DDL,X3CDL,X3CRD,X3CVW,X3CSK
      common      /WARGO4/ IUX3,ILX3,LDLX3
C     Data for Oxygen-III lines in the background.
C     .
C     !DASH
C     !EJECT
      external  WGET, WLCK, HI, BYE
      intrinsic max
C
      dimension IN(*)
C
      call HI ('MIIKO')
C     !BEG
      call WGET (IS,  CALLER)
C
      NC  = max(MOXL,MHEL,MHEE,MX2L,MX3L)
      NNC = N*NC
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+MAXDATL
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+N
      IN( 5) = IN( 4)+N
      IN( 6) = IN( 5)+N
      IN( 7) = IN( 6)+N
      IN( 8) = IN( 7)+NDT
      IN( 9) = IN( 8)+N
      IN(10) = IN( 9)+N
      IN(11) = IN(10)+NNC
C
      MUX    = IN(11)+NNC
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('MIIKO')
C
      return
      end
