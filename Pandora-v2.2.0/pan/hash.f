      subroutine HASH
     $(LU,KBX,BXI,NDWM)
C
C     Rudolf Loeser, 1978 Nov 08
C---- Prints parameters for line opacity contributors to the background
C     (Continuum).
C     !DASH
      save
C     !DASH
      real*8 BXI, dummy
      integer IONST, KBX, KEEOK, KHEOK, KOXOK, KX2OK, KX3OK, LU, NDWM,
     $        jummy
      logical ANY, LBHE, LBHY, LBOX, LBX2, LBX3, LEHE
      character QELSM*8
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
      equivalence (LEST(72),KOXOK)
      equivalence (LEST(77),KEEOK)
      equivalence (LEST(73),KHEOK)
      equivalence (LEST(57),KX2OK)
      equivalence (LEST(58),KX3OK)
C     !EJECT
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 56),IONST)
      equivalence (QZQ(  2),QELSM)
C     !DASH
      external LINER, LYDIA, PRIVET, HEMP, SHOUT, HI, BYE
C
C               BXI(KBX)
      dimension BXI(*)
C
      call HI ('HASH')
C     !BEG
      if(LU.gt.0) then
        LBHY = (KOPAC(11).gt.0).or.(KOPAC(16).gt.0).or.
     $         (KOPAC(34).gt.0).or.(KOPAC(36).gt.0).or.(KOPAC(37).gt.0)
        LBHE =  KOPAC(33).gt.0
        LEHE =  KOPAC(35).gt.0
        LBOX =  KOPAC(39).gt.0
        LBX2 =  KOPAC(44).gt.0
        LBX3 =  KOPAC(45).gt.0
        ANY  =  LBHY.or.LBHE.or.LEHE.or.LBOX.or.LBX2.or.LBX3
C
        if(ANY) then
          call LINER   (2, LU)
          write (LU,100) NDWM
  100     format(' ','BXI - standard XIs for background lines',19X,
     $               'NDWM =',I6)
          call PRIVET  (LU, BXI, KBX)
        end if
C
        if(LBHY) then
          call LINER   (2, LU)
          write (LU,101)
  101     format(' ','**********     Hydrogen Lyman lines: see '
     $               'separate printout subsection, below')
        end if
C     !EJECT
        if(LEHE) then
          call LINER   (2, LU)
          write (LU,102)
  102     format(' ','**********     Helium-I lines:')
          call LINER   (1, LU)
          call LYDIA   (LU, MHEE, 1, HEEMAS, HEESKE, IUHEE, ILHEE,
     $                  HEEWLO, HEEWVL, HEEWHI, HEENUU, HEEPU, HEENUL,
     $                  HEEPL, HEEAUL, HEECRD, HEECVW, HEECSK, jummy,
     $                  dummy, dummy, 'HASH')
          if((QELSM(1:2).eq.'HEE').and.(IONST.eq.1)) then
            call SHOUT (LU, KEEOK, 'HELIUM1')
          end if
        end if
C
        if(LBHE) then
          call LINER   (2, LU)
          write (LU,103)
  103     format(' ','**********     Helium-II lines:')
          call LINER   (1, LU)
          call LYDIA   (LU, MHEL, LHEL, HEMAS, HESKE, IUHE, ILHE,
     $                  HEWLO, HEWVL, HEWHI, HENUU, HEPU, HENUL,
     $                  HEPL, HEAUL, HECRD, HECVW, HECSK, LDLHE,
     $                  HEDDL, HECDL, 'HASH')
          if((QELSM(1:2).eq.'HE').and.(IONST.eq.2)) then
            call SHOUT (LU, KHEOK, 'HELIUM2')
          end if
        end if
C     !EJECT
        if(LBOX) then
          call LINER   (2, LU)
          write (LU,104)
  104     format(' ','**********     Oxygen-I lines:')
          call LINER   (1, LU)
          call LYDIA   (LU, MOXL, 1, OXMAS, OXSKE, IUOX, ILOX,
     $                  OXWLO, OXWVL, OXWHI, OXNUU, OXPU, OXNUL,
     $                  OXPL, OXAUL, OXCRD, OXCVW, OXCSK, jummy,
     $                  dummy, dummy, 'HASH')
          if((QELSM(1:2).eq.'O ').and.(IONST.eq.1)) then
            call SHOUT (LU, KOXOK, 'OXYGEN')
          end if
        end if
C
        if(LBX2) then
          call LINER   (2, LU)
          write (LU,105)
  105     format(' ','**********     Oxygen-II lines:')
          call LINER   (1, LU)
          call LYDIA   (LU, MX2L, LX2L, X2MAS, X2SKE, IUX2, ILX2,
     $                  X2WLO, X2WVL, X2WHI, X2NUU, X2PU, X2NUL,
     $                  X2PL, X2AUL, X2CRD, X2CVW, X2CSK, LDLX2,
     $                  X2DDL, X2CDL, 'HASH')
          if((QELSM(1:2).eq.'O ').and.(IONST.eq.2)) then
            call SHOUT (LU, KX2OK, 'OXYGEN2')
          end if
        end if
C
        if(LBX3) then
          call LINER   (2, LU)
          write (LU,106)
  106     format(' ','**********     Oxygen-III lines:')
          call HEMP    (LU)
          call LYDIA   (LU, MX3L, LX3L, X3MAS, X3SKE, IUX3, ILX3,
     $                  X3WLO, X3WVL, X3WHI, X3NUU, X3PU, X3NUL,
     $                  X3PL, X3AUL, X3CRD, X3CVW, X3CSK, LDLX3,
     $                  X3DDL, X3CDL, 'HASH')
          if((QELSM(1:2).eq.'O ').and.(IONST.eq.3)) then
            call SHOUT (LU, KX3OK, 'OXYGEN3')
          end if
        end if
      end if
C     !END
      call BYE ('HASH')
C
      return
      end
