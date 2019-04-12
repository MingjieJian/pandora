      subroutine SHEKLA
     $(XLM,CORE,HELIUM2,N,TE,HE2BD,DMPF,CEMI)
C
C     Rudolf Loeser, 2004 Jul 20
C---- Computes a set of He-II background line source function values.
C     (This is version 3 of SHEKLA.)
C     !DASH
      save
C     !DASH
      real*8 CEMI, CORE, HE2BD, TE, XLM
      integer I, IL, IU, L, LHEDS, N
      logical DMPF, DMPI, DUMP, HELIUM2, YES
C     !COM
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
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(211),LHEDS)
C     !DASH
      external HECKLE, THEKLA, MALTA, YALTA, MINNA, HI, BYE
C
C               CEMI(N,Nlin), HE2BD(N,Limd), TE(N)
      dimension CEMI(N,*),    HE2BD(N,*),    TE(*)
C     !EJECT
C
      call HI ('SHEKLA')
C     !BEG
      DMPF = .false.
C
      do 101 L = 1,MHEL
        call HECKLE     (XLM, L, CORE, HELIUM2, YES, DUMP)
        if(YES) then
          if(DUMP) then
            DMPF = .true.
          end if
C
          call MALTA    (XLM, DUMP, 'SHEKLA')
C
          IU = IUHE(L)
          IL = ILHE(L)
          do 100 I = 1,N
            call MINNA  (DUMP, I, LHEDS, DMPI)
            call THEKLA (L, TE(I), HE2BD(I,IU), HE2BD(I,IL), I, DMPI,
     $                   CEMI(I,L))
  100     continue
          call YALTA    (DUMP, 'SHEKLA')
        end if
  101 continue
C     !END
      call BYE ('SHEKLA')
C
      return
      end
