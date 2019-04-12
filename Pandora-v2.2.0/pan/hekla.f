      subroutine HEKLA
     $(XLM,CORE,HELIUM2,N,XNE,TE,V,HE2N,HE2BD,H1,VEX,EMU,CABS)
C
C     Rudolf Loeser, 2004 Jul 20
C---- Computes a set of He-II background line opacity values.
C     (This is version 2 of HEKLA.)
C     !DASH
      save
C     !DASH
      real*8 CABS, CORE, EMU, H1, HE2BD, HE2N, TE, V, VEX, XLM, XNE
      integer I, IL, IU, L, LHEDS, N
      logical DMPI, DUMP, HELIUM2, KILROY, YES
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
      external HECKLE, TEKLA, MALTA, YALTA, MINNA, HESSAR, HI, BYE
C
C               TE(N), H1(N), HE2N(N,Limp), V(N), XNE(N), CABS(N,Nlin),
      dimension TE(*), H1(*), HE2N(N,*),    V(*), XNE(*), CABS(N,*),
C
C               VEX(N), HE2BD(N,Limp)
     $          VEX(*), HE2BD(N,*)
C     !EJECT
C
      call HI ('HEKLA')
C     !BEG
      KILROY = .true.
C
      do 101 L = 1,MHEL
        call HECKLE     (XLM, L, CORE, HELIUM2, YES, DUMP)
        if(YES) then
C
          call MALTA    (XLM, DUMP, 'HEKLA')
          if(DUMP) then
            call HESSAR (XLM, L, KILROY)
          end if
C
          IU = IUHE(L)
          IL = ILHE(L)
          do 100 I = 1,N
            call MINNA  (DUMP, I, LHEDS, DMPI)
            call TEKLA  (L, XLM, EMU, XNE(I), TE(I), V(I), H1(I),
     $                   VEX(I), HE2N(I,1), HE2N(I,IL), HE2BD(I,IU),
     $                   HE2BD(I,IL), I, DMPI, CABS(I,L))
  100     continue
          call YALTA    (DUMP, 'HEKLA')
        end if
  101 continue
C     !END
      call BYE ('HEKLA')
C
      return
      end
