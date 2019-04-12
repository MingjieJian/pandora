      subroutine HESSAR
     $(XLM,L,KILROY)
C
C     Rudolf Loeser, 2004 Jul 20
C---- Dumps for HEKLA.
C     (This is version 2 of HESSAR.)
C     !DASH
      save
C     !DASH
      real*8 XLM
      integer L, LUEO
      logical KILROY
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
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, LYDIA, HI, BYE
C     !EJECT
C
      call HI ('HESSAR')
C     !BEG
      if(KILROY) then
        write (LUEO,100) XLM
  100   format(' ','Details of calculation of background Helium-II ',
     $             'lines at wavelength =',1PE20.13//
     $         ' ','Built-in He-II lines data:')
        call LINER (1, LUEO)
        call LYDIA (LUEO, MHEL, LHEL, HEMAS, HESKE, IUHE, ILHE, HEWLO,
     $              HEWVL, HEWHI, HENUU, HEPU, HENUL, HEPL, HEAUL,
     $              HECRD, HECVW, HECSK, LDLHE, HEDDL, HECDL, 'HESSAR')
        KILROY = .false.
      end if
      call LINER   (2, LUEO)
      write (LUEO,101) L,HEWVL(L)
  101 format(' ','HEHEHEHEHE Line #',I3,' at',1PE20.13,' Angstroms')
C     !END
      call BYE ('HESSAR')
C
      return
      end
