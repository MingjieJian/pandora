      subroutine TICKLE
     $(XLM,L,CORE,HELIUM1,YES,DUMP)
C
C     Rudolf Loeser, 2005 Jun 24
C---- Tells whether to compute the L'th He-I background line at
C     this wavelength, and sets the debug switch.
C     !DASH
      save
C     !DASH
      real*8 CORE, XLM
      integer L, LEEDS
      logical DUMP, HELIUM1, YES
C     !COM
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
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(215),LEEDS)
C     !DASH
      external KAKI, HI, BYE
C
      call HI ('TICKLE')
C     !BEG
      call KAKI (XLM, CORE, HELIUM1, HEEWLO(L), HEEWVL(L), HEEWHI(L),
     $           LEEDS, YES, DUMP)
C     !END
      call BYE ('TICKLE')
C
      return
      end
