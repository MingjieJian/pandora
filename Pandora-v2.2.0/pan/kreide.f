      subroutine KREIDE
     $(XLM,L,CORE,OXYGEN1,YES,DUMP)
C
C     Rudolf Loeser, 2004 Apr 21
C---- Tells whether to compute the L'th O-I background line at
C     this wavelength, and sets the debug switch.
C     !DASH
      save
C     !DASH
      real*8 CORE, XLM
      integer L, LOXDS
      logical DUMP, OXYGEN1, YES
C     !COM
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
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(209),LOXDS)
C     !DASH
      external KAKI, HI, BYE
C
      call HI ('KREIDE')
C     !BEG
      call KAKI (XLM, CORE, OXYGEN1, OXWLO(L), OXWVL(L), OXWHI(L),
     $           LOXDS, YES, DUMP)
C     !END
      call BYE ('KREIDE')
C
      return
      end
