      subroutine TUNE
     $(LIM)
C
C     Rudolf Loeser, 2004 Aug 06
C---- Computes the level index of the highest Hydrogen Lyman line
C     in the background.
C     (This is version 3 of TUNE.)
C     !DASH
      save
C     !DASH
      integer LIM, NLY
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
C---- FERGO       as of 2004 Aug 05
      parameter   (MHYL=24)
      integer     MHYL, IUHY, ILHY
      real*8      HYWVL, HYWLO, HYWHI, HYNUU, HYNUL
      dimension   HYWVL(MHYL), HYWLO(MHYL), HYWHI(MHYL), HYNUU(MHYL),
     $            HYNUL(MHYL), IUHY(MHYL),  ILHY(MHYL)
      common      /FERGO0/ HYWVL,HYWLO,HYWHI
      common      /FERGO1/ HYNUU,HYNUL
      common      /FERGO2/ IUHY,ILHY
C     Data for Hydrogen Lyman lines in the background.
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
      equivalence (KZQ(184),NLY  )
C     !DASH
      external HI, BYE
C
      call HI ('TUNE')
C     !BEG
      LIM = 0
      if(KOPAC(37).gt.0) then
        LIM = IUHY(MHYL)
      else if((KOPAC(34).gt.0).or.(KOPAC(36).gt.0)) then
        LIM = NLY
      else if((KOPAC(11).gt.0).or.(KOPAC(16).gt.0)) then
        LIM = 2
      end if
C     !END
      call BYE ('TUNE')
C
      return
      end
