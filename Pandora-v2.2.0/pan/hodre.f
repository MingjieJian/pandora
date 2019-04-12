      subroutine HODRE
     $(N,TE,TCO,HND,CON,CHN,OHN,NO)
C
C     Rudolf Loeser, 1981 May 19
C---- Computes molecular number densities:
C     CON - CO; CHN - CH; and OHN - OH.
C     (This is version 2 of HODRE.)
C     !DASH
      save
C     !DASH
      real*8 ABD, CHN, CON, HND, OHN, TCO, TE
      integer IN, KODE, N, NO
      character QELSM*8
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
      equivalence (QZQ(  2),QELSM)
C     !DASH
      external HUPA, HATRALI, CHECKER, WENDY, HI, BYE
C
C               TE(N), HND(N), TCO(N), CON(N), CHN(N), OHN(N)
      dimension TE(*), HND(*), TCO(*), CON(*), CHN(*), OHN(*)
C
      dimension ABD(3)
C
      call HI ('HODRE')
C     !BEG
C---- Get abundances
      call HUPA    (QELSM, ABD, IN, KODE)
C
C---- Compute number density, and abundance correction
      call HATRALI (TE, TCO, HND, CON, CHN, OHN, N, ABD(1), ABD(2), NO)
C
C---- Checksums
      call CHECKER (CON, 1, N, 'N(CO)')
      call CHECKER (CHN, 1, N, 'N(CH)')
      call CHECKER (OHN, 1, N, 'N(OH)')
C---- Continuum recalculation control
      call WENDY   (CON, 1, N,  8, 'HODRE')
      call WENDY   (CHN, 1, N, 40, 'HODRE')
      call WENDY   (OHN, 1, N, 41, 'HODRE')
C     !END
      call BYE ('HODRE')
C
      return
      end
