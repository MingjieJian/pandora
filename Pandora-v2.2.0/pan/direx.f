      subroutine DIREX
     $(TE,V,XIB,AMASS,WVL,WLO,WHI,M)
C
C     Rudolf Loeser, 2004 Jun 14
C---- Sets up wavelength cutoffs for background-contributing lines.
C     (This is version 2 of DIREX.)
C     !DASH
      save
C     !DASH
      real*8 AMASS, CDW, TE, V, WHI, WLO, WVL, XIB
      integer J, KBX, M, NDWM
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(11),KBX)
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(128),NDWM )
C     !DASH
      external MOLE, HI, BYE
C
C               TE(N), V(N), XIB(KBX), WVL(M), WLO(M), WHI(M)
      dimension TE(*), V(*), XIB(*),   WVL(*), WLO(*), WHI(*)
C
      call HI ('DIREX')
C     !BEG
      do 100 J = 1,M
        call MOLE (TE, V, NDWM, AMASS, WVL(J), CDW)
        WLO(J) = WVL(J)-CDW*XIB(KBX)
        WHI(J) = WVL(J)+CDW*XIB(KBX)
  100 continue
C     !END
      call BYE ('DIREX')
C
      return
      end
