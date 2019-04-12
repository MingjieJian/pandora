      subroutine CHIRP
     $(W,XPBL,HEELE)
C
C     Rudolf Loeser, 2001 Dec 14
C---- Gets Helium electrons.
C     (This is version 2 of CHIRP.)
C     !DASH
      save
C     !DASH
      real*8 HEELE, W, XPBL, YH, ZERO, dummy
      integer IHE1K, IHE2, IHE2K, IN, IS, MOX, N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (RZQ( 15),YH   )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external ZERO1, MAGUA, POPUTIL, GAMUA, WGIVE, HI, BYE
C
      dimension W(*)
C
C               XPBL(Lenpbl), HEELE(N)
      dimension XPBL(*),      HEELE(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IHE1K ),(IN( 2),IHE2  ),(IN( 3),IHE2K )
C     !EJECT
C
      call HI ('CHIRP')
C     !BEG
      if(YH.gt.ZERO) then
C       (Get, and allocate, W allotment)
        call MAGUA   (IN,IS,MOX,'CHIRP')
C
C----   Get ionized He-I
        call POPUTIL (XPBL,4, 0,dummy,1,W(IHE1K),0,dummy,0,dummy)
C----   Get He-II and ionized He-II
        call POPUTIL (XPBL,5, 0,dummy,1,W(IHE2K),1,W(IHE2),0,dummy)
C----   Compute Helium electrons
        call GAMUA   (N,W(IHE1K),W(IHE2),W(IHE2K),HEELE)
C
C       (Give back W allotment)
        call WGIVE   (W,'CHIRP')
      else
        call ZERO1   (HEELE,N)
      end if
C     !END
      call BYE ('CHIRP')
C
      return
      end
