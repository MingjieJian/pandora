      subroutine HODRA
     $(N,TE,PF,SA)
C
C     Rudolf Loeser, 2001 Dec 01
C---- Computes the Saha-Boltzmann term for the ion-of-the-run.
C     (This is version 2 of HODRA.)
C     !DASH
      save
C     !DASH
      real*8 PF, SA, TE, XNUK
      integer I, N
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
      equivalence (RZQ(  9),XNUK )
C     !DASH
      external SAHARA, HI, BYE
C
C               TE(N), PF(N), SA(N)
      dimension TE(*), PF(*), SA(*)
C
      call HI ('HODRA')
C     !BEG
      do 100 I = 1,N
        call SAHARA (XNUK, TE(I), PF(I), SA(I))
  100 continue
C     !END
      call BYE ('HODRA')
C
      return
      end
