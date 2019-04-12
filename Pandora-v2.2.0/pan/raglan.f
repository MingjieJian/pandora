      subroutine RAGLAN
     $(N,HND,RABD,HK,DEE,DELTA,F,G)
C
C     Rudolf Loeser, 1998 Mar 23
C---- Computes f and g for Hydrogen, for MASHA.
C     (This is version 2 of RAGLAN.)
C     !DASH
      save
C     !DASH
      real*8 CFH, DEE, DELTA, F, G, HK, HND, RABD
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
      equivalence (RZQ(113),CFH  )
C     !DASH
      external HI, BYE
C
C               DEE(4,5,N), DELTA(7,N), HND(N), RABD(N), HK(N), F(N),
      dimension DEE(4,5,*), DELTA(7,*), HND(*), RABD(*), HK(*), F(*),
C
C               G(N)
     $          G(*)
C
      call HI ('RAGLAN')
C     !BEG
      do 100 I = 1,N
        F(I) = DEE(1,1,I)*(RABD(I)*HND(I))
        G(I) = CFH*RABD(I)+HK(I)*DELTA(1,I)
  100 continue
C     !END
      call BYE ('RAGLAN')
C
      return
      end
