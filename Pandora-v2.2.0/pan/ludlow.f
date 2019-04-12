      subroutine LUDLOW
     $(N,HEND,HE2K,HE1,DEE,DELTA,F,G)
C
C     Rudolf Loeser, 1998 Mar 24
C---- Computes f and g for Helium-II, for MASHA.
C     (This is version 2 of LUDLOW.)
C     !DASH
      save
C     !DASH
      real*8 B1, B2, CFHE, DEE, DELTA, F, G, HE1, HE2K, HE31, HEND
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
      equivalence (RZQ(152),CFHE )
C     !DASH
      external DIVIDE, AWN, HI, BYE
C
C               DEE(4,5,N), DELTA(7,N), HEND(N), HE2K(N), HE1(N), F(N),
      dimension DEE(4,5,*), DELTA(7,*), HEND(*), HE2K(*), HE1(*), F(*),
C
C               G(N)
     $          G(*)
C
      call HI ('LUDLOW')
C     !BEG
      do 100 I = 1,N
        call DIVIDE (HE2K(I),HE1(I),HE31)
        call AWN    (DEE(1,1,I),HE1(I),HE2K(I),HEND(I),B1,B2)
C
        F(I) = HEND(I)*(B1-HE31*B2)
        G(I) = CFHE-(HE1(I)*DELTA(6,I)-HE2K(I)*DELTA(7,I))
  100 continue
C     !END
      call BYE ('LUDLOW')
C
      return
      end
