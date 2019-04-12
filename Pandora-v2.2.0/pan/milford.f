      subroutine MILFORD
     $(N,HEND,BETA,HE2K,HE1,DEE,DELTA,F,G)
C
C     Rudolf Loeser, 1998 Mar 24
C---- Computes f and g for Helium-I, for MASHA.
C     (This is version 2 of MILFORD.)
C     !DASH
      save
C     !DASH
      real*8 B1, B2, BETA, CFHE, DEE, DELTA, F, G, HE1, HE2K, HE32,
     $       HEND, ONE
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
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, AWN, HI, BYE
C
C               DEE(4,5,N), DELTA(7,N), HEND(N), BETA(N), HE1(N), G(N),
      dimension DEE(4,5,*), DELTA(7,*), HEND(*), BETA(*), HE1(*), G(*),
C
C               HE2K(N), F(N)
     $          HE2K(*), F(*)
C
      call HI ('MILFORD')
C     !BEG
      do 100 I = 1,N
        call DIVIDE (HE2K(I),BETA(I),HE32)
        call AWN    (DEE(1,1,I),HE1(I),HE2K(I),HEND(I),B1,B2)
C
        F(I) = HEND(I)*((ONE+HE32)*B1+HE32*B2)
        G(I) = CFHE+(BETA(I)+HE2K(I))*DELTA(4,I)+HE2K(I)*DELTA(5,I)
  100 continue
C     !END
      call BYE ('MILFORD')
C
      return
      end
