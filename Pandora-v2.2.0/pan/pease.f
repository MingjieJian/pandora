      subroutine PEASE
     $(HND,G,EM,FEM,H)
C
C     Rudolf Loeser, 1980 Nov 07
C---- Computes a new set of HND values, for H.S.E.
C     !DASH
      save
C     !DASH
      real*8 EM, FEM, G, H, HND, ONE, RAT
      integer LHHSE, N
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
      equivalence (KZQ(129),LHHSE)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HAND, DIVIDE, WROTH, WENDY, HI, BYE
C
C               HND(N), G(N), H(N), EM(N)
      dimension HND(*), G(*), H(*), EM(*)
C
      call HI ('PEASE')
C     !BEG
C---- Compute
      call DIVIDE (HND(LHHSE), G(LHHSE), RAT)
      call HAND   (RAT, N, G, H, ONE, HND)
      call WROTH  (N, EM, FEM, HND)
C---- Continuum Recalculation control
      call WENDY  (HND, 1, N, 4, 'PEASE')
C     !END
      call BYE ('PEASE')
C
      return
      end
