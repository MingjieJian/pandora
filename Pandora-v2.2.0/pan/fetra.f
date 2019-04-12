      subroutine FETRA
     $(N,IS,Z,VAX)
C
C     Rudolf Loeser, 2003 Mar 20
C---- Computes a shock velocity.
C     !DASH
      save
C     !DASH
      real*8 ARG, SCPS, SCVA, SCVB, SCVS, TRM, VAX, Z
      integer I, IS, N
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
      equivalence (RZQ(170),SCVA )
      equivalence (RZQ(171),SCVS )
      equivalence (RZQ(174),SCVB )
      equivalence (RZQ(175),SCPS )
C     !DASH
      external HI, BYE
C
C               Z(N), VAX(N)
      dimension Z(*), VAX(*)
C
      call HI ('FETRA')
C     !BEG
      if(IS.gt.1) then
        do 100 I = 1,IS
          ARG = (Z(I)-Z(IS))/SCVS
          TRM = exp(ARG)
          VAX(I) = -SCVA*TRM
  100   continue
      end if
      if(IS.lt.N) then
        do 101 I = (IS+1),N
          ARG = (Z(IS)-Z(I))/SCPS
          TRM = exp(ARG)
          VAX(I) =  SCVB*TRM
  101   continue
      end if
C     !END
      call BYE ('FETRA')
C
      return
      end
