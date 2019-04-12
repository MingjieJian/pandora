      subroutine ALDO
     $(LU,ALPHA,NL)
C
C     Rudolf Loeser, 1992 Jun 05
C---- Prints, for AGOUTA.
C     !DASH
      save
C     !DASH
      real*8 ALPHA, CLVLS
      integer LU, NL
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
      equivalence (RZQ(134),CLVLS)
C     !DASH
      external VECOUT, HI, BYE
C
C               ALPHA(NL)
      dimension ALPHA(*)
C
      call HI ('ALDO')
C     !BEG
      write (LU,100) CLVLS
  100 format(' ','CLEVELS =',1PE22.4)
C
      call VECOUT (LU,ALPHA,NL,'ALPHA')
C     !END
      call BYE ('ALDO')
C
      return
      end
