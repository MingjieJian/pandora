      subroutine MOTO
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1990 Nov 30
C---- Allocates scratch storage for MIAS.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NL, NNL
      character CALLER*(*)
C     !COM
C---- XINGU       as of 1999 Sep 21
      real*8      AXED,BXED,RCHX,DELCHX
      character   NAMXED*3
      integer     NXI,NPQLM,NPQMX
      parameter   (NXI=10)
C     (Remember to change all users when changing NXI)
      parameter   (NPQLM=15)
C     (Maximum permitted value of principal quantum number n)
C     (NPQLM must not exceed LIMDAT(1) [in popdata.inc], the
C     number of levels in the Hydrogen population ion model.)
      dimension   AXED(NXI), BXED(NXI), NAMXED(NXI)
      dimension   RCHX(NPQLM,NPQLM), DELCHX(NPQLM,NPQLM)
      common      /XINGU1/ AXED,BXED,RCHX,DELCHX
      common      /XINGU2/ NAMXED
      common      /XINGU3/ NPQMX
C---- Charge Exchange data tables
C     .
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C     !DASH
C     !EJECT
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('MOTO')
C     !BEG
      call WGET (IS,  CALLER)
C
      NNL = N*NL
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+NNL
      IN( 3) = IN( 2)+NNL
      IN( 4) = IN( 3)+N
      MUX    = IN( 4)+NPQLM**2
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('MOTO')
C
      return
      end
