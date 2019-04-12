      subroutine SOUR
     $(XNK,XND,BDI,XNE,H2N,DONE)
C
C     Rudolf Loeser, 1984 Nov 20
C---- Saves debug checksums, for GORSE.
C     (This is version 2 of SOUR.)
C     !DASH
      save
C     !DASH
      real*8 BDI, H2N, XND, XNE, XNK
      integer IOVER, ITHSL, LITER, N, NL, NNL
      logical DONE
      character TIT*40
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST( 2),IOVER)
      equivalence (LEST(24),LITER)
      equivalence (LEST(19),ITHSL)
C
C---- SENGE       as of 1989 Dec 18
      integer     NNDCK
      common      /SENGE/ NNDCK
C     Unique "serial number" for GORSE checksums.
C     .
C     !DASH
      external CHECKER, HI, BYE
C
C               XNE(N), XNK(N), XND(N,NL), H2N(N), BDI(N,NL)
      dimension XNE(*), XNK(*), XND(*),    H2N(*), BDI(*)
C     !EJECT
C
      call HI ('SOUR')
C     !BEG
      NNDCK = NNDCK+1
      write (TIT,100) IOVER,LITER,ITHSL,NNDCK
  100 format(4X,': Number Densities,',4I4)
C
      NNL = N*NL
C
      TIT(1:4) = '  NK'
      call CHECKER   (XNK , 1, N  , TIT)
C
      TIT(1:4) = '  ND'
      call CHECKER   (XND , 1, NNL, TIT)
C
      TIT(1:4) = ' BDI'
      call CHECKER   (BDI , 1, NNL, TIT)
C
      TIT(1:4) = '  NE'
      call CHECKER   (XNE , 1, N  , TIT)
C
      if(DONE) then
        TIT(1:4) = ' H2N'
        call CHECKER (H2N , 1, N  , TIT)
      end if
C     !END
      call BYE ('SOUR')
C
      return
      end
