      subroutine THYMON
     $(A,N,QNAME,LZA,ZAUX,Z,W)
C
C     Rudolf Loeser, 1990 Nov 21
C---- Constructs a call to CREAM, for charge exchange input tables.
C     !DASH
      save
C     !DASH
      real*8 A, W, Z, ZAUX
      integer J, K, LZA, N
      character QNAME*8
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
C     !DASH
      external MINT, CREAM, HI, BYE
C
      dimension W(*)
C
C               A(N,NPQLM,NXI), LZA(NZA), ZAUX(NZM,LZM), Z(N)
      dimension A(N,NPQLM,*),   LZA(*),   ZAUX(*),       Z(*)
C
C
      call HI ('THYMON')
C     !BEG
      call MINT  (QNAME,J)
      call MINT  (QNAME,K)
      call CREAM (A(1,J,K),QNAME,J,K,LZA,ZAUX,Z,W)
C     !END
      call BYE ('THYMON')
C
      return
      end
