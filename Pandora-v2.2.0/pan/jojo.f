      subroutine JOJO
     $(QNAME)
C
C     Rudolf Loeser, 1999 Sep 22
C---- Reads a value of the charge exchange parameter RCHX.
C     (This is version 5 of JOJO.)
C     !DASH
      save
C     !DASH
      integer I, J, jummy
      character QNAME*8, qummy*8
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
      external MINT, MUSTARD, HI, BYE
C
      call HI ('JOJO')
C     !BEG
      call MINT    (QNAME,I)
      call MINT    (QNAME,J)
      call MUSTARD (QNAME,RCHX(I,J),jummy,qummy,1,5)
C     !END
      call BYE ('JOJO')
C
      return
      end
