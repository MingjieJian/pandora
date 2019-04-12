      subroutine AGARIC
     $(TEI,CRR)
C
C     Rudolf Loeser, 1978 May 04
C---- Computes CRR, for RAGOUT.
C     !DASH
      save
C     !DASH
      real*8 CRR, R, SCALE, TEI, Z
C     !COM
C---- DICOM       as of 2000 Mar 01
      integer     NAPWRA,NAPWRB,NAPKNT
      real*8      APARAD,APETA,APCDP,APWRA,APWRB,APCI,APEI
      dimension   APCI(20), APEI(20)
      common      /DICOM1/ NAPWRA,NAPWRB,NAPKNT
      common      /DICOM2/ APARAD,APETA,APCDP,APWRA,APWRB,APCI,APEI
C     Data for recombinations.
C     .
C     !DASH
      external HI, BYE
C
      data SCALE /1.D-4/
C
      call HI ('AGARIC')
C     !BEG
      R = TEI*SCALE
      Z = R**APETA
C
      CRR = APARAD/Z
C     !END
      call BYE ('AGARIC')
C
      return
      end
