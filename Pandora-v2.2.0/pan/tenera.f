      subroutine TENERA
     $(TEI,H)
C
C     Rudolf Loeser, 2000 Mar 02
C---- Computes dielectronic recombination a la Romanik.
C     (This is version 2 of TENERA.)
C     !DASH
      save
C     !DASH
      real*8 CON, EX, FAC, H, RT, SUM, TEI, ZERO
      integer I
C     !COM
C---- DICOM       as of 2000 Mar 01
      integer     NAPWRA,NAPWRB,NAPKNT
      real*8      APARAD,APETA,APCDP,APWRA,APWRB,APCI,APEI
      dimension   APCI(20), APEI(20)
      common      /DICOM1/ NAPWRA,NAPWRB,NAPKNT
      common      /DICOM2/ APARAD,APETA,APCDP,APWRA,APWRB,APCI,APEI
C     Data for recombinations.
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external HI, BYE
C
      data CON /1.159D4/
C
      call HI ('TENERA')
C     !BEG
      SUM = ZERO
C
      if(NAPKNT.gt.0) then
        FAC = CON/TEI
        do 100 I = 1,NAPKNT
          EX  = exp(-FAC*APEI(I))
          SUM = SUM+APCI(I)*EX
  100   continue
      end if
C
      RT = sqrt(TEI)
      H  = SUM/(RT**3)
C     !END
      call BYE ('TENERA')
C
      return
      end
