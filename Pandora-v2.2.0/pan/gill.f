      subroutine GILL
     $(I,WRR)
C
C     Rudolf Loeser, 1978 May 04
C---- Computes WRR, for RAGOUT
C     !DASH
      save
C     !DASH
      real*8 D, E1, E2, WRR
      integer I, NIA, NIB
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
      external DIVIDE, HI, BYE
C
      call HI ('GILL')
C     !BEG
      NIA = I-NAPWRA
      if(NIA.le.0) then
        WRR = APWRA
      else
        NIB = NAPWRB-I
        if(NIB.le.0) then
          WRR = APWRB
        else
          E1 = NIB
          E2 = NIA
          D  = NAPWRB-NAPWRA
          call DIVIDE ((APWRA*E1+APWRB*E2),D,WRR)
        end if
      end if
C     !END
      call BYE ('GILL')
C
      return
      end
