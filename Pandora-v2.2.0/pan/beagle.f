      subroutine BEAGLE
     $(NO)
C
C     Rudolf Loeser, 2000 Mar 02
C---- Writes a heading, for BUGLE.
C     (This is version 2 of BEAGLE.)
C     !DASH
      save
C     !DASH
      integer I, NO
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
      external LINER, HI, BYE
C
      call HI ('BEAGLE')
C     !BEG
      if(NO.gt.0) then
        write (NO,100)
  100   format(' ','Details of the calculation of the Total ',
     $             'Recombination Term, TQS.',23X,
     $             '(Printout controlled by option RCOMPRNT)'//
     $         ' ','(This analysis of the Recombination Term ',
     $             'is printed for reference only.)')
        call LINER (2,NO)
        write (NO,101)
  101   format(' ',' TE  temperature'//
     $         ' ','TQS  total recombination term = essentially SQS ',
     $             'plus other terms. TQS =  X + (DRC/SA) + SQC'//
     $         ' ','SQS  sum of QS, minus DRC/SA ',
     $             '(see preceding RATES printout)'/
     $         ' ','SQC  collisional part of SQS'/
     $         ' ','SQR  radiative part of SQS'//
     $         ' ','RRC  computed radiative recombination coefficient',
     $             ' = SQR*SA, to be compared with CRR')
        call LINER (1,NO)
C     !EJECT
        write (NO,102) APARAD, APETA
  102   format(' ','CRR  radiative recombination coefficient ',
     $             '(Aldrovandi & Pequignot, 1973), ',
     $             'to be compared with RRC'/
     $         ' ','     input parameters for CRR: APARAD =',1PE12.4,
     $             ', APETA =',E12.4)
        call LINER (1,NO)
        write (NO,103)
  103   format(' ','  X  radiative recombination rate = ',
     $             '(1 - WRR)*SQR + WRR*(CRR/SA)')
        call LINER (1,NO)
        write (NO,104) NAPWRA, APWRA, NAPWRB, APWRB
  104   format(' ','WRR  weight used to compute X; WRR = 0 means ',
     $             'that CRR is not used'/
     $         ' ','     input parameters for WRR: NAPWRA =',I3,
     $             ', APWRA =',1PE12.4,', NAPWRB =',I3,
     $             ', APWRB =',E12.4)
        call LINER (1,NO)
        write (NO,105)
  105   format(' ','DRC  dielectronic recombination coefficient ',
     $             '(same units as CRR) = H*DEP')
        call LINER (1,NO)
        write (NO,106) APCI(1), APEI(1)
  106   format(' ','  H  dielectronic recombination rate ',
     $             '(Romanik, 1997)'/
     $         ' ','     input parameters for H: APCI =',1PE12.4,
     $             ', APEI =',E12.4)
        if(NAPKNT.gt.1) then
          write (NO,107) (APCI(I),APEI(I),I=2,NAPKNT)
  107     format(' ',35X,1PE12.4,8X,E12.4)
        end if
        call LINER (1,NO)
        write (NO,108) APCDP
  108   format(' ','DEP  density-dependent factor for DRC = ',
     $             'FNH/(1+APCDP*(NE**0.2)), where FNH = 1 for ',
     $             'NH < 10**12 and = 0 for NH > 10**14'/
     $         ' ','     input parameter for DEP: APCDP =',1PE12.4)
        call LINER (1,NO)
        write (NO,109)
  109   format(' ',' SA  Saha-Boltzmann term')
        call LINER (3,NO)
      end if
C     !END
      call BYE ('BEAGLE')
C
      return
      end
