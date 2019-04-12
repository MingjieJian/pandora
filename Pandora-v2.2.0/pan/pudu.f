      subroutine PUDU
     $(QNAME,KIND)
C
C     Rudolf Loeser, 2000 Mar 02
C---- Reads recombination data.
C     (This is version 2 of PUDU.)
C     !DASH
      save
C     !DASH
      real*8 dummy
      integer KIND, jummy
      character QNAME*8, qummy*8
C     !COM
C---- DICOM       as of 2000 Mar 01
      integer     NAPWRA,NAPWRB,NAPKNT
      real*8      APARAD,APETA,APCDP,APWRA,APWRB,APCI,APEI
      dimension   APCI(20), APEI(20)
      common      /DICOM1/ NAPWRA,NAPWRB,NAPKNT
      common      /DICOM2/ APARAD,APETA,APCDP,APWRA,APWRB,APCI,APEI
C     Data for recombinations.
C     .
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external MUSTARD, BASIL, HALT, HI, BYE
C
      call HI ('PUDU')
C     !BEG
      goto (
C
C       NAPWRA   NAPWRB   NAPKNT   APARAD   APETA    APCDP    APWRA
     $  101,     102,     103,     104,     105,     106,     107,
C
C       APWRB    APCI     APEI
     $  108,     109,     110
C
     $  ), KIND
C     !EJECT
  101 continue
        call MUSTARD (QNAME,dummy,NAPWRA,qummy,1,3)
        goto 400
  102 continue
        call MUSTARD (QNAME,dummy,NAPWRB,qummy,1,3)
        goto 400
  103 continue
        call MUSTARD (QNAME,dummy,NAPKNT,qummy,1,3)
        if(NAPKNT.gt.20) then
          write (MSSLIN(1),1031) NAPKNT
 1031     format('NAPKNT =',I10,', must be less than 21.')
          call HALT  ('PUDU',1)
        end if
        goto 400
  104 continue
        call MUSTARD (QNAME,APARAD,jummy,qummy,1,5)
        goto 400
  105 continue
        call MUSTARD (QNAME,APETA ,jummy,qummy,1,5)
        goto 400
  106 continue
        call MUSTARD (QNAME,APCDP ,jummy,qummy,1,5)
        goto 400
  107 continue
        call MUSTARD (QNAME,APWRA ,jummy,qummy,1,5)
        goto 400
  108 continue
        call MUSTARD (QNAME,APWRB ,jummy,qummy,1,5)
        goto 400
  109 continue
        call BASIL   (APCI,NAPKNT,QNAME)
        goto 400
  110 continue
        call BASIL   (APEI,NAPKNT,QNAME)
        goto 400
  400 continue
C     !END
      call BYE ('PUDU')
C
      return
      end
