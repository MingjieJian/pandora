      subroutine DUNE
     $(NL,IB,IE,QAR,LINE,NO)
C
C     Rudolf Loeser, 1980 Dec 27
C---- Writes Level Numbers.
C     (This is version 2 of DUNE.)
C     !DASH
      save
C     !DASH
      integer I, IB, IE, KNT, NL, NO
      character DOTS*10, LINE*120, QAR*10
C     !DASH
      external FRUG, HI, BYE
C
C               QAR(16)
      dimension QAR(*)
C
      data DOTS /'..........'/
C
      call HI ('DUNE')
C     !BEG
      KNT = 1
      QAR(KNT) = DOTS
C
      do 102 I = IB,IE
        KNT = KNT+1
        if(I.le.NL) then
          write (QAR(KNT),100) I
  100     format(8X,I2)
        else
          write (QAR(KNT),101) I
  101     format(6X,'(',I2,')')
        end if
  102 continue
C
      call FRUG ('Levels',QAR,KNT,LINE,NO)
C     !END
      call BYE ('DUNE')
C
      return
      end
