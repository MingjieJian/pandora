      subroutine RETHO
     $(J,XN,XNS,R,B)
C
C     Rudolf Loeser, 1983 May 03
C---- Makes labels, for CYMBAL.
C     !DASH
      save
C     !DASH
      integer J
      character B*7, BLANK*1, R*7, STAR*1, XN*7, XNS*7
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(45),STAR  )
C     !DASH
      external HI, BYE
C
      call HI ('RETHO')
C     !BEG
  100 format(A1,'(',I2,')',A1,' ')
C
      write (XN ,100) ALPHS(14),J,BLANK
      write (XNS,100) ALPHS(14),J,STAR
      write (R  ,100) ALPHS(18),J,BLANK
      write (B  ,100) ALPHS( 2),J,BLANK
C     !END
      call BYE ('RETHO')
C
      return
      end
