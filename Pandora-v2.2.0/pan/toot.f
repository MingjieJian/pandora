      subroutine TOOT
     $(HEADER,CRIT,KODE)
C
C     Rudolf Loeser, 2002 Nov 06
C---- Encodes a heading, for ATTU.
C     (See also OTTO.)
C     !DASH
      save
C     !DASH
      real*8 CRIT
      integer KODE
      character HEADER*127, REL*4
C     !DASH
      dimension REL(5)
C
      data REL /'.LT.', '.LE.', '.EQ.', '.GE.', '.GT.'/
C
      call HI ('TOOT')
C     !BEG
      write (HEADER,100) REL(KODE),CRIT
  100 format('*******************************  ',
     $       'EDIT2 - standard replacement editing: "bad" = ',A4,
     $       1PE10.2,'  ********************************')
C     !END
      call BYE ('TOOT')
C
      return
      end
