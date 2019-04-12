      subroutine OTTO
     $(HEADER,CRIT,KODE,MODE)
C
C     Rudolf Loeser, 2002 Nov 05
C---- Encodes a heading, for UTTA.
C     (See also TOOT.)
C     !DASH
      save
C     !DASH
      real*8 CRIT
      integer KODE, MODE
      character HEADER*127, REL*4
C     !DASH
      external HI, BYE
C
      dimension REL(5)
C
      data REL /'.LT.', '.LE.', '.EQ.', '.GE.', '.GT.'/
C
      call HI ('OTTO')
C     !BEG
      if(MODE.eq.1) then
        write (HEADER,100) REL(KODE),CRIT
  100   format('*******************  EDIT1 - standard simple editing: ',
     $         '"bad" = ',A4,1PE10.2,'; linear interpolation is used.',
     $         '  ******************')
      else
        write (HEADER,101) REL(KODE),CRIT
  101   format('**************  EDIT1 - standard simple editing: "bad"',
     $         ' = ',A4,1PE10.2,'; logarithmic interpolation is used.',
     $         '  ******************')
      end if
C     !END
      call BYE ('OTTO')
C
      return
      end
