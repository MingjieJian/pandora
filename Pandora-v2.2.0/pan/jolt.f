      subroutine JOLT
     $(IN,LIN)
C
C     Rudolf Loeser, 1980 Feb 15
C---- Encodes "IN", for BENJAMN.
C     (This is version 2 of JOLT.)
C     !DASH
      save
C     !DASH
      integer IN
      character LIN*3
C     !DASH
      external ACHILES, HI, BYE
C
      call HI ('JOLT')
C     !BEG
      if((IN.gt.0).and.(IN.lt.100)) then
        call ACHILES (IN,LIN)
C
      else if(IN.eq.0) then
        LIN = '   '
      else if(IN.eq.100) then
        LIN = '  *'
      else if(IN.lt.0) then
        LIN = '  -'
      else
        LIN = '  +'
      end if
C     !END
      call BYE ('JOLT')
C
      return
      end
