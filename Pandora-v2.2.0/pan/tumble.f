      subroutine TUMBLE
     $(LFB,FACELAB)
C
C     Rudolf Loeser, 1987 Jan 26
C---- Encodes a viewing-position label.
C     (This is version 2 of TUMBLE.)
C     !DASH
      save
C     !DASH
      integer LFB, LFBV
      character BLANK*1, FACELAB*10
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external ZORA, HI, BYE
C
      call HI ('TUMBLE')
C     !BEG
      FACELAB = BLANK
C
      call ZORA (LFBV)
C
      if(LFBV.eq.2) then
        if(LFB.eq.1) then
          FACELAB = 'Front face'
        else if(LFB.eq.2) then
          FACELAB = ' Back face'
        else
          FACELAB = '    Utopia'
        end if
      end if
C     !END
      call BYE ('TUMBLE')
C
      return
      end
