      subroutine ESSEX
     $(ELSYM,ELE,ION)
C
C     Rudolf Loeser, 1982 Jun 21
C---- Sets up ELE and ION, based on ELSYM.
C     !DASH
      save
C     !DASH
      integer ION, K
      character BLANK*1, C2*1, C3*1, ELE*2, ELSYM*3, NUMERAL*9
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  HALT, HI, BYE
      intrinsic index
C
      data NUMERAL /'123456789'/
C     !EJECT
C
      call HI ('ESSEX')
C     !BEG
      C2 = ELSYM(2:2)
      C3 = ELSYM(3:3)
      if(C2.eq.BLANK) then
        ION = 1
        ELE = ELSYM(1:1)//BLANK
      else
        K = index(NUMERAL,C2)
        if(K.ne.0) then
          ION = K
          ELE = ELSYM(1:1)//BLANK
        else
          if(C3.eq.BLANK) then
            ION = 1
            ELE = ELSYM(1:2)
          else
            K = index(NUMERAL,C3)
            if(K.ne.0) then
              ION = K
              ELE = ELSYM(1:2)
            else
              write (MSSLIN(1),100) ELSYM
  100         format('ELSYM = [',A,'], which is an improper form.')
              call HALT ('ESSEX',1)
            end if
          end if
        end if
      end if
C     !END
      call BYE ('ESSEX')
C
      return
      end
