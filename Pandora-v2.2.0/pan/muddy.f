      subroutine MUDDY
     $(M,MR,JLEV,WAV,RNU,RCP,YRATE,LINE)
C
C     Rudolf Loeser, 1980 Mar 04
C---- Encoding routine, for STONY.
C     !DASH
      save
C     !DASH
      real*8 RCP, RNU, WAV, YRATE
      integer JLEV, M, MR
      character BLANK*1, LINE*60, TIT*10
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external HAKO, HI, BYE
C
      call HI ('MUDDY')
C     !BEG
      LINE = BLANK
      if(M.le.MR) then
        call HAKO (YRATE,TIT)
        write (LINE(16:60),100) WAV,RNU,RCP,TIT(2:10)
  100   format(1PE15.7,0PF9.4,1PE11.3,1X,A9)
      end if
C     !END
      call BYE ('MUDDY')
C
      return
      end
