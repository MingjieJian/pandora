      subroutine ANDREW
     $(IMAGE,XLL,XRL,YLL,YUL)
C
C     Rudolf Loeser, 1982 Apr 12
C---- Sets up a common graph format.
C     !DASH
      save
C     !DASH
      real*8 XLL, XRL, YLL, YUL
      integer NH, NV
      logical GOOD
      character IMAGE*(*), NUMERO*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(38),NUMERO)
C
C---- BASH        as of 1984 Apr 19
      integer            JXLOBA,JXHIBA,JYLOBA,JYHIBA,NTHRBA,NPOIBA
      common      /BASH/ JXLOBA,JXHIBA,JYLOBA,JYHIBA,NTHRBA,NPOIBA
C     Control data for "BEIGE".
C     .
C     !DASH
      external KINIT, KRIGIA, HI, BYE
C
      data NH,NV /117, 51/
C
      call HI ('ANDREW')
C     !BEG
      JXLOBA = 1
      JXHIBA = NH
      JYLOBA = 1
      JYHIBA = NV
      NTHRBA = 20
      NPOIBA = 0
      call KINIT    (IMAGE,XLL,XRL,YLL,YUL,JYHIBA,JXHIBA,NUMERO,GOOD)
C
      if(.not.GOOD) then
        call KRIGIA (XLL,XRL,YLL,YUL,JYHIBA,JXHIBA)
      end if
C     !END
      call BYE ('ANDREW')
C
      return
      end
