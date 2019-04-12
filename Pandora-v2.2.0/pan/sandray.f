      subroutine SANDRAY
     $(IMAGE,XL,XR,BOT,TOP,NV,NH)
C
C     Rudolf Loeser, 1996 Mar 06
C---- Initializes the plot, for SKYE.
C     !DASH
      save
C     !DASH
      real*8 BOT, ONE, TEN, TOP, XL, XR
      integer NH, NV
      logical OK
      character IMAGE*(*), NUMERO*1, PERIOD*1
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT(11),TEN   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(38),NUMERO)
      equivalence (SYMBS(42),PERIOD)
C     !DASH
      external KINIT, KRIGIA, MONKEY, HI, BYE
C
      call HI ('SANDRAY')
C     !BEG
      call KINIT    (IMAGE,XL,XR,BOT,TOP,NV,NH,NUMERO,OK)
      if(.not.OK) then
        call KRIGIA (XL,XR,BOT,TOP,NV,NH)
      end if
      call MONKEY   (IMAGE,XL, XR, TEN,BOT,TOP,PERIOD,1)
      call MONKEY   (IMAGE,BOT,TOP,ONE,XL, XR, PERIOD,2)
C     !END
      call BYE ('SANDRAY')
C
      return
      end
