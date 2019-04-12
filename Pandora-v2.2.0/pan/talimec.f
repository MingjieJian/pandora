      subroutine TALIMEC
     $(IMAGE,XL,XR,GRID,NG,NV,NH,TIT)
C
C     Rudolf Loeser, 1998 Aug 24
C---- Initializes plot IMAGE, for METALIC.
C     !DASH
      save
C     !DASH
      real*8 BOT, GRID, TEN, TOP, VAL, XL, XR
      integer I, NG, NH, NV
      logical GOOD
      character IMAGE*(*), NUMERO*1, PERIOD*1, TIT*10
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
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
C     !EJECT
      external KINIT, KRIGIA, MONKEY, KLINEC, HI, BYE
C
C               GRID(NG)
      dimension GRID(*)
C
      call HI ('TALIMEC')
C     !BEG
C---- Axis limits
      TOP = log10(GRID( 1))
      BOT = log10(GRID(NG))
C---- Initialze plot image
      call KINIT    (IMAGE, XL, XR, BOT, TOP, NV, NH, NUMERO, GOOD)
      if(.not.GOOD) then
C       Abort with message
        call KRIGIA (XL, XR, BOT, TOP, NV, NH)
      end if
      if(TIT.eq.'Z-index   ') then
C----   Vertical grid lines
        call MONKEY (IMAGE, XL, XR, TEN, BOT, TOP, PERIOD, 1)
      end if
C---- Horizontal grid lines
      do 100 I = 2,(NG-1)
        VAL = log10(GRID(I))
        call KLINEC (IMAGE, XL, VAL, XR, VAL, PERIOD, 0)
  100 continue
C     !END
      call BYE ('TALIMEC')
C
      return
      end
