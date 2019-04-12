      subroutine ZEEL
     $(IMAGE,XL,XR,YL,YU,NLIM)
C
C     Rudolf Loeser, 1982 Apr 22
C---- Initializes the plot image, for HALYS.
C     !DASH
      save
C     !DASH
      real*8 FIVE, ONE, X, XL, XNINE, XR, Y, YL, YU, ZERO
      integer I, L, NH, NLIM, NV
      logical GOOD
      character IMAGE*(*), MINUS*1, NUMERO*1, PLUS*1, SYM*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(39),PLUS  )
      equivalence (SYMBS(40),MINUS )
      equivalence (SYMBS(38),NUMERO)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 6),FIVE  )
      equivalence (DLIT(10),XNINE )
C     !DASH
      external KINIT, KRIGIA, KLINEC, KPLOTC, HI, BYE
C
      data NV,NH /55, 117/
C     !EJECT
C
      call HI ('ZEEL')
C     !BEG
      call KINIT    (IMAGE,XL,XR,YL,YU,NV,NH,NUMERO,GOOD)
      if(.not.GOOD) then
        call KRIGIA (XL,XR,YL,YU,NV,NH)
      end if
C
      Y = ONE
      do 100 I = 1,5
C
        SYM = MINUS
        if(I.eq.3) then
          SYM = NUMERO
        end if
C
        Y = Y+XNINE
        call KLINEC (IMAGE,XL,Y,XR,Y,SYM,0)
  100 continue
C
      X = ZERO
      L = NLIM/5
      SYM = PLUS
      do 101 I = 1,L
        X = X+FIVE
        call KPLOTC (IMAGE,X,YL,SYM)
        call KPLOTC (IMAGE,X,YU,SYM)
  101 continue
C     !END
      call BYE ('ZEEL')
C
      return
      end
