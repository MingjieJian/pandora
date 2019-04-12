      subroutine ISUA
     $(IMAGE,N)
C
C     Rudolf Loeser, 1991 Jun 07
C---- Sets up the plot image for IDATH.
C     !DASH
      save
C     !DASH
      real*8 ONE, SIX, X, XMAX, XMIN, Y, YMAX, YMIN, ZERO
      integer I, N
      logical OK
      character IMAGE*(*), NUMERO*1, PERIOD*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(38),NUMERO)
      equivalence (SYMBS(42),PERIOD)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 7),SIX   )
C     !DASH
      external KINIT, KLINEC, HI, BYE
C     !EJECT
C
      call HI ('ISUA')
C     !BEG
      XMIN = ONE
      XMAX = N
      YMIN = ZERO
      YMAX = SIX
      call KINIT    (IMAGE,XMIN,XMAX,YMIN,YMAX, 55,117, NUMERO, OK)
C
      do 100 I = 10,N,10
        X = I
        call KLINEC (IMAGE, X,YMIN, X,YMAX, PERIOD,0)
  100 continue
C
      do 101 I = 1,5
        Y = I
        call KLINEC (IMAGE, XMIN,Y, XMAX,Y, PERIOD,0)
  101 continue
C     !END
      call BYE ('ISUA')
C
      return
      end
