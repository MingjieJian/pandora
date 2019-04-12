      subroutine HADDOCK
     $(N,IMAGEL)
C
C     Rudolf Loeser, 1980 Dec 11
C---- Sets up Spectrum Summary Plot index labels, for FOUND.
C     !DASH
      save
C     !DASH
      real*8 TENTH, TWO, X, YQ, ZERO
      integer I, IS, N, NH, NV
      logical GOOD
      character BLANK*1, IMAGEL*(*), STAR*1, STRING*3
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(19),TENTH )
      equivalence (DLIT( 3),TWO   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(45),STAR  )
C     !DASH
      external  KINIT, KRIGIA, KPLOTC, KANOTC, HI, BYE
      intrinsic mod
C
      data YQ /2.1D0/
      data NV,NH /2, 117/
C     !EJECT
C
      call HI ('HADDOCK')
C     !BEG
      X = N+1
      call KINIT      (IMAGEL,ZERO,X,ZERO,TWO,NV,NH,BLANK,GOOD)
      if(.not.GOOD) then
        call KRIGIA   (ZERO,X,ZERO,TWO,NV,NH)
      end if
C
      do 102 I = 1,N
        X = I
        call KPLOTC   (IMAGEL,X,YQ,STAR)
        if(mod(I,5).eq.0) then
C
          if(I.le.5) then
            STRING = '5'
            IS = 1
          else if(I.lt.100) then
            write (STRING,100) I
  100       format(I2)
            IS = 2
          else
            write (STRING,101) I
  101       format(I3)
            IS = 3
          end if
          call KANOTC (IMAGEL,X,TENTH,STRING,IS,1)
C
        end if
  102 continue
C     !END
      call BYE ('HADDOCK')
C
      return
      end
