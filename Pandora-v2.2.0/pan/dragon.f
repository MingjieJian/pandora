      subroutine DRAGON
     $(NO,N,NL,XND,XNK,BDI)
C
C     Rudolf Loeser, 1971 Dec 30
C---- Prints values of number density and departure coefficient.
C     !DASH
      save
C     !DASH
      real*8 BDI, ONE, XND, XNK
      integer N, NL, NNL, NO
      logical JNB, JND, JNK, PRNTZ
      character BLANK*1, STARS*20
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external LINER, NAUGHTD, OMAR, KONSTD, HI, BYE
C
C               XNK(N), XND(N,NL), BDI(N,NL)
      dimension XNK(*), XND(*),    BDI(*)
C
      data STARS /'********************'/
      data PRNTZ /.true./
C     !EJECT
C
      call HI ('DRAGON')
C     !BEG
      if(NO.gt.0) then
        NNL = N*NL
C
        call LINER   (2,NO)
        write (NO,100) STARS
  100   format(' ',A20,' Values of Level Number Density')
        call NAUGHTD (XND,1,NNL,JND)
        if(JND) then
          call LINER (1,NO)
          write (NO,101)
  101     format(' ','All zero.')
        else
          call OMAR  (NO,N,NL,XND,'Level ',PRNTZ)
        end if
C
        call LINER   (2,NO)
        write (NO,102) STARS
  102   format(' ',A20,' Values of Ionized Number Density')
        call NAUGHTD (XNK,1,N,JNK)
        if(JNK) then
          call LINER (1,NO)
          write (NO,101)
        else
          call OMAR  (NO,N,1,XNK,BLANK,PRNTZ)
        end if
C
        call LINER   (2,NO)
        write (NO,103) STARS
  103   format(' ',A20,' Values of Departure Coefficient')
        call KONSTD  (BDI,1,NNL,ONE,JNB)
        if(JNB) then
          call LINER (1,NO)
          write (NO,104)
  104     format(' ','All one.')
        else
          call OMAR  (NO,N,NL,BDI,'Level ',PRNTZ)
        end if
      end if
C     !END
      call BYE ('DRAGON')
C
      return
      end
