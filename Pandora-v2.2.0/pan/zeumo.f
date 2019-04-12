      subroutine ZEUMO
     $(N,NSL,RKI,IQRK,CQIN,PKS,NO)
C
C     Rudolf Loeser, 2004 Jan 12
C---- Prints, for POOF.
C     (This is version 2 of ZEUMO.)
C     !DASH
      save
C     !DASH
      real*8 CQIN, PKS, RKI
      integer IQRK, N, NO, NSL
      logical PRNTZ, QZ
      character BLANK*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external ABJECT, NAUGHTD, LINER, OMAR, LEI, HI, BYE
C
C               RKI(N,NSL), IQRK(NSL), CQIN(N), PKS(N)
      dimension RKI(*),     IQRK(*),   CQIN(*), PKS(*)
C
      data PRNTZ /.false./
C
      call HI ('ZEUMO')
C     !BEG
      if(NO.gt.0) then
        call ABJECT  (NO)
        write (NO,100)
  100   format(' ','K-Shell photoionization')
        call NAUGHTD (CQIN, 1, N, QZ)
        if(.not.QZ) then
          call LINER (3, NO)
          write (NO,101)
  101     format(' ','QIN - additional photoionization')
          call OMAR  (NO, N, 1, CQIN, BLANK, PRNTZ)
        end if
        call LINER   (3, NO)
        write( NO,102)
  102   format(' ','P - K-Shell photoionization rate')
        call OMAR    (NO, N, 1, PKS, BLANK, PRNTZ)
        call LINER   (3, NO)
        write (NO,103)
  103   format(' ','RK - Photoionization rate, modified for K-Shell ',
     $             'ionizations')
        call OMAR    (NO, N, NSL, RKI, 'Level ', PRNTZ)
        call LEI     (IQRK, NSL, 'RK', NO)
      end if
C     !END
      call BYE ('ZEUMO')
C
      return
      end
