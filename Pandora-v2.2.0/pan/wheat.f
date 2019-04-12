      subroutine WHEAT
     $(NO,N,NSL,CQUI,CQSI,SQS,DRCT)
C
C     Rudolf Loeser, 1978 May 03
C---- Prints, for QUETZAL.
C     !DASH
      save
C     !DASH
      real*8 CQSI, CQUI, DRCT, SQS
      integer N, NO, NSL
      logical PRNTZ
      character LAB*80
C     !DASH
      external ABJECT, LINER, OMAR, VECOUT, HI, BYE
C
C               CQUI(N,NSL), CQSI(N,NSL), SQS(N), DRCT(N)
      dimension CQUI(*),     CQSI(*),     SQS(*), DRCT(*)
C
      data LAB   /'DRCT - dielectronic recombination term, DRC/SA (has b
     $een added to QS[level 1])'/
C
      data PRNTZ /.false./
C
      call HI ('WHEAT')
C     !BEG
      if(NO.gt.0) then
        call ABJECT (NO)
        write (NO,100)
  100   format(' ','QU   - ionization term')
        call OMAR   (NO, N, NSL, CQUI, 'Level ', PRNTZ)
C
        call LINER  (2, NO)
        write (NO,101)
  101   format(' ','QS   - ionization term')
        call OMAR   (NO, N, NSL, CQSI, 'Level ', PRNTZ)
C
        call VECOUT (NO, DRCT, N, LAB                           )
        call VECOUT (NO, SQS,  N, 'SQS  - sum over levels of QS')
      end if
C     !END
      call BYE ('WHEAT')
C
      return
      end
