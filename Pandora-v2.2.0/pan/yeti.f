      subroutine YETI
     $(N,NSL,YK,RLI,IQRL,NO)
C
C     Rudolf Loeser, 1978 Jan 26
C---- Provides for additional recombinations.
C     !DASH
      save
C     !DASH
      real*8 RLI, YK, ZERO
      integer IQRL, J, N, NO, NSL
      logical PRNTZ
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external YAK, ABJECT, OMAR, LEI, HI, BYE
C
C               YK(NSL), RLI(N,NSL), IQRL(NSL)
      dimension YK(*),   RLI(N,*),   IQRL(*)
C
      data PRNTZ /.false./
C
      call HI ('YETI')
C     !BEG
      do 100 J = 1,NSL
        if((IQRL(J).gt.0).and.(YK(J).ne.ZERO)) then
          call YAK  (N,YK(J),RLI(1,J))
        end if
  100 continue
C
      if(NO.gt.0) then
        call ABJECT (NO)
        write (NO,101)
  101   format(' ','RL - Photorecombination rates, modified for ',
     $             'additional recombinations from higher levels')
        call OMAR   (NO,N,NSL,RLI,'Level ',PRNTZ)
        call LEI    (IQRL,NSL,'RL',NO)
      end if
C     !END
      call BYE ('YETI')
C
      return
      end
