      subroutine GRIP
     $(ITER,JLEV,ITAU,TR1,RK1,TR2,RK2,TRFLI,TR,RKN,RK)
C
C     Rudolf Loeser, 1984 Apr 16
C---- Dumps, for LUGGAGE.
C     !DASH
      save
C     !DASH
      real*8 RK, RK1, RK2, RKN, TR, TR1, TR2, TRFLI
      integer ITAU, ITER, JLEV, LUEO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, HI, BYE
C
      call HI ('GRIP')
C     !BEG
      call LINER (2, LUEO)
      write (LUEO,100) JLEV,ITAU,TRFLI
  100 format(' ','Details of iterative calculation of effective ',
     $           'radiation temperature'/
     $       ' ','for level',I4,', at depth',I4,10X,'Interval ',
     $           'limit TRFLI =',1PE12.5//
     $       ' ','ITER',10X,'TR1',10X,'RK1',10X,'TR2',10X,'RK2',10X,
     $           'TRN',10X,'RKN',11X,'RK')
C
      call LINER (1, LUEO)
      write (LUEO,101) ITER,TR1,RK1,TR2,RK2,TR,RKN,RK
  101 format(' ',I4,1P7E13.5)
C     !END
      call BYE ('GRIP')
C
      return
      end
