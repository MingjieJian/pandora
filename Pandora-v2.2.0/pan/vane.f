      subroutine VANE
     $(DMP,NO,K,LINES,N,NL,NCK,NT)
C
C     Rudolf Loeser, 2003 Apr 24
C---- Prints a dump, for JEWEL.
C     !DASH
      save
C     !DASH
      integer K, M, N, NCK, NL, NO, NT
      logical DMP
      character LINES*88
C     !DASH
      external ABJECT, LINER, HI, BYE
C
C               LINES(MXIBIS)
      dimension LINES(*)
C
      call HI ('VANE')
C     !BEG
      if(DMP.and.(NO.gt.0)) then
C
        call ABJECT (NO)
        write (NO,100) N,NL,NCK,NT
  100   format(' ','Summary of Iterative Summary data, ',
     $             'in the order collected.'//
     $         ' ','N =',I5,', NL =',I3,', NCK =',I3,', NT =',I5//
     $         ' ','Source',6X,'Name',11X,'Count',2X,'Length',6X,
     $             'I1',6X,'I2',4X,'ITER',3X,'LITER',3X,'ITHSL',3X,
     $             'IOVER')
        call LINER  (1, NO)
C
        write (NO,101) (LINES(M),M=1,K)
  101   format(5(' ',A88/))
C
      end if
C     !END
      call BYE ('VANE')
C
      return
      end
