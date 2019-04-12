      subroutine DIBE
     $(NO,IMAGE,N,LF,QF,LS,QS,LABEL,KS,MD)
C
C     Rudolf Loeser, 1991 Jun 07
C---- Prints plot image and legends, for IDATH.
C     !DASH
      save
C     !DASH
      integer KS, LF, LS, MD, N, NO
      character IMAGE*(*), LABEL*15, QF*1, QS*1
C     !DASH
      external ABJECT, LINER, THADI, HI, BYE
C
      call HI ('DIBE')
C     !BEG
      call ABJECT  (NO)
      write (NO,100) LABEL,LF,QF,LS,QS
  100 format(' ',A,T18,'Plot of Iterative Ratio (relative to ',
     $           'final value), for sets ',I3,'(',A,') - ',I3,
     $           '(',A,'), as a function of depth index.')
      if(KS.eq.1) then
        write (NO,101)
  101   format(' ',T18,'Wherever the Iterative Ratio is negative, ',
     $             'the corresponding lower-case plotting ',
     $             'symbol was used.')
      end if
      if(MD.eq.1) then
        write (NO,102)
  102   format(' ',T18,'(For numeric data instead, turn option ',
     $             'SUMGRAF off.)')
      else
        call LINER (1,NO)
      end if
C
      call THADI   (NO,IMAGE,N)
C     !END
      call BYE ('DIBE')
C
      return
      end
