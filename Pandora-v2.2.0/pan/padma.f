      subroutine PADMA
     $(NO,LABEL)
C
C     Rudolf Loeser, 1990 Jul 18
C---- Inserts printout markers centered between dashes.
C     !DASH
      save
C     !DASH
      integer J, K, NO
      character DASHES*60, LABEL*(*), LINE*127
C     !DASH
      external  ABJECT, LINER, HI, BYE
      intrinsic len, min
C
      data DASHES /
     $'------------------------------------------------------------'/
C
      call HI ('PADMA')
C     !BEG
      if(NO.gt.0) then
        J = len(LABEL)
        J = min(J,121)
        K = (117-J)/2+1
C
        LINE = DASHES(:K)//'     '//LABEL//'     '//DASHES(:K)
C
        call ABJECT (NO)
        call LINER  (2,NO)
C
        write (NO,100) LINE
  100   format(' ',A127)
C
        call LINER  (1,NO)
      end if
C     !END
      call BYE ('PADMA')
C
      return
      end
