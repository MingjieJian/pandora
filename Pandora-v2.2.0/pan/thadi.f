      subroutine THADI
     $(NO,IMAGE,N)
C     Rudolf Loeser, 1999 Jan 21
C---- Prints the standard back-to-back log plots.
C     !DASH
      save
C     !DASH
      integer J, K, LL, LN, N, NO
      character IMAGE*(*), LBL*4, LINE*117
C     !DASH
      external KGIVE, HI, BYE
C
      dimension LBL(7)
C
      data LBL /'   2', '1.10', '1.01', '   1',
     $                  '0.99', '0.90', '   0'/
C
      call HI ('THADI')
C     !BEG
      if(NO.gt.0) then
        call KGIVE     (IMAGE,1,LINE)
        write (NO,100) LBL(1),LINE
  100   format(' ',A4,1X,A117)
        LN = 1
        LL = 1
        do 102 J = 1,6
          do 101 K = 1,8
            LN = LN+1
            call KGIVE (IMAGE,LN,LINE)
            write (NO,100) '    ',LINE
  101     continue
          LN = LN+1
          LL = LL+1
          call KGIVE   (IMAGE,LN,LINE)
          write (NO,100) LBL(LL),LINE
  102   continue
        write (NO,103) N
  103   format(' ',5X,'1',113X,I3)
      end if
C     !END
      call BYE ('THADI')
C
      return
      end
