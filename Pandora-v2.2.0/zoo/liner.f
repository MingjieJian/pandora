      subroutine LINER
     $(KOUNT,NO)
C     Rudolf Loeser, 1980 Aug 22
C---- Writes "KOUNT" blank lines in the file attached to unit "NO".
C     !DASH
      save
C     !DASH
      integer I, KOUNT, NO
C
C     !BEG
      if((NO.gt.0).and.(KOUNT.gt.0)) then
        do 101 I = 1,KOUNT
          write (NO,100)
  100     format(' ')
  101   continue
      end if
C     !END
C
      return
      end
