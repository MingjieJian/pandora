      subroutine BLANKETY
     $(KOUNT)
C
C     Rudolf Loeser, 2001 Aug 23
C---- Types "KOUNT" blank lines.
C     !DASH
      save
C     !DASH
      integer I, KOUNT
C
C     !BEG
      if(KOUNT.gt.0) then
        do 101 I = 1,KOUNT
          write (*,100)
  100     format(' ')
  101   continue
      end if
C     !END
C
      return
      end
