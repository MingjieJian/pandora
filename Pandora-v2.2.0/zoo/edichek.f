      subroutine EDICHEK
     $(F,N,CRIT,KODE,KOUNT,IMG,BAD)
C     Rudolf Loeser, 1990 Jun 27
C---- Checks to see whether F contains "bad" values, for EDIT1.
C     !DASH
      save
C     !DASH
      real*8 CRIT, F
      integer I, IMG, KODE, KOUNT, N
      logical BAD
C     !DASH
      external EDITEST
C
      dimension F(N), IMG(N)
C
C     !BEG
      KOUNT = 0
      do 100 I = 1,N
        call EDITEST (F(I),CRIT,KODE,BAD)
        if(BAD) then
          KOUNT  = KOUNT+1
          IMG(I) = 1
        else
          IMG(I) = 0
        end if
  100 continue
      BAD = KOUNT.gt.0
C     !END
C
      return
      end
