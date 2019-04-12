      subroutine EDISET2
     $(F,N,CRIT,KODE,R)
C     Rudolf Loeser, 1990 Jun 28
C---- Fixes "bad" values, for EDIT2.
C     !DASH
      save
C     !DASH
      real*8 CRIT, F, R
      integer I, KODE, N
      logical BAD
C     !DASH
      external EDITEST
C
      dimension F(N), R(N)
C
C     !BEG
      do 100 I = 1,N
        call EDITEST (F(I),CRIT,KODE,BAD)
        if(BAD) then
          F(I) = R(I)
        end if
  100 continue
C     !END
C
      return
      end
