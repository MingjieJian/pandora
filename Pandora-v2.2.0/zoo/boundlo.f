      subroutine BOUNDLO
     $(N,X,BELOW)
C
C     Rudolf Loeser, 2001 Dec 21
C---- Edits X so that no value is less than BELOW.
C     !DASH
      save
C     !DASH
      real*8 BELOW, X
      integer I, N
C     !DASH
      intrinsic max
C
      dimension X(*)
C
C     !BEG
      if(N.gt.0) then
        do 100 I = 1,N
          X(I) = max(X(I),BELOW)
  100   continue
      end if
C     !END
C
      return
      end
