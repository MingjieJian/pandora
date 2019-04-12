      subroutine BOUNDUP
     $(N,X,ABOVE)
C
C     Rudolf Loeser, 2001 Dec 21
C---- Edits X so that no value is greater than ABOVE.
C     !DASH
      save
C     !DASH
      real*8 ABOVE, X
      integer I, N
C     !DASH
      intrinsic min
C
      dimension X(*)
C
C     !BEG
      if(N.gt.0) then
        do 100 I = 1,N
          X(I) = min(X(I),ABOVE)
  100   continue
      end if
C     !END
C
      return
      end
