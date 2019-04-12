      subroutine BOUNDS
     $(N,BELOW,X,ABOVE)
C
C     Rudolf Loeser, 2001 Dec 21
C---- Edits X so that no value is less than BELOW or greater
C     than ABOVE.
C     !DASH
      save
C     !DASH
      real*8 ABOVE, BELOW, X
      integer N
C     !DASH
      external BOUNDLO, BOUNDUP
      dimension X(*)
C
C     !BEG
      if(N.gt.0) then
        call BOUNDLO (N,X,BELOW)
        call BOUNDUP (N,X,ABOVE)
      end if
C     !END
C
      return
      end
