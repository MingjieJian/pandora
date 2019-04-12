      subroutine NEARUD
     $(T,N,X,I)
C     Rudolf Loeser, 1990 Nov 21
C---- Given an unsorted array T of length N,
C     this routine returns with the value of I set such that
C     abs(T(I)-X) is as small as possible.
C     Upon return, I .lt. 1 indicates an error.
C     !DASH
      save
C     !DASH
      real*8 D, DJ, T, X
      integer I, J, N
C     !DASH
      intrinsic abs
C
      dimension T(*)
C
C     !BEG
      if(N.le.0) then
        I = -1
      else if(N.eq.1) then
        I =  1
      else
        D = abs(T(1)-X)
        I = 1
        do 100 J = 2,N
          DJ = abs(T(J)-X)
          if(DJ.lt.D) then
            D = DJ
            I = J
          end if
  100   continue
      end if
C     !END
C
      return
      end
