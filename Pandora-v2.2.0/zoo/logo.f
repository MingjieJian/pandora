      subroutine LOGO
     $(ARG,N,MODE,SIG,RES)
C
C     Rudolf Loeser, 2006 Oct 06
C---- Computes RES, the log10's of a set of arguments ARG (length N).
C
C     If input MODE .eq. 1, returns RES = SIG if ARG .le. 0;
C
C     if input MODE .ne. 1, returns RES = SIG if ARG .eq. 0, otherwise
C                           using |ARG| for the calculation.
C     !DASH
      save
C     !DASH
      real*8 ARG, RES, SIG, ZERO
      integer I, MODE, N
C     !DASH
      intrinsic abs
C
      dimension ARG(*), RES(*)
C
      data ZERO /0.D0/
C
C     !BEG
      do 100 I = 1,N
        if(MODE.eq.1) then
          if(ARG(I).le.ZERO) then
            RES(I) = SIG
          else
            RES(I) = log10(ARG(I))
          end if
        else
          if(ARG(I).eq.ZERO) then
            RES(I) = SIG
          else
            RES(I) = log10(abs(ARG(I)))
          end if
        end if
  100 continue
C     !END
C
      return
      end
