      subroutine SING2
     $(N,FLAG,KRET)
C     Rudolf Loeser, 1979 Apr 05
C---- Common pop-stack-and-exit code for "SING" routines.
C     !DASH
      save
C     !DASH
      real*4 CONV, F, XN, XNC, XNR
      integer FLAG, I, IL, IU, J, K, KRET, L, M, N, NC, NR
C     !COM
      common /SING01/ I,J,K,L,M,NC,NR,IU,IL
      common /SORTST/ XNC,XNR
C     !DASH
      intrinsic float
C
      dimension IU(23),IL(23)
C
      data CONV /0.69315/
C
C     !BEG
      KRET = 1
      M    = M-1
      if(M.gt.0) then
        I = IL(M)
        J = IU(M)
      else
        XN   = N
        F    = CONV/(XN*log(XN))
        XNC  = F*float(NC)
        XNR  = F*float(NR)
        FLAG = 1
        KRET = 2
      end if
C     !END
C
      return
      end
