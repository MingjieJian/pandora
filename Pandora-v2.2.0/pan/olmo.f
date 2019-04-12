      subroutine OLMO
     $(T,TAU,DT,SE2,SES2,SE3,SES3,SE4,SES4,N)
C
C     Rudolf Loeser, 2000 Jan 25
C---- Sets up DT, and associated "save" arrays, for RT weight
C     matrix calculation.
C     !DASH
      save
C     !DASH
      real*8 DT, SE2, SE3, SE4, SES2, SES3, SES4, T, TAU
      integer I, N, N2
C     !DASH
      external  ZERO1, HI, BYE
      intrinsic abs
C
C               SES2(2*N), SES3(2*N), SES4(2*N), SE2(2*N), SE3(2*N),
      dimension SES2(*),   SES3(*),   SES4(*),   SE2(*),   SE3(*),
C
C               SE4(2*N), T(2*N), DT(2*N)
     $          SE4(*),   T(*),   DT(*)
C
      call HI ('OLMO')
C     !BEG
C
      N2 = 2*N
C
      do 100 I = 1,N2
        DT(I) = abs(T(I)-TAU)
  100 continue
C
      call ZERO1 (SE2 ,N2)
      call ZERO1 (SES2,N2)
      call ZERO1 (SE3 ,N2)
      call ZERO1 (SES3,N2)
      call ZERO1 (SE4 ,N2)
      call ZERO1 (SES4,N2)
C     !END
      call BYE ('OLMO')
C
      return
      end
