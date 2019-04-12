      subroutine ADOLF
     $(DWN,CDL,MP,DMAX,N)
C
C     Rudolf Loeser, 1992 Apr 23
C---- Truncates the far wings, for TABOR.
C     !DASH
      save
C     !DASH
      real*8 CDL, DMAX, DWN
      integer I, MP, N
C     !DASH
      external  HI, BYE
      intrinsic abs
C
C               DWN(MP), CDL(MP)
      dimension DWN(*),  CDL(*)
C
      call HI ('ADOLF')
C     !BEG
      N = 0
      do 100 I = 1,MP
C
        if(abs(DWN(I)).le.DMAX) then
          N = N+1
          DWN(N) = DWN(I)
          CDL(N) = CDL(I)
        end if
C
  100 continue
C     !END
      call BYE ('ADOLF')
C
      return
      end
