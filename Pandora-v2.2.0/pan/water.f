      subroutine WATER
     $(Z,F,Y,N, ZR,FR,YR,NR)
C     Rudolf Loeser, 1991 Jan 16
C---- Integrates "backwards."
C     (This is version 2 of WATER.)
C     !DASH
      save
C     !DASH
      real*8 F, FR, Y, YR, Z, ZR
      integer N, NR
C     !DASH
      external  MOVE1, ZERO1, REVERSD, BUSH, HI, BYE
C
C               Z(N), F(N), Y(N), ZR(NR), FR(NR), YR(NR)
      dimension Z(*), F(*), Y(*), ZR(*),  FR(*),  YR(*)
C
      call HI ('WATER')
C     !BEG
      call MOVE1   (Z,NR,ZR)
      call REVERSD (ZR,1,NR)
      call MOVE1   (F,NR,FR)
      call REVERSD (FR,1,NR)
      call BUSH    (ZR,1,FR,1,YR,1,NR)
      call REVERSD (YR,1,NR)
      call MOVE1   (YR,NR,Y)
      if(NR.lt.N) then
        call ZERO1 (Y(NR+1),(N-NR))
      end if
C     !END
      call BYE ('WATER')
C
      return
      end
