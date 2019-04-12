      subroutine DILLA
     $(G1,GNV1,N,MNG1,LIMIT)
C
C     Rudolf Loeser, 1990 May 02
C---- Computes GNV-1 replacement limit.
C     Note: An explanation of this procedure is printed by LETEM!
C     !DASH
      save
C     !DASH
      real*8 G1, GNV1
      integer I, J, LIMIT, MNG1, N
C     !DASH
      external  HI, BYE
      intrinsic abs, min, max
C
C               G1(N), GNV1(N)
      dimension G1(*), GNV1(*)
C
      call HI ('DILLA')
C     !BEG
      if(MNG1.lt.0) then
        J = N+1
        do 100 I = 1,N
          J = J-1
          if((abs(GNV1(J))).gt.(abs(G1(J)))) then
            go to 101
          end if
  100   continue
  101   continue
        LIMIT = min(J,-MNG1)
C
      else
        LIMIT = MNG1
      end if
C
      LIMIT = min(max(LIMIT,0),N)
C     !END
      call BYE ('DILLA')
C
      return
      end
