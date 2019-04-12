      subroutine BRONZE
     $(N,PQ,CP,S,IMG,FO,DMP1,RHO)
C
C     Rudolf Loeser, 1981 Feb 06
C           revised, 2004 May 06
C
C---- Computes RHO (net radiative bracket).
C     !DASH
      save
C     !DASH
      real*8 CP, FO, PQ, RAT, RHO, S
      integer I, IMG, N
      logical DMP1
C     !DASH
      external DIVIDE, BIFROST, LUNE, HI, BYE
C
C               PQ(N), CP(N), S(N), RHO(N), FO(N), IMG(N)
      dimension PQ(*), CP(*), S(*), RHO(*), FO(*), IMG(*)
C
      call HI ('BRONZE')
C     !BEG
      do 100 I = 1,N
        call DIVIDE (CP(I), S(I), RAT)
        RHO(I) = PQ(I)-RAT
  100 continue
C
C---- Edit bad values if needed
      call BIFROST  (RHO, IMG, FO, N)
C
      if(DMP1) then
        call LUNE   (N, PQ, CP, S, RHO)
      end if
C     !END
      call BYE ('BRONZE')
C
      return
      end
