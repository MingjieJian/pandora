      subroutine BARNEY
     $(N,INC,GAW,PA,PG,PB,OMD,DLC,PQ,PD,AW,ANT)
C
C     Rudolf Loeser, 2004 May 05
C---- Initializes frequency/angle sums
C     (This is version 2 of BARNEY.)
C     !DASH
      save
C     !DASH
      real*8 ANT, AW, DLC, OMD, PA, PB, PD, PG, PQ
      integer N, N2
      logical GAW, INC
C     !DASH
      external ZERO1, HI, BYE
C
C               PA(N,N), PG(N,N), PQ(N), PB(N), ANT(N), DLC(N), OMD(N),
      dimension PA(*),   PG(*),   PQ(*), PB(*), ANT(*), DLC(*), OMD(*),
C
C               PD(N), AW(N)
     $          PD(*), AW(*)
C
      call HI ('BARNEY')
C     !BEG
      N2 = N*N
      call ZERO1   (PA , N2)
      call ZERO1   (PG , N2)
      call ZERO1   (PB , N )
      call ZERO1   (PQ , N )
      call ZERO1   (OMD, N )
      call ZERO1   (DLC, N )
      call ZERO1   (ANT, N )
      if(INC) then
        call ZERO1 (PD , N )
      end if
      if(GAW) then
        call ZERO1 (AW , N )
      end if
C     !END
      call BYE ('BARNEY')
C
      return
      end
