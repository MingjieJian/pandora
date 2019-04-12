      subroutine FOPO
     $(N,NSL,RKI,IQRK,CQIN,PKS)
C
C     Rudolf Loeser, 1980 Mar 12
C---- Accounts for K-Shell ionization.
C     !DASH
      save
C     !DASH
      real*8 CQIN, PKS, RKI
      integer I, IQRK, J, N, NSL
C     !DASH
      external HI, BYE
C
C               RKI(N,NSL), IQRK(NSL), CQIN(N), PKS(N)
      dimension RKI(N,*),   IQRK(*),   CQIN(*), PKS(*)
C
      call HI ('FOPO')
C     !BEG
      do 101 J = 1,NSL
        if(IQRK(J).gt.0) then
C
          do 100 I = 1,N
            RKI(I,J) = RKI(I,J)+(CQIN(I)+PKS(I))
  100     continue
C
        end if
  101 continue
C     !END
      call BYE ('FOPO')
C
      return
      end
