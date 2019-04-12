      subroutine CHULYM
     $(N,C,WH,S,H)
C
C     Rudolf Loeser, 1985 Jan 23
C---- Core of calculation of H, the Line Flux Distribution, for a
C     particular frequency.
C     (This is version 2 of CHULYM.)
C     !DASH
      save
C     !DASH
      real*8 C, H, S, SUM, WH
      integer I, J, N
      logical DUMP
C     !DASH
      external ORYX, OKAPI, HI, BYE
C
C               C(N), WH(N,N), S(N), H(N)
      dimension C(*), WH(N,*), S(*), H(*)
C
      call HI ('CHULYM')
C     !BEG
      call ORYX    (N, H, C, S, WH, 'CHULYM', DUMP)
      do 101 I = 1,N
C
        SUM = WH(I,1)*S(1)
        do 100 J = 2,N
          SUM = SUM+WH(I,J)*S(J)
  100   continue
C
        H(I) = H(I)+C(I)*SUM
  101 continue
C
      if(DUMP) then
        call OKAPI (N, H, 'CHULYM')
      end if
C     !END
      call BYE ('CHULYM')
C
      return
      end
