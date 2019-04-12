      subroutine HELOT
     $(N,INT1,TAUIN,TAU)
C
C     Rudolf Loeser, 1983 May 24
C---- Edits "TAU=1" data, for CLOTEN.
C     (This is version 2 of HELOT.)
C     !DASH
      save
C     !DASH
      real*8 TAU, TAUIN
      integer INT1, N, NRPMX
C     !DASH
      external HI, BYE
C
C               TAU(N)
      dimension TAU(*)
C
      call HI ('HELOT')
C     !BEG
      NRPMX = 2*N+5
C
      if(INT1.le.(N-2)) then
        INT1  = INT1+1
        TAUIN = TAU(INT1-1)
C
      else if(INT1.eq.(N-1)) then
        TAUIN = TAU(INT1-1)
C
      else if((INT1.ge.N).and.(INT1.le.(N+2))) then
        INT1  = N
        TAUIN = TAU(N+2)
C
      else if((INT1.eq.(N+3)).or.(INT1.eq.(N+4))) then
        INT1  = -N
        TAUIN = TAU(N+2)
C
      else if(INT1.eq.(N+5)) then
        INT1  = -N+1
        TAUIN = TAU(NRPMX+INT1)
C
      else
        INT1  = INT1-NRPMX
        TAUIN = TAU(NRPMX+INT1)
      end if
C     !END
      call BYE ('HELOT')
C
      return
      end
