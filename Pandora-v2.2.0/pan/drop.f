      subroutine DROP
     $(N,NL,NSL,M,EPCBR,XND,ARHO,PS1)
C
C     Rudolf Loeser, 1978 Jun 26
C---- Computes PS1, a Supplementary Levels term.
C     !DASH
      save
C     !DASH
      real*8 ARHO, EPCBR, PS1, XND
      integer I, J, M, N, NL, NSL
C     !DASH
      external ZERO1, CONMUL, HI, BYE
C
C               XND(N,NL), ARHO(N,NL), PS1(N)
      dimension XND(N,*),  ARHO(N,*),  PS1(*)
C
      call HI ('DROP')
C     !BEG
      call ZERO1  (PS1,N)
C
      do 101 J = 1,NL
        if(J.ne.M) then
C
          do 100 I = 1,N
            PS1(I) = PS1(I)+XND(I,J)*ARHO(I,J)
  100     continue
C
        end if
  101 continue
C
      call CONMUL (EPCBR,PS1,N)
C     !END
      call BYE ('DROP')
C
      return
      end
