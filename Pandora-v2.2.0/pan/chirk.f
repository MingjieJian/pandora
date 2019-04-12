      subroutine CHIRK
     $(A,B,C,D,X,S,N, Y, W,IW)
C
C     Rudolf Loeser, 1998 Mar 10
C---- Solves the four-diagonal diffusion equations.
C     !DASH
      save
C     !DASH
      real*8 A, B, C, D, S, SUM, W, X, Y
      integer I, IW, J, KODE, N
      character TITLE*40
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external ZERO1, MOTOR, HALT, HI, BYE
C
      dimension W(*), IW(*)
C
C               X(N,N), A(N), B(N), C(N), D(N), S(N), Y(N)
      dimension X(N,*), A(*), B(*), C(*), D(*), S(*), Y(*)
C
      data TITLE /'Four-diagonal diffusion equations'/
C     !EJECT
C
      call HI ('CHIRK')
C     !BEG
      call ZERO1  (X, (N*N))
C
      do 100 I = 1,N
        X(I,I)     = B(I)
        if((I+1).le.N) then
          X(I,I+1) = A(I)
        end if
        if((I-1).ge.1) then
          X(I,I-1) = C(I)
        end if
        if((I-2).ge.1) then
          X(I,I-2) = D(I)
        end if
  100 continue
C
      call MOTOR  (X, N, TITLE, W, IW, KODE)
      if(KODE.ne.1) then
        write (MSSLIN(1),101)
  101   format('No Plan-B available.')
        call HALT ('CHIRK', 1)
      end if
C
      do 103 I = 1,N
        SUM = X(I,1)*S(1)
        do 102 J = 2,N
          SUM = SUM+X(I,J)*S(J)
  102   continue
        Y(I) = SUM
  103 continue
C     !END
      call BYE ('CHIRK')
C
      return
      end
