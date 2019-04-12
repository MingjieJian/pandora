      subroutine BILK
     $(X,S,RES,CHK,M,XO,W,IW)
C
C     Rudolf Loeser, 1998 Jun 30
C---- Inverts X, computes CHK, and solves for RES, for OBELISK.
C     !DASH
      save
C     !DASH
      real*8 CHK, RES, S, W, X, XO, Z
      integer I, IW, J, KODE, M
      character TITLE*50
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external MOTOR, HALT, KIBEL, HI, BYE
C
      dimension W(*), IW(*)
C
C               X(2*N,2*N), S(2*N), RES(2*N), CHK(2*N), XO(2*N,2*N)
      dimension X(M,*),     S(*),   RES(*),   CHK(*),   XO(*)
C
      data TITLE /'2*N matrix for simultaneous He-diffusion solution'/
C
      call HI ('BILK')
C     !BEG
      call MOTOR   (X, M, TITLE, W, IW, KODE)
C
      if(KODE.le.0) then
        write (MSSLIN(1),100)
  100   format('No Plan-B available.')
        call HALT  ('BILK', 1)
      end if
C
C---- Establish CHK = largest offdiagonal elements of XO*X
      call KIBEL   (XO, X, M, CHK)
C
C---- Compute RES
      do 102 I = 1,M
        Z = X(I,1)*S(1)
        do 101 J = 2,M
          Z = Z+X(I,J)*S(J)
  101   continue
        RES(I) = Z
  102 continue
C     !END
      call BYE ('BILK')
C
      return
      end
