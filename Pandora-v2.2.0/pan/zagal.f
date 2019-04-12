      subroutine ZAGAL
     $(II,SYM,N,M,X,Y,IMAGE)
C
C     Rudolf Loeser, 1982 Dec 14
C---- Enters points to be plotted.
C     (This is version 2 of ZAGAL.)
C     !DASH
      save
C     !DASH
      real*8 X, Y, YL
      integer I, II, J, K, LINC, M, N
      character IMAGE*(*), SYM*1
C     !DASH
      external LINK, HI, BYE
C
C               II(5), X(M), Y(N,M)
      dimension II(*), X(*), Y(N,*)
C
      dimension SYM(5)
C
      call HI ('ZAGAL')
C     !BEG
      do 101 K = 1,5
        I = II(K)
        if(I.gt.0) then
C
          LINC = 1
          do 100 J = 1,M
            YL = log10(Y(I,J))
            call LINK (IMAGE, X(J), YL, SYM(K), LINC)
  100     continue
C
        end if
  101 continue
C     !END
      call BYE ('ZAGAL')
C
      return
      end
