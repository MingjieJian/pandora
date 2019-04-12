      subroutine CYNON
     $(A,B,C,D,S,M,Y,W,IW)
C
C     Rudolf Loeser, 1998 Mar 10
C---- Sets up the solution of the four-diagonal diffusion equations.
C     !DASH
      save
C     !DASH
      real*8 A, B, C, D, S, W, Y
      integer IN, IS, IW, IXMAT, M, MOX
C     !COM
C---- MATRIX      as of 2006 Sep 06
      integer     PRNSW,EDJSW,KNTIN,KNTED
      real*8      CRITJ,TIMIN,TIMED
      common      /MATRIX1/ PRNSW,EDJSW,KNTIN,KNTED
      common      /MATRIX2/ CRITJ,TIMIN,TIMED
C
C     Control parameters for matrix inversion and determinants.
C
C     PRNSW = 1: print matrix messages; = 0: do not.
C     EDJSW = 1: edit out "junk" using CRITJ; = 0: do not.
C     KNTIN      count of calls to INVERS.
C     KNTED      count of calls to DETERM.
C     CRITJ      "junk" criterion for EDJSW.
C     TIMIN      total time for all matrix inversions.
C     TIMED      total time for all determinants.
C     .
C     !DASH
      external CLYRO, CHIRK, WGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               J = N+MXTAP
C
C               A(J), B(J), C(J), D(J), S(J), Y(J)
      dimension A(*), B(*), C(*), D(*), S(*), Y(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IXMAT )
C
      call HI ('CYNON')
C     !BEG
C     (Get W allotment)
      call CLYRO (IN, IS, MOX, 'CYNON', M)
C
      call CHIRK (A, B, C, D, W(IXMAT), S, M, Y, W, IW)
C
C     (Give back W allotment)
      call WGIVE (W, 'CYNON')
C     !END
      call BYE ('CYNON')
C
      return
      end
