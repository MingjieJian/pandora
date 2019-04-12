      subroutine BLOKE
     $(MN1,NUM,M,AA,BA,CA,DA,EA,AB,BB,CB,DB,EB,H,P,KBINN,KBOUT,
     $ ALFB,BETB,X,S)
C     Rudolf Loeser, 1998 Jun 30
C---- Sets up grand X-matrix, and S-vector, for OBELISK.
C
C---- Notes:
C
C     KBINN can = 0 or 1, KBOUT can = 0 or 1; but only may = 1.
C
C     NUM = MN1-KBINN-KBOUT; thus NUM can = MN1 or MN1-1.  M = 2*NUM.
C
C     X is a square matrix of order M; it consists of four square
C     submatrices, each of order NUM.
C
C     S is a vector of length M; it consists of two subvectors, each
C     of length NUM.
C
C     See writeups (98 Jun 29) [=981]; (98 Jul 07) [=982].
C     !DASH
      save
C     !DASH
      real*8 AA, AB, ALFB, BA, BB, BETB, CA, CB, DA, DB, EA, EB, H, P,
     $       S, X
      integer I, J, K, KBINN, KBOUT, L, M, MN1, NUM
      logical BCI, BCO
C     !DASH
      external ZERO1, MOVE1, HI, BYE
C
C               X(2*N,2*N), S(2*N), H(2,2,N), P(N), AA(N), BA(N), CA(N),
      dimension X(M,*),     S(*),   H(2,2,*), P(*), AA(*), BA(*), CA(*),
C
C               DA(N), EA(N), AB(N), BB(N), CB(N), DB(N), EB(N)
     $          DA(*), EA(*), AB(*), BB(*), CB(*), DB(*), EB(*)
C
      call HI ('BLOKE')
C     !BEG
      BCI = KBINN.eq.1
      BCO = KBOUT.eq.1
C
      call ZERO1 (S,NUM)
      call MOVE1 (P(1+KBINN),NUM,S(NUM+1))
      call ZERO1 (X,(M*M))
C
      K = 0
      do 100 I = (1+KBINN),(MN1-KBOUT)
        J = I+NUM
        K = K+1
        L = K+NUM
C
C     I and J are indices for X:
C     (I,I) is the diagonal of the UL submatrix, (J,J) is the diagonal
C     of the LR submatrix, (I,J) is the diagonal of the UR submatrix,
C     and (J,I) is the diagonal of the LL submatrix.
C
C     K and L are indices for S:
C     K for the upper and L for the lower subvector.
C     !EJECT
        if(I.eq.1) then
C
          if(.not.BCI) then
            X(I,I  ) = CA(I)+H(1,1,I)
            X(I,I+1) = BA(I)
            X(I,I+2) = AA(I)
C
            X(J,J  ) = CB(I)+H(2,2,I)
            X(J,J+1) = BB(I)
            X(J,J+2) = AB(I)
C
            X(I,J) = H(1,2,I)
            X(J,I) = H(2,1,I)
          end if
C
        else if(I.eq.2) then
C
          if(BCI) then
            S(K) = S(K)-ALFB*DA(I)
          else
            X(I,I-1) = DA(I)
          end if
          X(I,I  ) = CA(I)+H(1,1,I)
          X(I,I+1) = BA(I)
          X(I,I+2) = AA(I)
C
          if(BCI) then
            S(L) = S(L)-BETB*DB(I)
          else
            X(J,J-1) = DB(I)
          end if
          X(J,J  ) = CB(I)+H(2,2,I)
          X(J,J+1) = BB(I)
          X(J,J+2) = AB(I)
C
          X(I,J) = H(1,2,I)
          X(J,I) = H(2,1,I)
C     !EJECT
        else if(I.eq.3) then
C
          if(BCI) then
            S(K) = S(K)-ALFB*EA(I)
          else
            X(I,I-2) = EA(I)
          end if
          X(I,I-1) = DA(I)
          X(I,I  ) = CA(I)+H(1,1,I)
          X(I,I+1) = BA(I)
          X(I,I+2) = AA(I)
C
          if(BCI) then
            S(L) = S(L)-BETB*EB(I)
          else
            X(J,J-2) = EB(I)
          end if
          X(J,J-1) = DB(I)
          X(J,J  ) = CB(I)+H(2,2,I)
          X(J,J+1) = BB(I)
          X(J,J+2) = AB(I)
C
          X(I,J) = H(1,2,I)
          X(J,I) = H(2,1,I)
C
        else if(I.eq.(MN1-2)) then
C
          X(I,I-2) = EA(I)
          X(I,I-1) = DA(I)
          X(I,I  ) = CA(I)+H(1,1,I)
          X(I,I+1) = BA(I)
          if(.not.BCO) then
            X(I,I+2) = AA(I)
          else
            S(K) = S(K)-ALFB*AA(I)
          end if
C
          X(J,J-2)   = EB(I)
          X(J,J-1)   = DB(I)
          X(J,J  )   = CB(I)+H(2,2,I)
          X(J,J+1)   = BB(I)
          if(.not.BCO) then
            X(J,J+2) = AB(I)
          else
            S(L) = S(L)-BETB*AB(I)
          end if
C
          X(I,J) = H(1,2,I)
          X(J,I) = H(2,1,I)
C     !EJECT
        else if(I.eq.(MN1-1)) then
C
          X(I,I-2) = EA(I)
          X(I,I-1) = DA(I)
          X(I,I  ) = CA(I)+H(1,1,I)
          if(.not.BCO) then
            X(I,I+1) = BA(I)
          else
            S(K) = S(K)-ALFB*BA(I)
          end if
C
          X(J,J-2) = EB(I)
          X(J,J-1) = DB(I)
          X(J,J  ) = CB(I)+H(2,2,I)
          if(.not.BCO) then
            X(J,J+1) = BB(I)
          else
            S(L) = S(L)-BETB*BB(I)
          end if
C
          X(I,J) = H(1,2,I)
          X(J,I) = H(2,1,I)
C
        else if(I.eq.MN1) then
C
          if(.not.BCO) then
            X(I,I-2) = EA(I)
            X(I,I-1) = DA(I)
            X(I,I  ) = CA(I)+H(1,1,I)
C
            X(J,J-2) = EB(I)
            X(J,J-1) = DB(I)
            X(J,J  ) = CB(I)+H(2,2,I)
C
            X(I,J) = H(1,2,I)
            X(J,I) = H(2,1,I)
          end if
C     !EJECT
        else
C
          X(I,I-2) = EA(I)
          X(I,I-1) = DA(I)
          X(I,I  ) = CA(I)+H(1,1,I)
          X(I,I+1) = BA(I)
          X(I,I+2) = AA(I)
C
          X(J,J-2) = EB(I)
          X(J,J-1) = DB(I)
          X(J,J  ) = CB(I)+H(2,2,I)
          X(J,J+1) = BB(I)
          X(J,J+2) = AB(I)
C
          X(I,J) = H(1,2,I)
          X(J,I) = H(2,1,I)
C
        end if
C
  100 continue
C
C     !END
      call BYE ('BLOKE')
C
      return
      end
