      subroutine MERGED
     $(A,K,B,L,Z,M,MAX,KODE)
C     Rudolf Loeser, 1983 Jun 07
C---- Given array A, of length K, with A(i+1) .ge. A(i),
C     and array B, of length L, with B(i+1) .ge. B(i);
C     this routine merges them,
C     producing array Z, of length M .le. MAX, with Z(i+1) .ge. Z(i).
C
C     (The caller must supply the value of MAX; this routine
C     will set the elements of Z and compute M and KODE.
C     If the value of M is 0, then A and B were both empty.)
C
C     Upon return, the value of KODE tells whether or not things
C     went allright.
C     One normal return and three error returns are possible:
C     KODE =  1 means - all is OK (normal return);
C     KODE =  0 means - (K+L) .gt. MAX;
C     KODE = -1 means - A(i) .gt. A(i+1), some i;
C     KODE = -2 means - B(i) .gt. B(i+1), some i.
C     (Note: error checks are done in the above order.)
C     !DASH
      save
C     !DASH
      real*8 A, B, Z
      integer I, J, K, KODE, L, M, MAX, NA, NAR, NB, NBR
C     !DASH
      external MOVED
C
      dimension A(K), B(L), Z(MAX)
C     !EJECT
C
C     !BEG
C---- Make sure MAX is big enough
      if((K+L).gt.MAX) then
        KODE = 0
        goto 103
      end if
C---- Make sure A increases monotonically
      if(K.gt.1) then
        do 100 I = 2,K
          if(A(I).lt.A(I-1)) then
            KODE = -1
            goto 103
          end if
  100   continue
      end if
C---- Make sure B increases monotonically
      if(L.gt.1) then
        do 101 I = 2,L
          if(B(I).lt.B(I-1)) then
            KODE = -2
            goto 103
          end if
  101   continue
      end if
C---- No errors
      KODE = 1
C---- Special case if K+L = 0
      if((K+L).le.0) then
        M = 0
        goto 103
      end if
C---- Special case if K=0
      if(K.le.0) then
        call MOVED (B,1,L,Z,1,L)
        M = L
        goto 103
      end if
C---- Special case if L=0
      if(L.le.0) then
        call MOVED (A,1,K,Z,1,L)
        M = K
        goto 103
      end if
C     !EJECT
C---- Now, general merge
C     NA: index of current element of A;
C     NB: index of current element of B; and
C     M : index of last-filled element of Z.
      NA = 1
      NB = 1
      M  = 0
  102 continue
C----   Compare current A and current B, to choose the smaller
        if(A(NA).le.B(NB)) then
C----     Copy current A into next element of Z
          M = M+1
          Z(M) = A(NA)
C----     Increment NA
          NA = NA+1
          if(NA.gt.K) then
C----       A is now used up; check whether any B's are left
            NBR = L-NB
            if(NBR.gt.0) then
C----         Copy remaining B's into Z
              call MOVED (B(NB),1,NBR,Z(M+1),1,NBR)
C----         Compute final value of M
              M = M+NBR
            end if
            goto 103
          else
C----       There are A's left; compare again, using next A
            goto 102
          end if
        else
C----     Copy current B into next element of Z
          M = M+1
          Z(M) = B(NB)
C----     Increment NB
          NB = NB+1
          if(NB.gt.L) then
C----       B is now used up; check whether any A's are left
            NAR = K-NA
            if(NAR.gt.0) then
C----         Copy remaining A's into Z
              call MOVED (A(NA),1,NAR,Z(M+1),1,NAR)
C----         Compute final value of M
              M = M+NAR
            end if
            goto 103
          else
C----       There are B's left; compare again, using next B
            goto 102
          end if
        end if
C---- Go home
  103 continue
C     !END
C
      return
      end
