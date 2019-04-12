      subroutine TAKE
     $(LU,N,A,LA,B,LB,R)
C
C     Rudolf Loeser, 1999 Dec 29
C---- Compares A and B, computes R = A/B, and prints them.
C     !DASH
      save
C     !DASH
      real*8 A, B, R
      integer I, J, K, LU, M, N
      character BLANKS*14, CA*15, CB*15, CC*15, LA*(*), LABA*14,
     $          LABB*14, LB*(*), LINJ*60, LINK*60
C     !DASH
      external  CRAVAT, LINER, SHIM, HI, BYE
      intrinsic len
C
C               A(N), B(N), R(N)
      dimension A(*), B(*), R(*)
C
      data BLANKS /'              '/
C
      call HI ('TAKE')
C     !BEG
      M = 14-len(LA)
      LABA = BLANKS(:M)//LA
      M = 14-len(LB)
      LABB = BLANKS(:M)//LB
      write (LU,100) LABA,LABB,LABA,LABB
  100 format(' ',6X,A14,1X,A14,7X,'Compared',5X,'Ratio',5X,
     $           6X,A14,1X,A14,7X,'Compared',5X,'Ratio')
      call LINER      (1, LU)
C
      M = (N+1)/2
      J = 0
      K = M
      do 103 I = 1,M
        J = J+1
        call CRAVAT   (A(J), B(J), R(J), CA, CB, CC)
        write (LINJ,101) J,CA,CB,CC,R(J)
  101   format(I5,3A15,1PE10.2)
        K = K+1
        if(K.le.N) then
          call CRAVAT (A(K), B(K), R(K), CA, CB, CC)
          write (LINK,101) K,CA,CB,CC,R(K)
        else
          LINK = BLANKS
        end if
        write (LU,102) LINJ,LINK
  102   format(' ',A60,5X,A60)
        call SHIM     (I, 5, LU)
  103 continue
C     !END
      call BYE ('TAKE')
C
      return
      end
