      subroutine SULFUR
     $(LU,N,K,XJNU,DL,TIT)
C
C     Rudolf Loeser, 1977 Dec 12
C---- Prints PRD Jnu simply (see "FOSFOR").
C     !DASH
      save
C     !DASH
      real*8 DL, XJNU
      integer I, IE, IS, J, K, LU, N
      character TIT*(*)
C     !DASH
      external  LINER, HI, BYE
      intrinsic min
C
C               XJNU(N,K), DL(K)
      dimension XJNU(N,*), DL(*)
C
      call HI ('SULFUR')
C     !BEG
      if(LU.gt.0) then
        call LINER   (2, LU)
        write (LU,100) TIT
  100   format(' ',A)
C
        IE = 0
  101   continue
          IS = IE+1
          IE = min(IE+9,N)
C
          call LINER (1, LU)
          write (LU,102) (I,I=IS,IE)
  102     format(' ',13X,'DL',6X,'Depth',I5,8I12)
          call LINER (1, LU)
C
          do 104 J = 1,K
            write (LU,103) J,DL(J),(XJNU(I,J),I=IS,IE)
  103       format(' ',4X,I3,1P10E12.4)
  104     continue
C
        if(IE.lt.N) goto 101
C
      end if
C     !END
      call BYE ('SULFUR')
C
      return
      end
