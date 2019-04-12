      subroutine FOSFOR
     $(LU,N,K,XJNU,DL,JNUX,TIT)
C
C     Rudolf Loeser, 1977 Dec 12
C---- Prints PRD Jnu fancily, (see "SULFUR").
C     !DASH
      save
C     !DASH
      real*8 DL, VALUE, XJNU
      integer I, IE, INDEX, IS, J, JNUX, K, L, LU, M, N, NV
      logical JNZ
      character KODEV*1, MARK*1, TIT*(*)
C     !DASH
      external  NAUGHTD, NOG, LINER, HI, BYE
      intrinsic min, max
C
C               XJNU(N,K), JNUX(N,K), DL(K)
      dimension XJNU(N,*), JNUX(N,*), DL(*)
C
      parameter (NV=9)
      dimension VALUE(NV), KODEV(NV), MARK(3)
C
      data MARK /'-', ' ', '+'/
C
      call HI ('FOSFOR')
C     !BEG
      call LINER   (3,LU)
      write (LU,100) TIT
  100 format(' ',A)
C
      call NAUGHTD (XJNU,1,(N*K),JNZ)
      if(JNZ) then
        write (LU,101)
  101   format(' ','Equals zero.')
      else
C     !EJECT
        call NOG     (N,K,XJNU,JNUX)
C
        IE = 0
  102   continue
          IS = IE+1
          IE = min(IE+NV,N)
C
          call LINER (1,LU)
          write (LU,103) (I,I=IS,IE)
  103     format(' ',13X,'DL',5X,'Depth',I5,8I12)
          call LINER (1,LU)
C
          do 106 J = 1,K
C
            M = 0
            do 104 I = IS,IE
              M = M+1
              INDEX = min(max((JNUX(I,J)+2),1),3)
              VALUE(M) = XJNU(I,J)
              KODEV(M) = MARK(INDEX)
  104       continue
C
            write (LU,105) J,DL(J),(VALUE(L),KODEV(L),L=1,M)
  105       format(' ',4X,I3,1PE12.4,9(E11.4,A1))
  106     continue
C
        if(IE.lt.N) goto 102
      end if
C     !END
      call BYE ('FOSFOR')
C
      return
      end
