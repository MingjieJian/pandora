      subroutine SINGA
     $(A,N,FLAG)
C     Rudolf Loeser, 1991 Sep 10
C---- Special version of SINGD; see remarks in "SING."
C     !DASH
      save
C     !DASH
      real*8 A, T, TT
      real*4 R, RA, RB, RC, RD
      integer FLAG, I, IJ, IL, IU, J, K, KRET, L, M, N, NC, NR, jummy
C     !COM
      common /SING01/ I,J,K,L,M,NC,NR,IU,IL
      common /SINGRC/ RA,RB,RC,RD
C     !DASH
      external  SING1, SING2, SING3, SING4
      intrinsic float, ifix, abs
C
      dimension A(*),IU(23),IL(23)
C
C     !BEG
      call SING1     (N,FLAG,jummy,0,KRET)
      if(KRET.eq.1) then
        R = RA
  100   continue
        if(I.ge.J) then
          call SING2 (N,FLAG,KRET)
          goto (105,108), KRET
        end if
  101   continue
        K  = I
        NR = NR+1
        call SING3   (R)
        IJ = ifix(R*float(J-I))+I
        T  = A(IJ)
        NC = NC+1
        if(abs(A(I)).gt.abs(T)) then
          A(IJ) = A(I)
          A(I)  = T
          T     = A(IJ)
        end if
        L  =J
        NC =NC+1
        if(abs(A(J)).lt.abs(T)) then
          A(IJ) = A(J)
          A(J)  = T
          T     = A(IJ)
          NC = NC+1
          if(abs(A(I)).gt.abs(T)) then
            A(IJ) = A(I)
            A(I)  = T
            T     = A(IJ)
          end if
        end if
        goto 103
C     !EJECT
  102   continue
        A(L) = A(K)
        A(K) = TT
  103   continue
        L  = L-1
        NC = NC+1
        if(abs(A(L)).gt.abs(T)) goto 103
        TT = A(L)
  104   continue
        K  = K+1
        NC = NC+1
        if(abs(A(K)).lt.abs(T)) goto 104
        if        (K.le.L)      goto 102
        call SING4     (FLAG,KRET)
  105   continue
        if(KRET.eq.1) then
          if((J-I).ge.11) goto 101
          if    (I.eq.1)  goto 100
          I = I-1
  106     continue
          I = I+1
          if(I.eq.J) then
            call SING2 (N,FLAG,KRET)
            goto (105,108), KRET
          end if
          T  = A(I+1)
          NC = NC+1
          if(abs(A(I)).le.abs(T)) goto 106
          K = I
  107     continue
          A(K+1) = A(K)
          K  = K-1
          NC = NC+1
          if(abs(A(K)).gt.abs(T)) goto 107
          A(K+1) = T
          goto 106
        end if
      end if
  108 continue
C     !END
C
      return
      end
