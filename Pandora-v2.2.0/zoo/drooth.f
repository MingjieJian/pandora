      subroutine DROOTH
     $(N,NC,Z,FOLD,DOLD, D,DS, FNEW)
C
C     Rudolf Loeser, 1999 Dec 07
C---- Makes FNEW(Z), with a smooother first derivative than FOLD(Z).
C     !DASH
      save
C     !DASH
      real*8 D, DOLD, DS, FNEW, FOLD, FRAC, HALF, Z
      integer I, J, N, NC
      logical INCR
C     !DASH
      dimension Z(*), FOLD(*), DOLD(*), D(*), DS(*), FNEW(*)
C
      data HALF /5.D-1/
C
      call HI ('DROOTH')
C     !BEG
      INCR = FOLD(1).lt.FOLD(N)
C
      do 104 J = 1,NC
C
        do 100 I = 1,N
          if(J.eq.1) then
            D(I) = DOLD(I)
          else
            D(I) = DS(I)
          end if
  100   continue
C
        DS(1) = D(1)
        do 101 I = 2,(N-1)
          FRAC  = ((Z(I)-Z(I-1))/(Z(I+1)-Z(I-1)))
          DS(I) = D(I-1)+(D(I+1)-D(I-1))*FRAC
  101   continue
        DS(N) = D(N)
C
        if(INCR) then
          FNEW(1) = FOLD(1)
          do 102 I = 2,N,+1
            FNEW(I) = FNEW(I-1)+HALF*((DS(I-1)+DS(I))*(Z(I)-Z(I-1)))
  102     continue
        else
          FNEW(N) = FOLD(N)
          do 103 I = (N-1),1,-1
            FNEW(I) = FNEW(I+1)-HALF*((DS(I)+DS(I+1))*(Z(I+1)-Z(I)))
  103     continue
        end if
C
  104 continue
C     !END
      call BYE ('DROOTH')
C
      return
      end
