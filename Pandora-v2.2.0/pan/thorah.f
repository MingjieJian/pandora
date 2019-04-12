      subroutine THORAH
     $(N,Z,DEE,VEC,DOLD,W,IW,DUMP)
C
C     Rudolf Loeser, 2004 Feb 09
C---- Smoothing of d-coefficients for diffusion.
C     !DASH
      save
C     !DASH
      real*8 DEE, DOLD, VEC, W, Z
      integer I, INDX, IW, J, N, jummy
      logical DUMP, lummy
      character LABEL*20, TYPE*3
C     !DASH
      external MOVE1, MOVED, SMOOTH, VESTRY, ACTON, HI, BYE
C
      dimension W(*), IW(*)
C
C               Z(N), VEC(N), DEE(4,5,N), DOLD(4,5,N)
      dimension Z(*), VEC(*), DEE(4,5,*), DOLD(4,5,*)
C
      data TYPE,INDX /'lin', 0/
C
      call HI ('THORAH')
C     !BEG
      if(DUMP) then
C----   Save originals for dump
        call MOVE1    (DEE, (20*N), DOLD)
      end if
C
C---  Do smoothing for individual coefficients as functions of Z
      do 102 I = 1,4
        do 101 J = 1,5
          write (LABEL,100) I,J
  100     format('d(',I1,',',I1,') coefficient')
          call MOVED  (DEE(I,J,1), 20, N, VEC, 1, N)
          call SMOOTH (Z, VEC, N, TYPE, LABEL, INDX, W, IW, jummy,
     $                 lummy)
          call MOVED  (VEC, 1, N, DEE(I,J,1), 20, N)
  101   continue
  102 continue
C
      if(DUMP) then
C----   Details of smoothing
        call VESTRY   (N, Z, DEE, DOLD)
C----   Plot
        call ACTON    (N, DEE, DOLD, 'smoothed')
      end if
C     !END
      call BYE ('THORAH')
C
      return
      end
