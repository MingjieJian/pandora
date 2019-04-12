      subroutine TIP
     $(LU,N,NDT,LABEL,LAB1,W,APD,A)
C
C     Rudolf Loeser, 1987 Mar 21
C---- Prints, for QUARRY.
C     (This is version 2 of TIP.)
C     !DASH
      save
C     !DASH
      real*8 A, APD, W
      integer I, IE, IS, J, LU, N, NDT
      character LAB1*3, LABEL*(*)
C     !DASH
      external  LINER, HI, BYE
      intrinsic min
C
C               W(NDT), APD(NDT), A(N,NDT)
      dimension W(*),   APD(*),   A(N,*)
C
      call HI ('TIP')
C     !BEG
      if(LU.gt.0) then
        IE = 0
  100   continue
          IS = IE+1
          IE = min(IE+10,N)
C
          call LINER (2,LU)
          write (LU,101) LABEL,LAB1,(I,I=IS,IE)
  101     format(' ',27X,A//
     $           ' ',8X,A3,8X,'APD',3X,10I10)
          call LINER (1,LU)
C
          do 103 J = 1,NDT
            write (LU,102) W(J),APD(J),(A(I,J),I=IS,IE)
  102       format(' ',1P2E11.3,3X,10E10.2)
  103     continue
C
        if(IE.lt.N) goto 100
      end if
C     !END
      call BYE ('TIP')
C
      return
      end
