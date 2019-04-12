      subroutine BOMBAY
     $(X,F,N,TYPE,LABEL,FOLD,IMG,INDX)
C     Rudolf Loeser, 1998 Oct 16
C---- Prints, for SMOOTH.
C     !DASH
      save
C     !DASH
      real*8 F, FOLD, X
      integer I, IMG, INDX, J, LU, M, N
      logical DUMP
      character BLANK*1, LABEL*(*), LEFT*63, LIBEL*100, RITE*63, TYPE*3
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external MESHED, MASHED, MAMBOY, LINER, IMGPRNT, SHIM, HI, BYE
C
C               X(N), F(N), FOLD(N), IMG(N)
      dimension X(*), F(*), FOLD(*), IMG(*)
C     !EJECT
C
      call HI ('BOMBAY')
C     !BEG
      call MAMBOY     (INDX, LU, DUMP)
      if(LU.gt.0) then
        if(DUMP) then
          call MESHED ('BOMBAY', 2)
        end if
C
        LIBEL = LABEL
        write (LU,100) TYPE,LIBEL
  100   format(' ','SMOOTH (',A,') modified: ',A)
        call IMGPRNT  (LU, IMG, N, 2)
C
        if(DUMP) then
          call LINER  (1, LU)
          write (LEFT,101)
  101     format(24X,'x',3X,'original F(x)',3X,'smoothed F(x)',6X)
          write (LU,102) LEFT,LEFT
  102     format(' ',A63,1X,A63)
          call LINER  (1, LU)
          M = (N/2)+1
          J = M
          do 104 I = 1,M
            write (LEFT,103)   I,X(I),FOLD(I),F(I),IMG(I)
  103       format(I10,1P3E16.8,I5)
            J = J+1
            if(J.le.N) then
              write (RITE,103) J,X(J),FOLD(J),F(J),IMG(J)
            else
              RITE = BLANK
            end if
            write (LU,102) LEFT,RITE
            call SHIM (I, 5, LU)
  104     continue
          call MASHED ('BOMBAY')
        end if
C
      end if
C     !END
      call BYE ('BOMBAY')
C
      return
      end
