      subroutine GUYOT
     $(F,N,EDIT, M)
C     Rudolf Loeser, 1997 Mar 28
C---- Edits, for SDERIV2.
C
C     The table of F values may have a "flat tail."
C     If the tail is long enough, GUYOT will identify it, and
C     set M so that it is the index of the first point of the tail.
C     Thus, GUYOT sets M = N initially, and then tries to make
C     M as small as allowable. Example:
C
C     F = 6 5 4 4 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2;
C     here N = 23, and GUYOT should set M = 6.
C     !DASH
      save
C     !DASH
      real*8 CRITE, CRITZ, F, TEN, VAL, ZERO
      integer I, IRET, M, N
      logical EDIT
C     !DASH
      external  ARRSUM, COMPD
      intrinsic abs
C
      dimension F(N)
C
      data ZERO, TEN, CRITZ, CRITE /0.D0, 1.D0, 1.D-14, 1.D-12/
C
C     !BEG
      M = N
C
      if(EDIT.and.(N.gt.10)) then
        call ARRSUM  (F(N-9),10,VAL)
        VAL = VAL/TEN
        if(abs(VAL).lt.CRITZ) then
          VAL = ZERO
        end if
C
        call COMPD   (F(M  ),VAL,CRITE,IRET)
        if(IRET.ne.0) goto 101
C
        do 100 I=1,N
          call COMPD (F(M-1),VAL,CRITE,IRET)
          if(IRET.ne.0) then
            goto 101
          end if
          M = M-1
  100   continue
C
  101   continue
      end if
C     !END
C
      return
      end
