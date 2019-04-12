      subroutine PROXY
     $(LU,A,M,MDIM,N)
C     Rudolf Loeser, 1998 Jul 20
C---- Prints the two-dimensional array A, with M rows and N columns.
C     Special version (handling of zeros) of PRAY (q.v.).
C     !DASH
      save
C     !DASH
      real*8 A, SET
      integer I, J, KS, LB, LE, LN, LNC, LU, M, MDIM, N
      logical ZERO
      character COL*6, DASH*13, LINE*117
C     !DASH
      external  LINER, MOVED, NAUGHTD, PYRA, SHIM
      intrinsic min
C
C               A(MDIM,N)
      dimension A(MDIM,*)
C
      dimension SET(9)
C
      data      COL, DASH /'Column', '-------------'/
C     !EJECT
C
C     !BEG
      if(LU.gt.0) then
C
        LE = 0
  100   continue
          LB  = LE+1
          LE  = min(LE+9,N)
          LNC = LE-LB+1
C
          call LINER         (1,LU)
          write (LU,101) (DASH,J=LB,LE)
  101     format(' ',10X,9A13)
          write (LU,102) (COL,J,J=LB,LE)
  102     format(' ',10X,9(3X,A6,I4))
          call LINER         (1,LU)
C
          LN = 0
          KS = 0
          I  = 0
  103     continue
            I = I+1
            if(I.le.M) then
C
              if((I.gt.1).and.(I.lt.M)) then
                call NAUGHTD (A(I,LB),MDIM,LNC,ZERO)
                if(ZERO) then
                  KS = KS+1
                  goto 103
                end if
              end if
              if(KS.gt.0) then
                if(KS.eq.1) then
                  I = I-1
                else
                  write (LU,104) KS
  104             format(' ',12X,'[',I5,' rows of zeroes omitted]')
                  LN = LN+1
                  call SHIM  (LN,5,LU)
                end if
              end if
C
              call MOVED     (A(I,LB),MDIM,LNC, SET,1,LNC)
              call PYRA      (SET,LNC,LINE)
              write (LU,105) I,LINE
  105         format(' ',I10,A117)
              KS = 0
              LN = LN+1
              call SHIM      (LN,5,LU)
              goto 103
            end if
        if(LE.lt.N) goto 100
C
      end if
C     !END
C
      return
      end
