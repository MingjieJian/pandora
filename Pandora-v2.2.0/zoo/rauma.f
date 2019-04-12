      subroutine RAUMA
     $(LU,A,N,M,L)
C
C     Rudolf Loeser, 2002 Dec 17
C---- Basic routine for printing compact arrays of size N x M.
C     L = 1 means: single precision,  4 significant figures;
C     L = 2 means: double precision, 16 significant figures.
C     !DASH
      save
C     !DASH
      real*8 A
      integer I, IQ, J, JE, JS, L, LN, LO, LU, M, N
      logical DOIT
      character BLANK*1, COL*8, FORMD*30, FORMI*25, INDX*12, LINE*127,
     $          PINE*127
C     !DASH
      external  CAVIR, LINER, AMURA, RIVAC
      intrinsic min
C
      dimension A(N,M), IQ(2), FORMD(2), FORMI(2)
C
      data BLANK,COL,IQ /' ', 'column =',  10, 5/
      data FORMD /'(A12,3X,1P5E11.3,2X,5E11.3)', '(A12,1P5E23.15)'/
      data FORMI /'(8X,A8,5I11,2X,5I11)',        '(5X,A8,5I23)'/
C
C     !BEG
C---- Check whether to print in full
      call CAVIR       (LU,A,(N*M),L,DOIT)
      if(DOIT) then
        LINE = BLANK
        LO = 0
C
        JE = 0
  100   continue
          JS = JE+1
          JE = min((JE+IQ(L)),M)
C----     Write columns heading
          call LINER   (1,LU)
          write (LU,FORMI(L)) COL,(J,J=JS,JE)
C----     Loop over all rows
          LN = 0
          do 101 I = 1,N
C----       Encode row index
            call AMURA (I,INDX)
C----       Save previous line, and emcode new one
            PINE = LINE
            write (LINE,FORMD(L)) INDX,(A(I,J),J=JS,JE)
C----       Print line, or keep track of omitted identical lines
            call RIVAC (LU,(I.lt.N),LINE,PINE,LO,LN)
  101     continue
C
        if(JE.lt.M) goto 100
      end if
C     !END
C
      return
      end
