      subroutine GRAPE
     $(LU,TIT,N,IU,IL,METSE,MM,METNAM,KNT,A)
C
C     Rudolf Loeser, 1984 Oct 26
C---- Prints comparisons, for TIDY.
C     (This is version 2 of GRAPE.)
C     !DASH
      save
C     !DASH
      real*8 A, T
      integer I, IE, IL, IS, IU, J, KNT, LU, MET, METSE, MM, MSE, MT, N
      character METNAM*15, TIT*2
C     !DASH
      external  LINER, SCALP, HI, BYE
      intrinsic min
C
C               A(N,KNT), MM(KNT), METNAM(KNT)
      dimension A(N,*),   MM(*),   METNAM(*)
C
      dimension T(16)
C
      call HI ('GRAPE')
C     !BEG
      if(LU.gt.0) then
        MSE = METSE+1
        call LINER   (3, LU)
        write (LU,100) TIT,METNAM(MSE)
  100   format(' ',6('************'),'**'/
     $         ' ','Comparison of values of ',A2,' computed by ',
     $             'different methods; method selected: ',A)
C
        call LINER   (1, LU)
        write (LU,101)
  101   format(' ',15X,'Absolute values')
C
        IE = 0
  102   continue
          IS = IE+1
          IE = min((IE+8),N)
          call LINER (2, LU)
          write (LU,103) (I,I=IS,IE)
  103     format(' ',10X,'Depth',8I14)
          call LINER (1, LU)
C
          do 105 J = 1,KNT
            MET = MM(J)+1
            write (LU,104) METNAM(MET),(A(I,MET),I=IS,IE)
  104       format(' ',A15,1P8E14.6)
  105     continue
C
        if(IE.lt.N) goto 102
C     !EJECT
        call LINER         (2, LU)
        write (LU,106) METNAM(MSE)
  106   format(' ',15X,'Values relative to those of ',A)
C
        IE = 0
  107   continue
          IS = IE+1
          IE = min((IE+16),N)
          call LINER       (2, LU)
          write (LU,108) (I,I=IS,IE)
  108     format(' ',10X,'Depth',16I7)
          call LINER       (1, LU)
C
          do 111 J = 1,KNT
            MET = MM(J)+1
            if(MET.ne.MSE) then
              MT = 0
              do 109 I = IS,IE
                MT = MT+1
                call SCALP (A(I,MET), A(I,MSE), T(MT))
  109         continue
              write (LU,110) METNAM(MET),(T(I),I=1,MT)
  110         format(' ',A15,16F7.3)
            end if
  111     continue
C
        if(IE.lt.N) goto 107
C
      end if
C     !END
      call BYE ('GRAPE')
C
      return
      end
