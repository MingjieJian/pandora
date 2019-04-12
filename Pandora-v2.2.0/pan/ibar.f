      subroutine IBAR
     $(NO,N,NL,A,Z,LAB)
C
C     Rudolf Loeser, 1980 Mar 12
C---- Output routine for two "two-dimensioned" arrays being compared.
C     LAB can be up to 6 characters long.
C
C     This is a special version of OMAR.
C     !DASH
      save
C     !DASH
      real*8 A, Z
      integer I, IE, IS, J, KK, N, NL, NN, NO
      logical DOOZ, NONE
      character AA*13, BLANKS*6, LAB*(*), LABEL*5, TIT*9, ZZ*13
C     !DASH
      external  TBAR, LINER, HI, BYE
      intrinsic min
C
C               A(N,NL), Z(N,NL)
      dimension A(N,*),  Z(N,*)
C
      dimension AA(9), ZZ(9)
C
      data BLANKS /'      '/
C     !EJECT
C
      call HI ('IBAR')
C     !BEG
      if(NO.gt.0) then
        LABEL = LAB//BLANKS
        NONE  = LABEL.eq.BLANKS
C
        IE = 0
  100   continue
          IS = IE+1
          IE = min(IE+9,N)
          NN = IE-IS+1
C
          call LINER       (2,NO)
          write (NO,101) (I,I=IS,IE)
  101     format(' ',4X,'Depth',' ',9I13)
          call LINER       (1,NO)
C
          KK = 0
          do 105 J = 1,NL
            if(KK.eq.0) then
              if((NL.eq.1).or.NONE) then
                TIT = BLANKS
              else
                write (TIT,102) LABEL,J
  102           format(A6,I3)
              end if
            else
              if(NONE) then
                TIT = BLANKS
              else
                write (TIT,103) J
  103           format(6X,I3)
              end if
            end if
            KK = KK+1
C
            call TBAR    (A(IS,J), Z(IS,J), NN, AA, ZZ, DOOZ)
            write (NO,104) TIT,AA
  104       format(' ',A9,' ',9A13)
            if(DOOZ) then
              write (NO,104) BLANKS,ZZ
              call LINER (1,NO)
            end if
  105     continue
        if(IE.lt.N) goto 100
      end if
C     !END
      call BYE ('IBAR')
C
      return
      end
