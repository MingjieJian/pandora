      subroutine OMAR
     $(NO,N,NL,A,LAB,PRNTZ)
C
C     Rudolf Loeser, 1980 Mar 12
C---- Output routine for "two-dimensioned" arrays.
C     LAB can be up to 6 characters long.
C     Zeroes will be printed or not, depending on PRNTZ.
C     See also ROMA.
C     (This is version 2 of OMAR.)
C     !DASH
      save
C     !DASH
      real*8 A
      integer I, IE, INC, IS, J, KK, N, NL, NO
      logical NONE, PRNTZ, ZA
      character BLANKS*6, LAB*(*), LABEL*5, TIT*9
C     !DASH
      external  NAUGHTD, LINER, HI, BYE
      intrinsic min
C
C               A(N,NL)
      dimension A(N,*)
C
      data BLANKS /'      '/
C     !EJECT
C
      call HI ('OMAR')
C     !BEG
      if(NO.gt.0) then
        LABEL = LAB//BLANKS
        NONE  = LABEL.eq.BLANKS
C
        IE = 0
  100   continue
          IS  = IE+1
          IE  = min(IE+9,N)
          INC = IE-IS
C
          call LINER       (2, NO)
          write (NO,101) (I,I=IS,IE)
  101     format(' ',4X,'Depth',' ',9I13)
          call LINER       (1, NO)
C
          KK = 0
          do 105 J = 1,NL
C
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
C
            if(PRNTZ) then
              ZA = .false.
            else
              call NAUGHTD (A(IS,J), 1, (INC+1), ZA)
            end if
C
            if(.not.ZA) then
              KK = KK+1
              write (NO,104) TIT,(A(I,J),I=IS,IE)
  104         format(' ',A9,' ',1P9E13.5)
            end if
  105     continue
        if(IE.lt.N) goto 100
      end if
C     !END
      call BYE ('OMAR')
C
      return
      end
