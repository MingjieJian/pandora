      subroutine ROMA
     $(NO,N,NL,MS,ME,A,LAB,PRNTZ)
C
C     Rudolf Loeser, 1980 Mar 12
C---- Output routine for "two-dimensioned" arrays.
C     LAB can be up to 6 characters long.
C     Zeroes will be printed or not, depending on PRNTZ.
C
C     Special version of OMAR (q.v.): selected J-values only.
C     !DASH
      save
C     !DASH
      real*8 A
      integer I, IC, IE, IS, J, KK, LE, LS, ME, MS, N, NL, NO
      logical NONE, PRNTZ, ZA
      character BLANKS*6, LAB*(*), LABEL*5, TIT*9
C     !DASH
      external  NAUGHTD, LINER, HI, BYE
      intrinsic min, max
C
C               A(N,NL)
      dimension A(N,*)
C
      data BLANKS /'      '/
C
      call HI ('ROMA')
C     !BEG
      if(NO.gt.0) then
        LABEL = LAB//BLANKS
        NONE  = LABEL.eq.BLANKS
C
        LS = max(MS,1)
        LE = min(ME,NL)
        if(LS.gt.LE) then
          LS = 1
          LE = NL
        end if
C     !EJECT
        IE = 0
  100   continue
C
          IS = IE+1
          IE = min(IE+9,N)
          IC = IE-IS+1
          call LINER       (2,NO)
          write (NO,101) (I,I=IS,IE)
  101     format(' ',4X,'Depth',' ',9I13)
          call LINER       (1,NO)
C
          KK = 0
          do 105 J = LS,LE
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
              call NAUGHTD (A(IS,J),1,IC,ZA)
            end if
C
            if(.not.ZA) then
              write (NO,104) TIT,(A(I,J),I=IS,IE)
  104         format(' ',A9,' ',1P9E13.5)
              KK = KK+1
            end if
  105     continue
C
        if(IE.lt.N) goto 100
      end if
C     !END
      call BYE ('ROMA')
C
      return
      end
