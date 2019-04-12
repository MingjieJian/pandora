      subroutine STENO
     $(NO,N,NL,RATES)
C
C     Rudolf Loeser, 1982 Jun 25
C---- Prints CIJ or PIJ.
C     (This is version 3 of STENO.)
C     !DASH
      save
C     !DASH
      real*8 RATES
      integer I, IJ, J, K, KE, KS, N, NL, NO
      character BLANK*1, LAB*3
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  LINER, INDXIJ, HI, BYE
      intrinsic min
C
C               RATES(N,NL**2)
      dimension RATES(N,*)
C
      call HI ('STENO')
C     !BEG
      if(NO.gt.0) then
        KE = 0
  100   continue
          KS = KE+1
          KE = min(KE+9,N)
          call LINER        (2,NO)
          write (NO,101) (K,K=KS,KE)
  101     format(' ',5X,'Depth',9I13)
          call LINER        (1,NO)
C
          LAB = 'i,j'
          do 104 I = 1,NL
            do 103 J = 1,NL
              if(I.ne.J) then
                call INDXIJ (I,J,IJ)
                write (NO,102) LAB,I,J,(RATES(K,IJ),K=KS,KE)
  102           format(' ',A3,I3,',',I2,1X,1P9E13.5)
                LAB = BLANK
              end if
  103       continue
  104     continue
C
        if(KE.lt.N) goto 100
      end if
C     !END
      call BYE ('STENO')
C
      return
      end
