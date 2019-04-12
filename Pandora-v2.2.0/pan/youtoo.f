      subroutine YOUTOO
     $(BDIJ,BDS,JF,JL,N,NL,KIJ,NO,TERM)
C
C---- Prints a complete B-ratios array, from two sources.
C     (This is in effect a special version of MEETOO, using ENDORS.)
C     !DASH
      save
C     !DASH
      real*8 BDIJ, BDS, TERM
      integer I, IE, IL, IS, IU, JF, JL, KIJ, MB, N, NL, NO
      character BLANK*1, FLAG*1, LAB*3, STAR*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(45),STAR  )
C     !DASH
      external  LINER, ENDORS, HI, BYE
      intrinsic min
C
C               TERM(N), BDIJ(N,NL), BDS(N,NT), KIJ(NL,NL)
      dimension TERM(*), BDIJ(*),    BDS(*),    KIJ(*)
C     !EJECT
C
      call HI ('YOUTOO')
C     !BEG
      if(NO.gt.0) then
        IE = JF-1
  100   continue
          IS = IE+1
          IE = min(IE+9,JL)
C
          call LINER      (2, NO)
          write (NO,101) (I,I=IS,IE)
  101     format(' ',5X,'Depth',9I13)
          call LINER      (1, NO)
C
          LAB = 'u/l'
          do 104 IU = 2,NL
            do 103 IL = 1,(IU-1)
              call ENDORS (IU, IL, NL, IS, IE, N, KIJ, BDIJ, BDS,
     $                     TERM, MB)
              if(MB.eq.1) then
                FLAG = STAR
              else
                FLAG = BLANK
              end if
              write (NO,102) LAB,IU,IL,FLAG,(TERM(I),I=IS,IE)
  102         format(' ',A3,I3,'/',I2,A1,1P9E13.5)
              LAB = BLANK
  103       continue
  104     continue
C
        if(IE.lt.JL) goto 100
      end if
C     !END
      call BYE ('YOUTOO')
C
      return
      end
