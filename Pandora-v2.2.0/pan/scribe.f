      subroutine SCRIBE
     $(A,INDEX,KIJ,JF,JL,N,NL,NO,KODE)
C
C     Rudolf Loeser, 1980 Oct 31
C---- Prints transition arrays.
C
C     "INDEX"  may =   IJ   or   UL   or   NT .
C
C---- In case of INDEX = 'NT' only,
C     KODE tells which types of transitions to print:
C     KODE=1 : radiative transitions only;
C     KODE=2 : passive transitions only; or
C     KODE=3 : both types of transitions.
C     With other values of INDEX, KODE and KIJ are not needed.
C
C     (This is version 2 of SCRIBE.)
C     !DASH
      save
C     !DASH
      real*8 A
      integer I, IE, IL, IN, IS, IU, JF, JL, KIJ, KODE, N, NL, NO
      character BLANK*1, INDEX*2, LAB*3
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  LINER, KATE, HI, BYE
      intrinsic min
C
C               A(N,*), KIJ(NL,NL)
      dimension A(N,*), KIJ(*)
C     !EJECT
C
      call HI ('SCRIBE')
C     !BEG
      if(NO.gt.0) then
        IE = JF-1
  100   continue
          IS  = IE+1
          IE  = min(IE+9,JL)
C
          call LINER    (2, NO)
          write (NO,101) (I,I=IS,IE)
  101     format(' ',5X,'Depth',9I13)
          call LINER    (1, NO)
C
          LAB = 'u,l'
          do 104 IU = 2,NL
            do 103 IL = 1,(IU-1)
              call KATE (INDEX, KODE, KIJ, NL, IU, IL, IN)
              if(IN.gt.0) then
                write (NO,102) LAB,IU,IL,(A(I,IN),I=IS,IE)
  102           format(' ',A3,I3,'/',I2,1X,1P9E13.5)
                LAB = BLANK
              end if
  103       continue
  104     continue
C
        if(IE.lt.JL) goto 100
      end if
C     !END
      call BYE ('SCRIBE')
C
      return
      end
