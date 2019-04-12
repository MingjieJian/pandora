      subroutine DRIP1
     $(N,NO,IETA,U,F1,SP,EP1,EP2,D,XLF,XLP,RNDT,IQINC,METEP,KN,NCR)
C
C     Rudolf Loeser, 1975 Jan 02
C---- Produces part of the HAWSER printout.
C     !DASH
      save
C     !DASH
      real*8 D, EP1, EP2, F1, RNDT, SP, U, XLF, XLP
      integer I, IETA, IKEY, IQINC, KN, METEP, N, NCR, NO
      character BLANK*1, BLANKS*10, MARC*1, STAR*1, TATS*12, TIT*10,
     $          TITS*10
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
      external  SETC, MOVEC, MARKI, SHIM, ABJECT, LINER, HI, BYE
      intrinsic max
C
C               U(N), D(N), SP(N,KKX), EP1(N), EP2(N), RNDT(N), XLF(N),
      dimension U(*), D(*), SP(N,*),   EP1(*), EP2(*), RNDT(*), XLF(*),
C
C               XLP(N), F1(N)
     $          XLP(*), F1(*)
C
      dimension TIT(4), TITS(4), TATS(4)
C
      data TITS /' Incident ', 'Radiation ', '   term   ', '   RNDT   '/
      data TATS /'--- Nova ---', ' Complex-U -', ' Complex-L -',
     $           '-- Chain ---'/
      data BLANKS /'          '/
C
      call HI ('DRIP1')
C     !BEG
      if(NO.gt.0) then
        IKEY = max(IQINC,NCR)
        if(IKEY.le.0) then
          call SETC  (TIT,  1, 4, BLANKS)
        else
          call MOVEC (TITS, 1, 4, TIT, 1, 4)
        end if
C     !EJECT
        call ABJECT  (NO)
        write (NO,100) TIT(1),TIT(2),TIT(3),KN,TIT(4)
  100   format(' ',114X,A10/
     $         ' ',114X,A10/
     $         ' ',5X,'(h*nu)/(k*T)',10X,'Background Source Function',
     $             4X,'--- Epsilons ---',7X,'Delta',29X,A10/
     $         ' ',10X,'U',10X,'F1',10X,'SP(1)',4X,'SP(',I7,')',
     $             5X,'EP1',8X,'EP2',10X,'D',11X,'LF',10X,'LP',6X,A10)
        call LINER   (1, NO)
C
        do 103 I = 1,N
          call MARKI (I, IETA, MARC, STAR, BLANK)
          if(I.le.IETA) then
            if(IKEY.gt.0) then
              write (NO,101) I,MARC,U(I),F1(I),SP(I,1),SP(I,KN),EP1(I),
     $                       EP2(I),D(I),XLF(I),XLP(I),RNDT(I)
  101         format(' ',I3,A1,1P10E12.4)
            else
              write (NO,101) I,MARC,U(I),F1(I),SP(I,1),SP(I,KN),EP1(I),
     $                       EP2(I),D(I),XLF(I),XLP(I)
            end if
          else
            if(IKEY.gt.0) then
              write (NO,102) I,MARC,U(I),F1(I),SP(I,1),SP(I,KN),EP1(I),
     $                       EP2(I),D(I),XLP(I),RNDT(I)
  102         format(' ',I3,A1,1P7E12.4,12X,2E12.4)
            else
              write (NO,102) I,MARC,U(I),F1(I),SP(I,1),SP(I,KN),EP1(I),
     $                       EP2(I),D(I),XLP(I)
            end if
          end if
          call SHIM  (I, 5, NO)
  103   continue
C
        call LINER   (1, NO)
        write (NO,104) TATS(METEP+1)
  104   format(' ',54X,'-----',A12,'-----')
      end if
C     !END
      call BYE ('DRIP1')
C
      return
      end
