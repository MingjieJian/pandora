      subroutine LYDIA
     $(NO,ML,LDX,AMASS,SKE,IU,IL,WLO,WVL,WHI,XNUU,PU,XNUL,PL,AUL,
     $ CRD,CVW,CSK,LDL,DDL,CDL,CALLER)
C
C     Rudolf Loeser, 2004 Jun 11
C---- Prints background line ion models.
C     (This is version 4 of LYDIA.)
C     !DASH
      save
C     !DASH
      real*8 AMASS, AUL, CDL, CRD, CSK, CVW, DDL, PL, PU, SKE, WHI, WLO,
     $       WVL, XNUL, XNUU
      integer I, IL, IU, L, LDL, LDX, ML, NO
      character BLANK*1, CALLER*(*)
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external LINER, HI, BYE
C
C               XNUU(ML), XNUL(ML), WLO(ML), WHI(ML), AUL(ML), LDL(ML),
      dimension XNUU(*),  XNUL(*),  WLO(*),  WHI(*),  AUL(*),  LDL(*),
C
C               CRD(LDX,ML), CVW(LDX,ML), CSK(LDX,ML), WVL(ML), PU(ML),
     $          CRD(LDX,*),  CVW(LDX,*),  CSK(LDX,*),  WVL(*),  PU(*),
C
C               CDL(LDX,ML), DDL(LDX,ML), PL(ML), IU(ML), IL(ML)
     $          CDL(LDX,*),  DDL(LDX,*),  PL(*),  IU(*),  IL(*)
C     !EJECT
C
      call HI ('LYDIA')
C     !BEG
      write (NO,100) AMASS,SKE
  100 format(' ','Mass =',F14.6,', SKE =',F10.5,' (Stark exponent)')
      do 107 I = 1,ML
        call LINER (1, NO)
        write (NO,101)
  101   format(' ',9X,'LO cutoff',4X,'HI cutoff',12X,'P(u)',
     $             12X,'P(l)',11X,'NU(u)',11X,'NU(l)',17X,
     $             'Core Wavelength')
        write (NO,102) IU(I),IL(I),WLO(I),WHI(I),PU(I),PL(I),XNUU(I),
     $                 XNUL(I),WVL(I)
  102   format(' ',I2,',',I2,2F13.4,1P4E16.8,E32.16)
        call LINER (1, NO)
        if(LDX.le.1) then
          write (NO,103)
  103     format(' ',41X,'A(u,l)',13X,'CRD',13X,'CVW',13X,'CSK',:,A1,
     $               12X,'DDL',13X,'CDL')
          write (NO,104) AUL(I),CRD(1,I),CVW(1,I),CSK(1,I)
  104     format(' ',31X,1P6E16.8)
        else
          write (NO,103) BLANK
          write (NO,104) AUL(I),CRD(1,I),CVW(1,I),CSK(1,I),DDL(1,I),
     $                          CDL(1,I)
          do 106 L = 2,LDL(I)
            write (NO,105)      CRD(L,I),CVW(L,I),CSK(L,I),DDL(L,I),
     $                          CDL(L,I)
  105       format(' ',47X,1P5E16.8)
  106     continue
        end if
  107 continue
      call LINER   (1, NO)
      write (NO,108) CALLER
  108 format(' ',96X,'(Printed for ',A8,' by LYDIA)')
C     !END
      call BYE ('LYDIA')
C
      return
      end
