      subroutine EGRET
     $(LINE,NO)
C
C     Rudolf Loeser, 1983 Mar 21
C---- Prints the ELEMENT table, with abundances.
C     (This is version 3 of EGRET.)
C     !DASH
      save
C     !DASH
      real*8 DELTA
      integer I, IATNO, IFLG, IN, IND, INDEX, J, LIM, LUM, MUM, NO, NUM
      logical REFR
      character BLANK*1, ELNAM*12, EQUAL*1, LINE*120, MET*1, REF*3,
     $          SIG*1, STAR*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(44),EQUAL )
      equivalence (SYMBS(45),STAR  )
C
C---- ELEMENT     as of 1998 Aug 17
      integer     NELX
      parameter   (NELX=50)
C     (Remember to recompile all users when changing NELX)
      real*8      ELABD,ELCHI,ELLU1,ELLU2,ELABL,ELDEF
      integer     LATNO,LDEFR,NMT,NMTMAX
      logical     LATEM
      character   ELSYM*3, ELSUB*3
      dimension   ELSYM(NELX),ELSUB(NELX),ELABD(NELX),ELCHI(NELX),
     $            ELLU1(NELX),ELLU2(NELX),ELABL(NELX),ELDEF(NELX),
     $            LATNO(NELX),LDEFR(NELX),LATEM(NELX)
C
      common      /ELEMNT0/ NMT,NMTMAX
      common      /ELEMNT1/ ELSYM,ELSUB
      common      /ELEMNT2/ ELABD,ELCHI,ELLU1,ELLU2,ELABL,ELDEF
      common      /ELEMNT3/ LATNO,LDEFR
      common      /ELEMNT4/ LATEM
C
C     Element data tables:
C             ELSYM - element symbol;
C             ELSUB - (Scratch storage for I.D. symbols);
C             ELABD - abundance (w.r.t. Hydrogen);
C             ELCHI - Chi, i.e. ionization potential;
C             ELLU1 - U-I partition function;
C             ELLU2 - U-II partition function;
C             ELABL - logarithmic abundance;
C             ELDEF - defaults values of logarithmic abundance;
C             LATNO - atomic number; and
C             LDEFR - default values sources codes.
C             LATEM - "metal" designator
C     .
C     !EJECT
C---- ELEGANT     as of 2002 Mar 26
      integer     LNAM,LABR,NELE
      character   ENAMES*12,EABBRS*2
      dimension   LNAM(92),LABR(92),ENAMES(92),EABBRS(92)
      common      /ELEGNT1/ NELE,LNAM,LABR
      common      /ELEGNT2/ ENAMES,EABBRS
C     "ENAMES" is a list of element names, and "LNAM" specifies
C     the number of characters in each name.
C     "EABBRS" is the corresponding list of element symbols, and
C     "LABR" specifies the number of characters in each symbol.
C     .
C     !DASH
      external  LINER, WODEN, JAVELIN, COMPD, HI, BYE
      intrinsic mod
C
      dimension LUM(2), INDEX(NELX)
C
      data DELTA /1.D-6/
C
      call HI ('EGRET')
C     !BEG
      if(NO.gt.0) then
        write (NO,100)
  100   format(' ',2('  Atomic',19X,'----- A B U N D A N C E -----',4X)/
     $         ' ',2('  number',19X,'  Ratio      Log     Default ',4X))
        call LINER   (1, NO)
C
        REFR = .false.
        call WODEN   (INDEX, LIM)
        LINE = BLANK
        J  = -60
        IN = 1
        NUM = 1
        MUM = NUM
        call JAVELIN (LIM, LUM)
C     !EJECT
        do 104 I = 1,LIM
          if(MUM.le.LIM) then
            J = J+60
            IND = INDEX(MUM)
            IATNO = LATNO(IND)
            ELNAM = ENAMES(IATNO)
            call COMPD (ELABL(IND), ELDEF(IND), DELTA, IFLG)
            SIG = EQUAL
            if(IFLG.ne.0) then
              SIG = BLANK
            end if
            REF = BLANK
            if(LDEFR(IND).gt.0) then
              write (REF,101) LDEFR(IND)
  101         format('[',I1,']')
              REFR = .true.
            end if
            MET = BLANK
            if(LATEM(IND)) then
              MET = STAR
            end if
            write (LINE(J+1:J+60),102) IATNO,ELNAM,MET,ELSYM(IND),
     $             ELABD(IND),ELABL(IND),SIG,ELDEF(IND),REF
  102       format(I6,3X,A12,A1,A3,1PE11.3,0PF8.2,1X,A1,F6.2,1X,A3,4X)
            MUM = MUM+LUM(IN)
            IN  = IN+1
            if(mod(I,2).eq.0) then
              write (NO,103) LINE
  103         format(' ',A120)
              LINE = BLANK
              J  = -60
              IN = 1
              NUM = NUM+1
              MUM = NUM
            end if
          end if
  104   continue
C
        if(J.ge.0) then
          write (NO, 103) LINE
        end if
C     !EJECT
        if(REFR) then
          call LINER (2, NO)
          write (NO,105)
  105     format(' ','Sources of default abundance values:'/
     $           ' ','[1]  Grevesse, N., and Sauval, A. J. 2000, in ',
     $               'O. Manuel, ed., Origin of Elements in the ',
     $               'Solar System (Kluwer).'/
     $           ' ','[2]  Asplund, M., Grevesse, N., Sauval, A.J., ',
     $               'Allende Prieto, C., and Kiselmanm D. 2004, ',
     $               'A & A, 417, 751-768.'/
     $           ' ','[3]  Asplund, M., Grevesse, N., and Sauval ',
     $               'A. J., 2005, in T. G. Barnes and F. N. Bash, ',
     $               'eds.,'/
     $           ' ','     Cosmic Abundances as Records of Stellar ',
     $               'Evolution and Nucleosynthesis, ASP Conf. Ser., ',
     $               'in press.'/
     $           ' ','[4]  Asplund, M., Grevesse, N., Sauval, A.J., ',
     $               'Allende Prieto, C., and Blomme, R. 2005, ',
     $               'A & A, 431, 693.')
          write (NO,106)
  106     format(' ','[5]  Asplund, M., Ann.Rev.Astron.Astrophys. ',
     $               '2005, 43, 481.')
        end if
        call LINER   (1, NO)
        write (NO,107)
  107   format(' ','(Last revised 2006 Sep 21.)')
        call LINER   (1, NO)
      end if
C     !END
      call BYE ('EGRET')
C
      return
      end
