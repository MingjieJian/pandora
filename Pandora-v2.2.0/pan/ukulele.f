      subroutine UKULELE
     $(NO,JLEV,IRLS,DORS,DORK,KSHL,KOOL,PTRF,DETL)
C
C     Rudolf Loeser, 1980 Mar 11
C---- Prints header, for HULA.
C     (This is version 2 of UKULELE.)
C     !DASH
      save
C     !DASH
      integer I, IRLS, JLEV, NO
      logical DETL, DORK, DORS, KOOL, KSHL, PFAB, PORK, PTRF, QTRF,
     $        REGL
      character BLANK*1, TIT*10, TRL*3
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external ABJECT, LINER, HI, BYE
C
      dimension TRL(2)
C
      data TRL /'RLA', 'RLB'/
C
      call HI ('UKULELE')
C     !BEG
      if(KSHL) then
        TIT = ' K  Shell '
      else
        write (TIT,100) JLEV
  100   format(' Level',I3,' ')
      end if
      REGL = .not.KSHL
      PFAB = DETL.and.REGL
      QTRF = DORK.and.PTRF
      PORK = DORK.or.KSHL
C     !EJECT
      call ABJECT (NO)
      write (NO,101) (TIT,I=1,4)
  101 format(' ',4('**********',A10,'***********'))
      call LINER  (2, NO)
C
      if(DORS) then
                 write (NO,102)
  102            format(' ',32X,'TR - Radiation Temperature',
     $                          T111,'Option  RATEALL'/
     $                  ' ',32X,'J  - Mean Intensity',
     $                          T111,'Option  RATEALL')
        if(PFAB) write (NO,103)
  103            format(' ',32X,'FA - Intermediate for RLA',
     $                          T111,'Option  RATEALL'/
     $                  ' ',32X,'FB - Intermediate for RLB',
     $                          T111,'Option  RATEALL')
        if(QTRF) write (NO,104)
  104            format(' ',32X,'TRK- Effective Radiation Temperature')
        if(PORK) write (NO,105)
  105            format(' ',32X,'RK - Photoionization Rate')
        if(REGL) write (NO,106) TRL(IRLS)
  106            format(' ',32X,'RL - Photorecombination Rate: ',A3,
     $                          T111,'Option RATEFULL')
      end if
C
      if(REGL)   write (NO,107)
  107            format(' ',32X,'CK - Collisional Ionization Rate')
C
      if(DORS) then
        if(DETL) write (NO,108)
  108            format(' ',32X,'M  - suffix for remainder term',
     $                          T111,'Option RATEFULL')
        if(KOOL) write (NO,109)
  109            format(' ',32X,'C  - suffix for Cooling Rates data',
     $                          T111,'Option RATEFULL')
      end if
C
C     !END
      call BYE ('UKULELE')
C
      return
      end
