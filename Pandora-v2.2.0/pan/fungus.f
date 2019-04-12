      subroutine FUNGUS
     $(NO,IPOP,TITLE)
C
C     Rudolf Loeser, 1988 Apr 29
C---- Prints ion data, for SERF.
C     (This is version 2 of FUNGUS.)
C     !DASH
      save
C     !DASH
      real*8 ZERO
      integer I, IPOP, J, JE, JS, NO
      logical PRNT
      character TITLE*(*)
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- IONDATA     as of 2007 Jan 12
C     Data tables for the built-in models of "non-LTE" ions; these
C     models are used to compute bound-free absorption and emission.
C     Hydrogen (of course!) is treated as a special case.
      integer     NPION,NIONL,NDPNT
C
      parameter   (NPION=14)
C     NPION     = number of ion models, as follows:
C     01: H      02: C-I    03: Si-I   04: He-I   05: He-II  06: Al-I
C     07: Mg-I   08: Fe-I   09: Na-I   10: Ca-I   11: O-I    12: S-I
C     13: O-II   14: O-III
C
      parameter   (NIONL=15)
C     NIONL     = maximum number of levels in each model
C
      parameter   (NDPNT=150)
C     NDPNT     = maximum length of tables specifying the wavelength-
C     dependence of the absorption in each continuum, by
C     piece-wise linear approximation.
C
C     REMEMBER to recompile all users when changing NPION, NIONL, NDPNT.
C
      real*8      PILEVL,XLMTHR,CCPLEV,SCPLEV,XLMTAB,RCPTAB
      integer     LIMDAT,MAXDATL,LEND,NPTABL
      character   LLABEL*16
      logical     LLPRNT
      dimension   LIMDAT(            NPION), LLPRNT(            NPION),
     $            PILEVL(      NIONL,NPION), XLMTHR(      NIONL,NPION),
     $            NPTABL(      NIONL,NPION), CCPLEV(      NIONL,NPION),
     $            SCPLEV(      NIONL,NPION), LLABEL(      NIONL,NPION),
     $            XLMTAB(NDPNT,NIONL,NPION), RCPTAB(NDPNT,NIONL,NPION)
C
C     LIMDAT    = actual number of levels in each model (LIMDAT should
C                 equal LIMPOP in labelled common POPDATA)
C     MAXDATL   = maximum value of LIMDAT
C     LEND      = sum of LIMDAT
C     LLABEL    = "term designation" of each level of each model
C     LLPRNT    = data tables print switch
C     PILEVL    = statistical weight of each level of each model
C     XLMTHR    = threshhold wavelengths of continua
C     NPTABL    = actual number of data points in absorption data table
C                 of each level of each model (used only when > 0)
C     CCPLEV    = threshhold absorption factors
C     SCPLEV    = exponent of power-law wavelength dependence of
C                 absorption of each level of each model (used only
C                 when > 0; should be > 0 when corresponding NPTABL = 0)
C     XLMTAB    = wavelength values for which RCPTAB is specified
C     RCPTAB    = absorption in the continuum of a level of a model
C                 (At wavelengths < XLMTAB(NPTABL), a power law
C                  with exponent = 3 is used.)
C
      common      /IODAT01/ MAXDATL,LEND
      common      /IODAT02/ LIMDAT
      common      /IODAT03/ LLPRNT
      common      /IODAT04/ PILEVL
      common      /IODAT05/ XLMTHR
      common      /IODAT06/ NPTABL
      common      /IODAT07/ CCPLEV
      common      /IODAT08/ SCPLEV
      common      /IODAT09/ LLABEL
      common      /IODAT10/ XLMTAB
      common      /IODAT11/ RCPTAB
C     .
C     !DASH
      external  QUAKE, DASHER, LINER, HI, BYE
      intrinsic min
C     !EJECT
C
      call HI ('FUNGUS')
C     !BEG
      if((NO.gt.0).and.LLPRNT(IPOP)) then
        call QUAKE         (NO, TITLE, PRNT)
        if(PRNT) then
          LLPRNT(IPOP) = .false.
          write (NO,100)
  100     format(' ',16X,'Term',9X,'Statistical',10X,'Threshhold',
     $               7X,'Coefficient',7X,'Exponent',5X,'Table'/
     $           ' ',5X,'Level',3X,'Designation',10X,'Weight',
     $               10X,'Wavelength',15X,'CCP',12X,'SCP',4X,'Length')
          call LINER       (1, NO)
C
          do 102 I = 1,LIMDAT(IPOP)
            write (NO,101) I,LLABEL(I,IPOP),PILEVL(I,IPOP),
     $                       XLMTHR(I,IPOP),CCPLEV(I,IPOP),
     $                       SCPLEV(I,IPOP),NPTABL(I,IPOP)
  101       format(' ',I10,3X,A16,F11.0,1PE20.7,0PF18.3,F15.3,I10)
  102     continue
          call LINER       (1, NO)
          write (NO,103)
  103     format(' ',69X,'(x 10E-18)')
C
          do 106 I = 1,LIMDAT(IPOP)
            if(SCPLEV(I,IPOP).eq.ZERO) then
              call LINER   (1, NO)
              JE = 0
  104         continue
                JS = JE+1
                JE = min(JE+10,NPTABL(I,IPOP))
                call LINER (1,NO)
                write (NO,105) 'LM ',I,(XLMTAB(J,I,IPOP),J=JS,JE)
  105           format(' ',A3,I3,1X,1P10E12.5)
                write (NO,105) 'RCP',I,(RCPTAB(J,I,IPOP),J=JS,JE)
              if(JE.lt.NPTABL(I,IPOP)) goto 104
            end if
  106     continue
C
          call LINER       (1, NO)
          call DASHER      (NO)
        end if
      end if
C     !END
      call BYE ('FUNGUS')
C
      return
      end
