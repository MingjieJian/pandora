      subroutine CYAN
     $(QNAME)
C
C     Rudolf Loeser, 1988 Apr 29
C---- Reads Default Ionization Data Table changes, for POMP.
C     QNAME may be 'POPION', 'POPXLM', or 'POPRCP'.
C     (This is version 4 of CYAN.)
C     !DASH
      save
C     !DASH
      real*8 dummy
      integer J, KERR, KIND, LUEO, MODE, jummy
      logical KILROY
      character QNAME*8, qummy*8
C     !COM
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
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external KIWI, BASIL, GLINT, CILENTO, CHLOE, CARMEN, MUSHED,
     $         MASHED, ABORT, HI, BYE
C
      call HI ('CYAN')
C     !BEG
      KILROY = .true.
      KERR   = 0
      call KIWI        (MODE, dummy, KIND, qummy, jummy)
      if(MODE.ne.3) goto 203
C
      if((KIND.lt.1).or.(KIND.gt.NPION)) then
        call MUSHED    ('CYAN', 1, KILROY)
        write (LUEO,100) 'ion index is bad'
  100   format(' ','Trouble reading Ionization Data: ',A)
        goto 102
      end if
      call KIWI        (MODE, dummy, J, qummy, jummy)
      if(MODE.ne.3) goto 203
      if(QNAME(1:6).eq.'POPION') then
        if(J.eq.1) then
          call BASIL   (XLMTHR(1,KIND), LIMDAT(KIND), QNAME)
        else if(J.eq.2) then
          call BASIL   (CCPLEV(1,KIND), LIMDAT(KIND), QNAME)
        else if(J.eq.3) then
          call CILENTO (NPTABL(1,KIND), LIMDAT(KIND), QNAME)
        else if(J.eq.4) then
          call BASIL   (SCPLEV(1,KIND), LIMDAT(KIND), QNAME)
        else if(J.eq.5) then
          call BASIL   (PILEVL(1,KIND), LIMDAT(KIND), QNAME)
        else if(J.eq.6) then
          call GLINT   (LLABEL(1,KIND), LIMDAT(KIND), QNAME)
        else
          call MUSHED  ('CYAN', 1, KILROY)
          write (LUEO,100) 'table code is bad'
          goto 102
        end if
      else
        if((J.lt.1).or.(J.gt.LIMDAT(KIND))) then
          call MUSHED  ('CYAN', 1, KILROY)
          write (LUEO,100) 'level index is bad'
          goto 102
        end if
        if(QNAME(1:6).eq.'POPXLM') then
          call BASIL   (XLMTAB(1,J,KIND), NPTABL(J,KIND), QNAME)
        else if(QNAME(1:6).eq.'POPRCP') then
          call BASIL   (RCPTAB(1,J,KIND), NPTABL(J,KIND), QNAME)
        else
          call MUSHED  ('CYAN', 1, KILROY)
          write (LUEO,101) QNAME(1:6)
  101     format(' ','Trouble reading Ionization Data: the name ',A6,
     $               ' is unknown')
          goto 102
        end if
      end if
      goto 199
C     !EJECT
C---- Error message
  203 KERR = KERR+1
  202 KERR = KERR+1
  201 KERR = KERR+1
      call MUSHED   ('CYAN', 1, KILROY)
      write (LUEO,200)
  200 format(' ','Error reading Ionization Data.')
      call CHLOE    (LUEO, QNAME, KERR)
C
  102 continue
      call ABORT
      call CARMEN
C
C---- Go home
  199 continue
C
      if(.not.KILROY) then
        call MASHED ('CYAN')
      end if
C     !END
      call BYE ('CYAN')
C
      return
      end
