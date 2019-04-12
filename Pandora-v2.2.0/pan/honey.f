      subroutine HONEY
     $(IPOP,H,LIMD,XLM)
C
C     Rudolf Loeser, 1988 Apr 28
C---- Computes "H" -- ion cross-sections -- for
C     the "population updata ion" # IPOP,
C     where IPOP > 1 (i.e. NOT for Hydrogen).
C     (This is version 2 of HONEY.)
C     !DASH
      save
C     !DASH
      real*8 H, RAT, RCP, XLM, ZERO
      integer ILEV, IPOP, LIMD, MODE, NP
      logical OUT
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
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external HALT, ESKER, HENBIT, HI, BYE
C
C               H(LIMD)
      dimension H(*)
C
      data MODE /0/
C
      call HI ('HONEY')
C     !BEG
      if((IPOP.lt.2).or.(IPOP.gt.NPION)) then
        write (MSSLIN(1),100) IPOP,NPION
  100   format('IPOP =',I12,', which is not between 1 and',I4,
     $         ', inclusive.')
        call HALT       ('HONEY', 1)
      end if
C
      LIMD = LIMDAT(IPOP)
      do 102 ILEV = 1,LIMD
        if(SCPLEV(ILEV,IPOP).ne.ZERO) then
          call ESKER    (XLMTHR(ILEV,IPOP), XLM, MODE, OUT)
          if(OUT) then
            H(ILEV) = ZERO
          else
            RAT = XLM/XLMTHR(ILEV,IPOP)
            RCP = RAT**SCPLEV(ILEV,IPOP)
C
            H(ILEV) = CCPLEV(ILEV,IPOP)*RCP
          end if
        else
          NP = NPTABL(ILEV,IPOP)
          if(NP.lt.2) then
            write (MSSLIN(1),101) IPOP,ILEV,NP
  101       format('Population ion #',I3,5X,'Level #',I3,5X,
     $             'RCP table length =',I3,'  is bad.')
            call HALT   ('HONEY', 1)
          end if
          call ESKER    (XLMTAB(NP,ILEV,IPOP), XLM, MODE, OUT)
          if(OUT) then
            H(ILEV) = ZERO
          else
            call HENBIT (XLMTAB(1,ILEV,IPOP), RCPTAB(1,ILEV,IPOP), NP,
     $                   XLM, RCP)
            H(ILEV) = CCPLEV(ILEV,IPOP)*RCP
          end if
        end if
  102 continue
C     !END
      call BYE ('HONEY')
C
      return
      end
