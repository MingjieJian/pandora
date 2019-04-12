      subroutine ADDELI
     $(X,WVLO,WVL,WVHI,CRIT,DUMP,WA,K,YES)
C
C     Rudolf Loeser, 2004 Jul 20
C---- Appends a set of potential additional wavelength values,
C     needed to capture the He-II background lines,
C     to the current provisional WA table.
C
C     Returns YES = true if any values were appended
C     (with K increased accodingly).
C
C     WVLO and WVHI (Angstroms) define the wavelength range spanned by
C     the current basic XI table.
C     !DASH
      save
C     !DASH
      real*8 CRIT, DELTA, TAB, WA, WVHI, WVL, WVLO, X
      integer I, IFLG, J, K, LUEO, M
      logical DUMP, OK, YES
C     !COM
C---- FARGO       as of 2004 Jul 19
      parameter   (MHEL=7, LHEL=7)
      integer     MHEL, LHEL, IUHE, ILHE, LDLHE
      real*8      HEMAS, HESKE, HEWVL, HEWLO, HEWHI, HENUU, HENUL, HEAUL
      real*8      HEPU,  HEPL,  HEDDL, HECDL, HECRD, HECVW, HECSK
      dimension   HEWVL(MHEL), HEWLO(MHEL), HEWHI(MHEL), HENUU(MHEL),
     $            HENUL(MHEL), HEPU(MHEL),  HEPL(MHEL),  HEAUL(MHEL),
     $            IUHE(MHEL),  ILHE(MHEL),  LDLHE(MHEL)
      dimension   HEDDL(LHEL,MHEL), HECDL(LHEL,MHEL),
     $            HECRD(LHEL,MHEL), HECVW(LHEL,MHEL), HECSK(LHEL,MHEL)
      common      /FARGO0/ HEMAS,HESKE
      common      /FARGO1/ HEWVL,HEWLO,HEWHI
      common      /FARGO2/ HENUU,HENUL,HEPU,HEPL
      common      /FARGO3/ HEAUL,HEDDL,HECDL,HECRD,HECVW,HECSK
      common      /FARGO4/ IUHE,ILHE,LDLHE
C     Data for Helium-II lines in the background.
C     .
C---- NOTHER      as of 2004 Jun 25
      integer     NIAUGM
      parameter   (NIAUGM=3000)
C     (Be sure to recompile all users when changing NIAUGM ! )
C     Upper limit for total profile data points for coincident
C     background lines.
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external COMPD, WITHIN, LAPEL, HAWERA, HI, BYE
C
      dimension X(*)
C
C               WA(KM)
      dimension WA(*)
C
      dimension TAB(NIAUGM)
C
      data DELTA /1.D-12/
C
      call HI ('ADDELI')
C     !BEG
      YES = .false.
C
      M = 0
      do 101 J = 1,MHEL
        call COMPD      (WVL, HEWVL(J), DELTA, IFLG)
        if(IFLG.ne.0) then
          call WITHIN   (WVLO, HEWVL(J), WVHI, 0, OK)
C
          if(DUMP) then
            write (LUEO,100) J,WVLO,HEWVL(J),WVHI,OK
  100       format(' ','ADDELI:   J =',I3,', WVLO =',1PE14.7,
     $                 ', WVL(J) =',E14.7,', WVHI =',E14.7,'; OK:',L5)
          end if
C
          if(OK) then
            call HAWERA (X, J, J, NIAUGM, TAB, M)
          end if
        end if
  101 continue
C
      YES = M.gt.0
      if(YES) then
        do 102 I = 1,M
          call LAPEL  (WVL, CRIT, TAB(I), WA, K)
  102   continue
      end if
C     !END
      call BYE ('ADDELI')
C
      return
      end
