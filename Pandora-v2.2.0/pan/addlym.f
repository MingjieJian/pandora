      subroutine ADDLYM
     $(X,LIM,WVLO,WVL,WVHI,CRIT,DUMP,WA,K,YES)
C
C     Rudolf Loeser, 2004 Aug 06
C---- Appends a set of potential additional wavelength values,
C     needed to capture the Hydrogen Lyman background lines,
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
      integer I, IFLG, J, K, LIM, LUEO, M
      logical DUMP, OK, YES
C     !COM
C---- FERGO       as of 2004 Aug 05
      parameter   (MHYL=24)
      integer     MHYL, IUHY, ILHY
      real*8      HYWVL, HYWLO, HYWHI, HYNUU, HYNUL
      dimension   HYWVL(MHYL), HYWLO(MHYL), HYWHI(MHYL), HYNUU(MHYL),
     $            HYNUL(MHYL), IUHY(MHYL),  ILHY(MHYL)
      common      /FERGO0/ HYWVL,HYWLO,HYWHI
      common      /FERGO1/ HYNUU,HYNUL
      common      /FERGO2/ IUHY,ILHY
C     Data for Hydrogen Lyman lines in the background.
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
      external COMPD, WITHIN, LAPEL, KASLAR, HI, BYE
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
      call HI ('ADDLYM')
C     !BEG
      YES = .false.
C
      M = 0
      do 101 J = 1,LIM
        call COMPD      (WVL, HYWVL(J), DELTA, IFLG)
        if(IFLG.ne.0) then
          call WITHIN   (WVLO, HYWVL(J), WVHI, 0, OK)
C
          if(DUMP) then
            write (LUEO,100) J,WVLO,HYWVL(J),WVHI,OK
  100       format(' ','ADDLYM:   J =',I3,', WVLO =',1PE14.7,
     $                 ', WVL(J) =',E14.7,', WVHI =',E14.7,'; OK:',L5)
          end if
C
          if(OK) then
            call KASLAR (X, J, J, NIAUGM, TAB, M)
          end if
        end if
  101 continue
C
      YES = M.gt.0
      if(YES) then
        do 102 I = 1,M
          call LAPEL    (WVL, CRIT, TAB(I), WA, K)
  102   continue
      end if
C     !END
      call BYE ('ADDLYM')
C
      return
      end
