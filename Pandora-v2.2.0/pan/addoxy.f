      subroutine ADDOXY
     $(X,WVLO,WVL,WVHI,CRIT,DUMP,WA,K,YES)
C
C     Rudolf Loeser, 2004 Apr 22
C---- Appends a set of potential additional wavelength values,
C     needed to capture the O-I background lines,
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
C---- FURGO       as of 2004 Jun 11
      parameter   (MOXL=11)
      integer     MOXL, IUOX, ILOX
      real*8      OXMAS, OXSKE, OXWVL, OXWLO, OXWHI, OXNUU, OXNUL
      real*8      OXPU,  OXPL,  OXAUL, OXCRD, OXCVW, OXCSK
      dimension   OXWVL(MOXL), OXWLO(MOXL), OXWHI(MOXL), OXNUU(MOXL),
     $            OXNUL(MOXL), OXPU(MOXL),  OXPL(MOXL),  OXAUL(MOXL),
     $            OXCRD(MOXL), OXCVW(MOXL), OXCSK(MOXL),
     $            IUOX(MOXL),  ILOX(MOXL)
      common      /FURGO0/ OXMAS,OXSKE
      common      /FURGO1/ OXWVL,OXWLO,OXWHI
      common      /FURGO2/ OXNUU,OXNUL,OXPU,OXPL
      common      /FURGO3/ OXAUL,OXCRD,OXCVW,OXCSK
      common      /FURGO4/ IUOX,ILOX
C     Data for Oxygen-I lines in the background.
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
      external COMPD, WITHIN, LAPEL, COMBAT, HI, BYE
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
      call HI ('ADDOXY')
C     !BEG
      YES = .false.
C
      M = 0
      do 101 J = 1,MOXL
        call COMPD (WVL, OXWVL(J), DELTA, IFLG)
        if(IFLG.ne.0) then
          call WITHIN   (WVLO, OXWVL(J), WVHI, 0, OK)
C
          if(DUMP) then
            write (LUEO,100) J,WVLO,OXWVL(J),WVHI,OK
  100       format(' ','ADDOXY:   J =',I3,', WVLO =',1PE14.7,
     $                 ', WVL(J) =',E14.7,', WVHI =',E14.7,'; OK:',L5)
          end if
C
          if(OK) then
            call COMBAT (X, J, J, NIAUGM, TAB, M)
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
      call BYE ('ADDOXY')
C
      return
      end
