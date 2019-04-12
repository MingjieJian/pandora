      subroutine ADDOX3
     $(X,WVLO,WVL,WVHI,CRIT,DUMP,WA,K,YES)
C
C     Rudolf Loeser, 2007 Jan 22
C---- Appends a set of potential additional wavelength values,
C     needed to capture the O-III background lines,
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
C---- WARGO       as of 2007 Jan 18
      parameter   (MX3L=2, LX3L=6)
      integer     MX3L, LX3L, IUX3, ILX3, LDLX3
      real*8      X3MAS, X3SKE, X3WVL, X3WLO, X3WHI, X3NUU, X3NUL, X3AUL
      real*8      X3PU,  X3PL,  X3DDL, X3CDL, X3CRD, X3CVW, X3CSK
      dimension   X3WVL(MX3L), X3WLO(MX3L), X3WHI(MX3L), X3NUU(MX3L),
     $            X3NUL(MX3L), X3PU(MX3L),  X3PL(MX3L),  X3AUL(MX3L),
     $            IUX3(MX3L),  ILX3(MX3L),  LDLX3(MX3L)
      dimension   X3DDL(LX3L,MX3L), X3CDL(LX3L,MX3L),
     $            X3CRD(LX3L,MX3L), X3CVW(LX3L,MX3L), X3CSK(LX3L,MX3L)
      common      /WARGO0/ X3MAS,X3SKE
      common      /WARGO1/ X3WVL,X3WLO,X3WHI
      common      /WARGO2/ X3NUU,X3NUL,X3PU,X3PL
      common      /WARGO3/ X3AUL,X3DDL,X3CDL,X3CRD,X3CVW,X3CSK
      common      /WARGO4/ IUX3,ILX3,LDLX3
C     Data for Oxygen-III lines in the background.
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
      external COMPD, WITHIN, LAPEL, LOUDER, HI, BYE
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
      call HI ('ADDOX3')
C     !BEG
      YES = .false.
C
      M = 0
      do 101 J = 1,MX3L
        call COMPD      (WVL, X3WVL(J), DELTA, IFLG)
        if(IFLG.ne.0) then
          call WITHIN   (WVLO, X3WVL(J), WVHI, 0, OK)
C
          if(DUMP) then
            write (LUEO,100) J,WVLO,X3WVL(J),WVHI,OK
  100       format(' ','ADDOX3:   J =',I3,', WVLO =',1PE14.7,
     $                 ', WVL(J) =',E14.7,', WVHI =',E14.7,'; OK:',L5)
          end if
C
          if(OK) then
            call LOUDER (X, J, J, NIAUGM, TAB, M)
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
      call BYE ('ADDOX3')
C
      return
      end
