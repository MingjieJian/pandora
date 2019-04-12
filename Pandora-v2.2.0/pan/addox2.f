      subroutine ADDOX2
     $(X,WVLO,WVL,WVHI,CRIT,DUMP,WA,K,YES)
C
C     Rudolf Loeser, 2007 Jan 22
C---- Appends a set of potential additional wavelength values,
C     needed to capture the O-II background lines,
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
C---- WURGO       as of 2007 Jan 25
      parameter   (MX2L=1, LX2L=3)
      integer     MX2L, LX2L, IUX2, ILX2, LDLX2
      real*8      X2MAS, X2SKE, X2WVL, X2WLO, X2WHI, X2NUU, X2NUL, X2AUL
      real*8      X2PU,  X2PL,  X2DDL, X2CDL, X2CRD, X2CVW, X2CSK
      dimension   X2WVL(MX2L), X2WLO(MX2L), X2WHI(MX2L), X2NUU(MX2L),
     $            X2NUL(MX2L), X2PU(MX2L),  X2PL(MX2L),  X2AUL(MX2L),
     $            IUX2(MX2L),  ILX2(MX2L),  LDLX2(MX2L)
      dimension   X2DDL(LX2L,MX2L), X2CDL(LX2L,MX2L),
     $            X2CRD(LX2L,MX2L), X2CVW(LX2L,MX2L), X2CSK(LX2L,MX2L)
      common      /WURGO0/ X2MAS,X2SKE
      common      /WURGO1/ X2WVL,X2WLO,X2WHI
      common      /WURGO2/ X2NUU,X2NUL,X2PU,X2PL
      common      /WURGO3/ X2AUL,X2DDL,X2CDL,X2CRD,X2CVW,X2CSK
      common      /WURGO4/ IUX2,ILX2,LDLX2
C     Data for Oxygen-II lines in the background.
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
      external COMPD, WITHIN, LAPEL, POWDER, HI, BYE
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
      call HI ('ADDOX2')
C     !BEG
      YES = .false.
C
      M = 0
      do 101 J = 1,MX2L
        call COMPD      (WVL, X2WVL(J), DELTA, IFLG)
        if(IFLG.ne.0) then
          call WITHIN   (WVLO, X2WVL(J), WVHI, 0, OK)
C
          if(DUMP) then
            write (LUEO,100) J,WVLO,X2WVL(J),WVHI,OK
  100       format(' ','ADDOX2:   J =',I3,', WVLO =',1PE14.7,
     $                 ', WVL(J) =',E14.7,', WVHI =',E14.7,'; OK:',L5)
          end if
C
          if(OK) then
            call POWDER (X, J, J, NIAUGM, TAB, M)
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
      call BYE ('ADDOX2')
C
      return
      end
