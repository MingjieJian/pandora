      subroutine WAIL
     $(XLM,L,KILROY)
C
C     Rudolf Loeser, 2007 Jan 22
C---- Dumps for RAIL.
C     !DASH
      save
C     !DASH
      real*8 XLM
      integer L, LUEO
      logical KILROY
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
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external LINER, LYDIA, HI, BYE
C
      call HI ('WAIL')
C     !BEG
      if(KILROY) then
        write (LUEO,100) XLM
  100   format(' ','Details of calculation of background Oxygen-III ',
     $             'lines at wavelength =',1PE20.13//
     $         ' ','Built-in O-III lines data:')
        call LINER (1, LUEO)
        call LYDIA (LUEO, MX3L, LX3L, X3MAS, X3SKE, IUX3, ILX3, X3WLO,
     $              X3WVL, X3WHI, X3NUU, X3PU, X3NUL, X3PL, X3AUL,
     $              X3CRD, X3CVW, X3CSK, LDLX3, X3DDL, X3CDL, 'WAIL')
        KILROY = .false.
      end if
      call LINER   (2, LUEO)
      write (LUEO,101) L,X3WVL(L)
  101 format(' ','X3X3X3X3X3 Line #',I3,' at',1PE20.13,' Angstroms')
C     !END
      call BYE ('WAIL')
C
      return
      end
