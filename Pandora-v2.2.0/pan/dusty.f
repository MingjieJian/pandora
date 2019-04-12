      subroutine DUSTY
     $(XLM,L,KILROY)
C
C     Rudolf Loeser, 2007 Jan 22
C---- Dumps for RUSTY.
C     !DASH
      save
C     !DASH
      real*8 XLM
      integer L, LUEO
      logical KILROY
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
      call HI ('DUSTY')
C     !BEG
      if(KILROY) then
        write (LUEO,100) XLM
  100   format(' ','Details of calculation of background Oxygen-II ',
     $             'lines at wavelength =',1PE20.13//
     $         ' ','Built-in O-II lines data:')
        call LINER (1, LUEO)
        call LYDIA (LUEO, MX2L, LX2L, X2MAS, X2SKE, IUX2, ILX2, X2WLO,
     $              X2WVL, X2WHI, X2NUU, X2PU, X2NUL, X2PL, X2AUL,
     $              X2CRD, X2CVW, X2CSK, LDLX2, X2DDL, X2CDL, 'DUSTY')
        KILROY = .false.
      end if
      call LINER   (2, LUEO)
      write (LUEO,101) L,X2WVL(L)
  101 format(' ','X2X2X2X2X2 Line #',I3,' at',1PE20.13,' Angstroms')
C     !END
      call BYE ('DUSTY')
C
      return
      end
