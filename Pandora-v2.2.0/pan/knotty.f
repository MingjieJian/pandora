      subroutine KNOTTY
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 2007 Jan 22
C---- Plays with data needed for O-II background contributor lines.
C     !DASH
      save
C     !DASH
      real*8 DNU, W, X
      integer I, IW, IX, JJBXI, JJTE, JJV
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
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ( 12),JJV  )
      equivalence (IZOQ(254),JJBXI)
C     !DASH
C     !EJECT
      external ANGIE, DIREX, RITTER, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      call HI ('KNOTTY')
C     !BEG
C---- Set up central wavelengths
      do 100 I = 1,MX2L
        DNU = X2NUU(I)-X2NUL(I)
        call ANGIE (DNU, X2WVL(I))
  100 continue
C---- Set up wavelength cut-offs
      call DIREX   (X(JJTE), X(JJV), X(JJBXI), X2MAS, X2WVL, X2WLO,
     $              X2WHI, MX2L)
C---- Modify for blends
      call RITTER  (MX2L, LX2L, LDLX2, X2DDL, X2WLO, X2WHI)
C     !END
      call BYE ('KNOTTY')
C
      return
      end
