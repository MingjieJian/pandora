      subroutine POWDER
     $(X,K,L,NMAX,TAB,N)
C
C     Rudolf Loeser, 2007 Jan 22
C---- Sets up a table of wavelengths (Angstroms) to capture
C     one (L=K) or all (L=0) of the O-II background lines.
C     !DASH
      save
C     !DASH
      real*8 TAB, X
      integer JJBXI, JJTE, JJV, K, KBX, L, LL, N, NMAX
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
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(11),KBX)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(254),JJBXI)
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ( 12),JJV  )
C     !DASH
C     !EJECT
      external MUMBAI, HI, BYE
C
      dimension X(*)
C
C               TAB(NMAX)
      dimension TAB(*)
C
      call HI ('POWDER')
C     !BEG
      LL = L
      if((LL.le.0).or.(LL.gt.MX2L)) then
        LL = MX2L
      end if
C
      call MUMBAI (K, LL, X(JJBXI), KBX, X(JJTE), X(JJV), X2MAS, X2WVL,
     $             LDLX2, LX2L, X2DDL, N, NMAX, TAB)
C     !END
      call BYE ('POWDER')
C
      return
      end
