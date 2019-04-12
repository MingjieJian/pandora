      subroutine LOUDER
     $(X,K,L,NMAX,TAB,N)
C
C     Rudolf Loeser, 2007 Jan 22
C---- Sets up a table of wavelengths (Angstroms) to capture
C     one (L=K) or all (L=0) of the O-III background lines.
C     !DASH
      save
C     !DASH
      real*8 TAB, X
      integer JJBXI, JJTE, JJV, K, KBX, L, LL, N, NMAX
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
      call HI ('LOUDER')
C     !BEG
      LL = L
      if((LL.le.0).or.(LL.gt.MX3L)) then
        LL = MX3L
      end if
C
      call MUMBAI (K, LL, X(JJBXI), KBX, X(JJTE), X(JJV), X3MAS, X3WVL,
     $             LDLX3, LX3L, X3DDL, N, NMAX, TAB)
C     !END
      call BYE ('LOUDER')
C
      return
      end
