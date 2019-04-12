      subroutine KASLAR
     $(X,K,L,NMAX,TAB,N)
C
C     Rudolf Loeser, 2004 Aug 06
C---- Sets up a table of wavelengths (Angstroms) to capture
C     one (L=K) or some(L>K) of the Hydrogen Lyman background lines.
C     (This is version 2 of KASLAR.)
C     !DASH
      save
C     !DASH
      real*8 ONE, TAB, X
      integer JJBXI, JJTE, JJV, K, KBX, L, N, NMAX
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
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external WOMBAT, HI, BYE
C
      dimension X(*)
C
C               TAB(NMAX)
      dimension TAB(*)
C
      call HI ('KASLAR')
C     !BEG
      call WOMBAT (K, L, X(JJBXI), KBX, X(JJTE), X(JJV), ONE, HYWVL,
     $             NMAX, TAB, N)
C     !END
      call BYE ('KASLAR')
C
      return
      end
