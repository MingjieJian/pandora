      subroutine COMBAT
     $(X,K,L,NMAX,TAB,N)
C
C     Rudolf Loeser, 2004 Jun 28
C---- Sets up a table of wavelengths (Angstroms) to capture
C     one (L=K) or all (L=0) of the O-I background lines.
C     !DASH
      save
C     !DASH
      real*8 TAB, X
      integer JJBXI, JJTE, JJV, K, KBX, L, LL, N, NMAX
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
      external WOMBAT, HI, BYE
C
      dimension X(*)
C
C               TAB(NMAX)
      dimension TAB(*)
C
      call HI ('COMBAT')
C     !BEG
      LL = L
      if((LL.le.0).or.(LL.gt.MOXL)) then
        LL = MOXL
      end if
C
      call WOMBAT (K, LL, X(JJBXI), KBX, X(JJTE), X(JJV), OXMAS, OXWVL,
     $             NMAX, TAB, N)
C     !END
      call BYE ('COMBAT')
C
      return
      end
