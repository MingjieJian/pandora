      subroutine QUAHOG
     $(X,K,L,NMAX,TAB,N)
C
C     Rudolf Loeser, 2005 Jun 24
C---- Sets up a table of wavelengths (Angstroms) to capture
C     one (L=K) or all (L=0) of the He-I background lines.
C     (This is version 2 of QUAHOG.)
C     !DASH
      save
C     !DASH
      real*8 TAB, X
      integer JJBXI, JJTE, JJV, K, KBX, L, LL, N, NMAX
C     !COM
C---- FIRGO       as of 2005 Jul 07
      parameter   (MHEE=4)
      integer     MHEE, IUHEE, ILHEE
      real*8      HEEMAS, HEEWVL, HEEWLO, HEEWHI, HEENUU, HEENUL
      real*8      HEEAUL, HEEPU,  HEEPL,  HEECRD, HEECVW, HEECSK
      real*8      HEESKE
      dimension   HEEWVL(MHEE), HEEWLO(MHEE), HEEWHI(MHEE),
     $            HEENUU(MHEE), HEENUL(MHEE), HEEPU(MHEE),
     $            HEEPL(MHEE),  HEEAUL(MHEE), HEECRD(MHEE),
     $            HEECVW(MHEE), HEECSK(MHEE),
     $            IUHEE(MHEE),  ILHEE(MHEE)
      common      /FIRGO0/ HEEMAS,HEESKE
      common      /FIRGO1/ HEEWVL,HEEWLO,HEEWHI
      common      /FIRGO2/ HEENUU,HEENUL,HEEPU,HEEPL
      common      /FIRGO3/ HEEAUL,HEECRD,HEECVW,HEECSK
      common      /FIRGO4/ IUHEE,ILHEE
C     Data for Helium lines in the background.
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
      call HI ('QUAHOG')
C     !BEG
      LL = L
      if((LL.le.0).or.(LL.gt.MHEE)) then
        LL = MHEE
      end if
C
      call WOMBAT (K, LL, X(JJBXI), KBX, X(JJTE), X(JJV), HEEMAS,
     $             HEEWVL, NMAX, TAB, N)
C     !END
      call BYE ('QUAHOG')
C
      return
      end
