      subroutine DOTTY
     $(AMASS,A,P,XNU,CRD,CVW,CSK,KODE)
C
C     Rudolf Loeser, 2004 Apr 20
C---- Checks the O-I data in FURGO. Returns with KODE=1 if they agree,
C     with KODE=0 if not.
C     !DASH
      save
C     !DASH
      real*8 A, AMASS, CRD, CSK, CVW, DELTA, P, XNU
      integer I, IL, IU, IUL, K, K1, K2, K3, K4, K5, KODE, LDLMX, LUEO,
     $        NL
      logical OK
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(33),LDLMX)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
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
C     !DASH
C     !EJECT
      external  INTRANS, COMPD, MESHED, MASHED, SHOUT, LINER, SLIME,
     $          HI, BYE
      intrinsic abs
C
C               CRD(LDLMX,NT), CVW(LDLMX,NT), CSK(LDLMX,NT), A(NL,NL),
      dimension CRD(LDLMX,*),  CVW(LDLMX,*),  CSK(LDLMX,*),  A(NL,*),
C
C               XNU(NSL), P(NSL)
     $          XNU(*),   P(*)
C
      data DELTA /1.D-5/
C
      call HI ('DOTTY')
C     !BEG
      call SLIME     (MOXL, IUOX, ILOX, 'OXYGEN', KODE, OK)
      if(.not.OK) then
        goto 106
      end if
C
C
      call COMPD     (AMASS,    OXMAS,    DELTA, K )
C
      do 100 I = 1,MOXL
        IU = IUOX(I)
        IL = ILOX(I)
C
        call COMPD   (A(IU,IL), OXAUL(I), DELTA, K1)
        call COMPD   (P(IU),    OXPU(I),  DELTA, K2)
        call COMPD   (P(IL),    OXPL(I),  DELTA, K3)
        call COMPD   (XNU(IU),  OXNUU(I), DELTA, K4)
        call COMPD   (XNU(IL),  OXNUL(I), DELTA, K5)
        K = K+abs(K1)+abs(K2)+abs(K3)+abs(K4)+abs(K5)
C
        call INTRANS (IU, IL, 'DOTTY', IUL)
        call COMPD   (CRD(1,IUL), OXCRD(I), DELTA, K1)
        call COMPD   (CVW(1,IUL), OXCVW(I), DELTA, K2)
        call COMPD   (CSK(1,IUL), OXCSK(I), DELTA, K3)
        K = K+abs(K1)+abs(K2)+abs(K3)
  100 continue
C
      if(K.eq.0) then
        KODE = 1
      else
        KODE = 0
      end if
C     !EJECT
      if(KODE.ne.1) then
        call MESHED    ('DOTTY', 3)
        call SHOUT     (LUEO, KODE, 'OXYGEN')
C
        I = 0
        call LINER     (1, LUEO)
        write (LUEO,101)
  101   format(' ',25X,'mass')
        write (LUEO,102) I,AMASS
  102   format(' ',I5,'  this run',1P8E14.6)
        write (LUEO,103)   OXMAS
  103   format(' ',5X,'  built-in',1P8E14.6)
        call LINER     (1, LUEO)
        write (LUEO,104)
  104   format(' ',25X,'P(u)',10X,'P(l)',9X,'NU(u)',9X,'NU(l)',13X,'A',
     $             11X,'CRD',11X,'CVW',11X,'CSK')
        do 105 I = 1,MOXL
          IU = IUOX(I)
          IL = ILOX(I)
          call INTRANS (IU, IL, 'DOTTY', IUL)
          write (LUEO,102) I,P(IU),P(IL),XNU(IU),XNU(IL),A(IU,IL),
     $                       CRD(1,IUL),CVW(1,IUL),CSK(1,IUL)
          write (LUEO,103)   OXPU(I),OXPL(I),OXNUU(I),OXNUL(I),OXAUL(I),
     $                       OXCRD(I),OXCVW(I),OXCSK(I)
  105   continue
        call MASHED    ('DOTTY')
      end if
C
  106 continue
C     !END
      call BYE ('DOTTY')
C
      return
      end
