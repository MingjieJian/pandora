      subroutine PRODAY
     $(GMASIN,G,Z,N,X,W,IW,IMG,XCBL,ZOLD,P,Y,GG,ZZ,OP5000,TAU5000,ISWA,
     $ MODE)
C
C     Rudolf Loeser, 2003 Nov 03
C---- Computes Z from mass and G.
C     !DASH
      save
C     !DASH
      real*8 CZ, G, GG, GMASIN, ONE, OP5000, P, TAU5000, W, WAVE, X,
     $       XCBL, Y, Z, ZOLD, ZZ
      integer I, IMG, ISWA, IW, IZ, KODE, KTYPE, LUEO, MODE, N
      logical DUMP, EDINT, EDTAU, KILROY
      character LABEL*40, LOBEL*49
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
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
      external MOVE1, RECIPRO, PAYDOR, DOPRAY, NOTMORE, RIPPLE, CATRIN,
     $         MESHED, VECOUT, HALT, CONSUB, TUNA, ABORT, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               GMASIN(N), XCBL(Miklen), TAU5000(N), OP5000(N), IMG(N),
      dimension GMASIN(*), XCBL(*),      TAU5000(*), OP5000(*), IMG(*),
C
C               G(N), ZZ(N), ZOLD(N), P(N), Y(N), GG(N), ISWA(Nopac),
     $          G(*), ZZ(*), ZOLD(*), P(*), Y(*), GG(*), ISWA(*),
C
C               Z(N)
     $          Z(*)
C
      data WAVE,DUMP,KTYPE  /5000.D0, .false., 9/
      data LOBEL /'Tau-5000 for Z-from-mass'/
      data LABEL /'(Z-Z1) as integral over y of 1/G'/
C
      call HI ('PRODAY')
C     !BEG
C---- Save current Z
      call MOVE1    (Z, N, ZOLD)
C---- Get 1/G
      call MOVE1    (G, N, GG)
      call RECIPRO  (GG, N)
C---- Get P and y
      call PAYDOR (GMASIN, N, P)
      do 100 I = 1,N
        Y(I) = log(P(I))
  100 continue
C
C---- Integrate to get ZZ
      call TUNA     (N, Y, GG, ZZ, LABEL, KODE, EDINT, EDTAU, IMG, W)
      if(KODE.gt.0) then
        write (MSSLIN(1),101) 'ZZ',KODE
  101   format('KODE(',A,') =',I12,'; cannot compute with Z-from-mass.')
        call HALT   ('PRODAY', 1)
      end if
C     !EJECT
C---- Compute TAU5000, and the index of the largest TAU5000
C     not greater than 1
      KILROY = .true.
      call RIPPLE   (X, W, IW, WAVE, KTYPE, XCBL, KILROY, OP5000, DUMP)
      call TUNA     (N, ZZ, OP5000, TAU5000, LOBEL, KODE, EDINT, EDTAU,
     $               IMG, W)
      if(KODE.gt.0) then
        write (MSSLIN(1),101) 'TAU5',KODE
        call HALT   ('PRODAY', 1)
      end if
      call CATRIN   (TAU5000, N)
      call NOTMORE  (TAU5000, N, ONE, IZ)
      if((IZ.lt.1).or.(IZ.ge.N)) then
        call MESHED ('PRODAY', 1)
        write (LUEO, 102) IZ
  102   format(' ','IZ =',I12,'; TAU5000 for Z-from-mass is bad.')
        call VECOUT (LUEO, TAU5000, N, 'TAU-5000')
        call ABORT
      end if
C
C---- Compute Z-offset, and get final new Z
      CZ = ((TAU5000(IZ+1)-ONE)*ZZ(IZ)+(ONE-TAU5000(IZ))*ZZ(IZ+1))
     $     /(TAU5000(IZ+1)-TAU5000(IZ))
      call MOVE1    (ZZ, N, Z)
      call CONSUB   (CZ, Z, N)
C
C---- (? print)
      call DOPRAY   (MODE, N, IZ, CZ, ZOLD, GMASIN, P, Y, GG, ZZ,
     $               OP5000, TAU5000, Z, WAVE, ISWA, XCBL)
C     !END
      call BYE ('PRODAY')
C
      return
      end
