      subroutine HADRY
     $(Z,ZIN,GMASIN,TAUKIN,N,QNAME,KZXST,KMASN,KTKIN,RFMAS,
     $ GMASRF,REFLM)
C
C     Rudolf Loeser, 1978 Sep 15
C---- Sets up Z-recalculation control.
C     !DASH
      save
C     !DASH
      real*8 GMASIN, GMASRF, REFLM, RFMAS, RYDBRG, TAUKIN, Z, ZERO, ZIN
      integer KMASN, KTKIN, KZXST, LUEO, N
      logical ZG, ZT, ZZ
      character QNAME*8
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 4),RYDBRG)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external NAUGHTD, MESHED, VECOUT, ABORT, MOVE1, CONADD, HI, BYE
C
C               GMASIN(N), GMASRF(N), TAUKIN(N), Z(N), ZIN(N)
      dimension GMASIN(*), GMASRF(*), TAUKIN(*), Z(*), ZIN(*)
C
      call HI ('HADRY')
C     !BEG
      KZXST = 0
      KMASN = 0
      KTKIN = 0
C
      call NAUGHTD    (Z, 1, N, ZZ)
      if(.not.ZZ) then
        KZXST = 1
      end if
C
      call NAUGHTD    (GMASRF, 1, N, ZG)
      if(.not.ZG) then
        KMASN = 1
      end if
C
      call NAUGHTD    (TAUKIN, 1, N, ZT)
      if((.not.ZT).and.(QNAME.eq.'HYDROGEN')) then
        KTKIN = 1
        if(REFLM.le.ZERO) then
          REFLM = RYDBRG
        end if
      end if
C
      if((KMASN+KTKIN).eq.2) then
        call MESHED   ('HADRY', 1)
        write (LUEO,100) KTKIN,KMASN
  100   format(' ','Error in HADRY: KTKIN = KMASN = 1',2I5//
     $         ' ','Input values of either ZMASS or TAUKIN may be ',
     $             'provided, but not both.')
        call VECOUT   (LUEO, TAUKIN, N, 'TAUKIN')
        call VECOUT   (LUEO, GMASRF, N, 'ZMASS' )
        call ABORT
      end if
C
      call MOVE1      (Z, N, ZIN)
C
      if(KMASN.gt.0) then
        call MOVE1    (GMASRF, N, GMASIN)
        if(RFMAS.gt.ZERO) then
          call CONADD (RFMAS, GMASIN, N)
        end if
      end if
C     !END
      call BYE ('HADRY')
C
      return
      end
