      subroutine FLUFF
     $(TE,CEQHH,N,KODE)
C
C     Rudolf Loeser, 1982 May 28
C---- Computes H2 number density cofficient.
C     Data from R. Kurucz, SAO Spec. Rept. #309, pp. 55 and 267.
C     Returns KODE .eq. 0 if calculation was successful, .gt. 0 if not.
C     (This is version 3 of FLUFF).
C     !DASH
      save
C     !DASH
      real*8 A1, A2, A3, A4, A5, A6, ARG, B, CEQ, CEQHH, CEQMX, HUNDRD,
     $       T, TE, THI, TLOG, ZERO
      integer I, KODE, N
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 75),CEQMX)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external  HI, BYE
      intrinsic min
C
C               TE(N), CEQHH(N)
      dimension TE(*), CEQHH(*)
C
      data A1,A2,A3,A4,A5,A6 /
     $     +5.1955D+04, -4.6628D+01, +1.8031D-03, -5.0239D-07,
     $     +8.1424D-11, -5.0501D-15/
      data B,HUNDRD,THI /1.5D0, 1.D2, 6.D3/
C
      call HI ('FLUFF')
C     !BEG
      KODE = 0
C
      do 100 I = 1,N
        T = min(TE(I),THI)
        if(T.le.ZERO) then
          KODE = I
          goto 101
        end if
C
        TLOG = log(T)
        ARG  = A1/T+A2+(A3+(A4+(A5+A6*T)*T)*T)*T
        ARG  = ARG-B*TLOG
        if(ARG.gt.HUNDRD) then
          CEQHH(I) = CEQMX
        else
          CEQ      = exp(ARG)
          CEQHH(I) = min(CEQ,CEQMX)
        end if
  100 continue
C
  101 continue
C     !END
      call BYE ('FLUFF')
C
      return
      end
