      subroutine GRUFF
     $(TE,CEQHH,N,KODE)
C
C     Rudolf Loeser, 1985 May 07
C---- Computes H2 number density coefficient.
C     Data from R. Kurucz, 1985, ApJ.Lett. (in press).
C
C     Returns KODE .eq. 0 if calculation was successful, .gt. 0 if not.
C     !DASH
      save
C     !DASH
      real*8 A, ARG, B, C, CEQHH, D, EVPK, PT, T, TA, TB, TE, TL, TN,
     $       ZERO
      integer I, KODE, N
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON(14),EVPK  )
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
      external HI, BYE
C
C               TE(N), CEQHH(N)
      dimension TE(*), CEQHH(*)
C
      dimension C(7)
C
      data A,B,TN /4.478D0, 1.5D0, 1.D4/
      data C /4.64584D1, 1.6366D1, 4.93992D1, 1.11822D2, 1.49567D2,
     $        1.06206D2, 3.0872D1/
C
      call HI ('GRUFF')
C     !BEG
      KODE = 0
C
      D = A/EVPK
      do 100 I = 1,N
        if(TE(I).le.ZERO) then
          KODE = I
          goto 101
        else
C
          TL  = log(TE(I))
          TB  = B*TL
          TA  = D/TE(I)
          T   = TE(I)/TN
          PT  = T*(C(2)-T*(C(3)-T*(C(4)-T*(C(5)-T*(C(6)-T*C(7))))))
          ARG = TA+(PT-C(1))-TB
C
          CEQHH(I) = exp(ARG)
        end if
  100 continue
C
  101 continue
C     !END
      call BYE ('GRUFF')
C
      return
      end
