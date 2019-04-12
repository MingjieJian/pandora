      subroutine BLUFF
     $(TE,CEQHH,N,KODE)
C
C     Rudolf Loeser, 1983 Mar 17
C---- Computes H2 number density coefficient.
C     Data from Tsuji,T. 1973, Astron.Astrophys., 23, 411.
C     Returns KODE .eq. 0 if calculation was successful, .gt. 0 if not.
C     !DASH
      save
C     !DASH
      real*8 A1, A2, A3, A4, A5, AT, BOLZMN, CEQHH, PAT, TE, TEMP, TEN,
     $       THETA, ZERO
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
      equivalence (PCON( 2),BOLZMN)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(11),TEN   )
C     !DASH
C     !EJECT
      external RECTOR, HI, BYE
C
C               TE(N), CEQHH(N)
      dimension TE(*), CEQHH(*)
C
      data A1,A2,A3 / 1.2739D1, -5.1172D0, 1.2572D-1/
      data A4,A5    /-1.4149D-2, 6.3021D-4/
C
      call HI ('BLUFF')
C     !BEG
      KODE = 0
C
      do 100 I = 1,N
        TEMP = TE(I)
        if(TEMP.le.ZERO) then
          KODE = I
          goto 101
        end if
C
        call RECTOR (TEMP,THETA)
        AT  = A1+(A2+(A3+(A4+A5*THETA)*THETA)*THETA)*THETA
        PAT = TEN**(-AT)
C
        CEQHH(I) = TEMP*BOLZMN*PAT
C
  100 continue
C
  101 continue
C     !END
      call BYE ('BLUFF')
C
      return
      end
