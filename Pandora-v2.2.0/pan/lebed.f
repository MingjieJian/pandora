      subroutine LEBED
     $(N,Z,EMU,ZN,R1N)
C
C     Rudolf Loeser, 1981 Oct 26
C---- Computes values of EMU = cos(THETA), for a ray passing the disk.
C     (See also "KIRGIZ".)
C     !DASH
      save
C     !DASH
      real*8 D2, DC, DI, EMU, R1N, RT, Z, ZERO, ZN
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external HI, BYE
C
C               Z(N), EMU(N)
      dimension Z(*), EMU(*)
C
      call HI ('LEBED')
C     !BEG
      DC = R1N+ZN
      D2 = (DC-Z(N))**2
C
      do 100 I = 1,(N-1)
        DI = DC-Z(I)
        RT = sqrt(DI**2-D2)
        EMU(I) = RT/DI
  100 continue
C
      EMU(N) = ZERO
C     !END
      call BYE ('LEBED')
C
      return
      end
