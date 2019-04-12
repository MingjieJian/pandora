      subroutine THETA
     $(TE,RT,N)
C
C     Rudolf Loeser, 1982 Jun 21
C---- Computes a table of reciprocal temperature.
C     !DASH
      save
C     !DASH
      real*8 RT, TE
      integer I, N
C     !DASH
      external RECTOR, HI, BYE
C
C               TE(N), RT(N)
      dimension TE(*), RT(*)
C
      call HI ('THETA')
C     !BEG
      do 100 I = 1,N
        call RECTOR (TE(I),RT(I))
  100 continue
C     !END
      call BYE ('THETA')
C
      return
      end
