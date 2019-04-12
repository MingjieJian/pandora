      subroutine HOWEL
     $(N,NL,Z,CHT,CHL,VEC,FRS)
C
C     Rudolf Loeser, 1982 Feb 24
C---- Computes Integrated Heating rates, for DYLAN.
C     !DASH
      save
C     !DASH
      real*8 CHL, CHT, FRS, VEC, Z
      integer J, N, NL, NT
C     !DASH
      external HEAP, HI, BYE
C
C               CHT(N,NT), CHL(N,NL), Z(N), VEC(N), FRS(N)
      dimension CHT(N,*),  CHL(N,*),  Z(*), VEC(*), FRS(*)
C
C
      call HI ('HOWEL')
C     !BEG
      NT = (NL*(NL-1))/2
C
      do 100 J = 1,NT
        call HEAP (N,Z,CHT(1,J),VEC,FRS)
  100 continue
C
      do 101 J = 1,NL
        call HEAP (N,Z,CHL(1,J),VEC,FRS)
  101 continue
C     !END
      call BYE ('HOWEL')
C
      return
      end
