      subroutine NATICA
     $(N,G,GC,YBRC,SN,RHOSO)
C
C     Rudolf Loeser, 1986 Jul 30
C---- Computes "Sobolev" RHO, for SKUA.
C     !DASH
      save
C     !DASH
      real*8 G, GC, ONE, RAT, RHOSO, SN, YBRC
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, BOUNDUP, HI, BYE
C
C               G(N), GC(N), YBRC(N), SN(N), RHOSO(N)
      dimension G(*), GC(*), YBRC(*), SN(*), RHOSO(*)
C
      call HI ('NATICA')
C     !BEG
      do 100 I = 1,N
        call DIVIDE (YBRC(I),SN(I),RAT)
        RHOSO(I) = G(I)-GC(I)*RAT
  100 continue
      call BOUNDUP  (N,RHOSO,ONE)
C     !END
      call BYE ('NATICA')
C
      return
      end
