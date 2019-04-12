      subroutine PETREL
     $(N,EP,BS,PB,DEL,DLC,FNDT,BA,BF)
C
C     Rudolf Loeser, 1984 Nov 02
C           revised, 2004 May 07
C
C---- Computes BA and BF for LINDEN.
C     !DASH
      save
C     !DASH
      real*8 BA, BF, BS, DEL, DLC, EP, FNDT, OED, ONE, PB
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
      external DIVIDE, HI, BYE
C
C               DEL(N), DLC(N), FNDT(N), BS(N), EP(N), PB(N), BA(N),
      dimension DEL(*), DLC(*), FNDT(*), BS(*), EP(*), PB(*), BA(*),
C
C               BF(N)
     $          BF(*)
C
      call HI ('PETREL')
C     !BEG
      do 100 I = 1,N
        call DIVIDE (ONE, (EP(I)+DEL(I)), OED)
C
        BA(I) = (FNDT(I)+EP(I)*BS(I)+DLC(I))*OED
        BF(I) = BA(I)+PB(I)*OED
  100 continue
C     !END
      call BYE ('PETREL')
C
      return
      end
