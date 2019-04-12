      subroutine VASEY
     $(N,XNK,SLVLS,ETA)
C
C     Rudolf Loeser, 1978 Aug 12
C---- Computes ETA, for LUDMILA.
C     !DASH
      save
C     !DASH
      real*8 ETA, SLVLS, XNK, ZERO
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
      external DIVIDE, HI, BYE
C
C               XNK(N), SLVLS(N), ETA(N)
      dimension XNK(*), SLVLS(*), ETA(*)
C
      call HI ('VASEY')
C     !BEG
      do 101 I = 1,N
        if(XNK(I).ne.ZERO) then
          call DIVIDE (XNK(I), (SLVLS(I)+XNK(I)), ETA(I))
        else
          ETA(I) = ZERO
        end if
  101 continue
C     !END
      call BYE ('VASEY')
C
      return
      end
