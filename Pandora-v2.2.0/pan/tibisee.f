      subroutine TIBISEE
     $(HNK,HND,N,HNKR)
C
C     Rudolf Loeser, 1978 Aug 21
C---- Computes ionized fraction of Hydrogen, for HELGA.
C     !DASH
      save
C     !DASH
      real*8 HND, HNK, HNKR
      integer N
      logical RZERO
C     !DASH
      external NAUGHTD, ARRDIV, HI, BYE
C
C               HNK(N), HND(N), HNKR(N)
      dimension HNK(*), HND(*), HNKR(*)
C
      call HI ('TIBISEE')
C     !BEG
      call NAUGHTD  (HNKR,1,N,RZERO)
      if(RZERO) then
        call ARRDIV (HNK,HND,HNKR,N)
      end if
C     !END
      call BYE ('TIBISEE')
C
      return
      end
