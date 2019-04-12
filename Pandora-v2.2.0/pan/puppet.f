      subroutine PUPPET
     $(IN,N,LIMP,POPK,POPN,KBD,BD)
C
C     Rudolf Loeser, 1988 Oct 24
C---- Updates Continuum Recalculation controls.
C     (This is version 5 of PUPPET.)
C     !DASH
      save
C     !DASH
      real*8 BD, POPK, POPN
      integer IN, KBD, KNT, LIMP, N
C     !DASH
      external WENDY, HI, BYE
C
C               POPK(N), POPN(N,LIMP), BD(N,LIMP)
      dimension POPK(*), POPN(*),      BD(*)
C
      call HI ('PUPPET')
C     !BEG
      KNT = N*LIMP
      call WENDY   (POPN,1,KNT,(IN  ),'PUPPET')
      if(KBD.gt.0) then
        call WENDY (BD  ,1,KNT,(IN+1),'PUPPET')
      end if
      call WENDY   (POPK,1,N  ,(IN+2),'PUPPET')
C     !END
      call BYE ('PUPPET')
C
      return
      end
