      subroutine MOKA
     $(DEE,DEEL,CRIT,ALLSML)
C
C     Rudolf Loeser, 2001 Dec 27
C---- Checks values, for CONTA.
C     !DASH
      save
C     !DASH
      real*8 CRIT, DEE, DEEL, ZERO
      integer I
      logical ALLSML
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
C               DEE(4,5), DEEL(4,5)
      dimension DEE(*),   DEEL(*)
C
      call HI ('MOKA')
C     !BEG
      ALLSML = .true.
C
      do 100 I = 1,20
        if(DEE(I).ne.ZERO) then
C
          if(DEEL(I).ge.CRIT) then
            ALLSML = .false.
            goto 101
          end if
C
        end if
  100 continue
C
  101 continue
C     !END
      call BYE ('MOKA')
C
      return
      end
