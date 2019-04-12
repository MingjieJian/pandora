      subroutine BUCKET
     $(ZF,NF,ZT,NT,RHO)
C
C     Rudolf Loeser, 1969 Feb 04
C---- Sets extrapolated Rho's at large depths = 0, for BULL.
C     !DASH
      save
C     !DASH
      real*8 DELTA, RHO, ZERO, ZF, ZT
      integer IFLG, J, NF, NT
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external COMPD, HI, BYE
C
C               ZF(NF), ZT(NT), RHO(NT)
      dimension ZF(*),  ZT(*),  RHO(*)
C
      data      DELTA /1.D-12/
C
      call HI ('BUCKET')
C     !BEG
      if(NF.gt.0) then
C
        J = NT
  100   continue
C
        call COMPD (ZT(J), ZF(NF), DELTA, IFLG)
        if(IFLG.gt.0) then
          RHO(J) = ZERO
C
          J = J-1
          if(J.gt.0) then
            goto 100
          end if
C
        end if
C
      end if
C     !END
      call BYE ('BUCKET')
C
      return
      end
