      subroutine VINIFY
     $(LS,NSL,XNUK,XNU,CP)
C
C     Rudolf Loeser, 2006 Jul 14
C---- Computes default values of CP (photoionization rate) with the
C     hydrogenic approximation.
C     !DASH
      save
C     !DASH
      real*8 CON, CP, DNU, FAC, RT, XNU, XNUK, ZERO
      integer JDCPN, L, LS, NSL
C     !COM
C---- APOLLO      as of 2006 Dec 04
      integer     MEST
      dimension   MEST(28)
      common      /APOLLO/ MEST
C     Atomic model parameter default values indicators
      equivalence (MEST(27),JDCPN)
C
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
C               XNU(NSL), CP(NSL+1)
      dimension XNU(*),   CP(*)
C
      data FAC,CON /7.91D-18, 3.2881D0/
C
      call HI ('VINIFY')
C     !BEG
      do 100 L = LS,NSL
        if(CP(L).le.ZERO) then
C
          if(XNU(L).lt.XNUK) then
            DNU = XNUK-XNU(L)
            RT  = sqrt(CON/DNU)
            CP(L) = FAC*RT
            JDCPN = JDCPN+1
          else
            CP(L) = ZERO
          end if
C
        end if
  100 continue
C     !END
      call BYE ('VINIFY')
C
      return
      end
