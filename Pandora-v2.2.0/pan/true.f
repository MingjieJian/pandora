      subroutine TRUE
     $(DI,XX,YY,XMI,XR,NLM,D,ZM11,W,IW,ITAU,DMPI)
C
C     Rudolf Loeser, 1968 Jan 30
C---- Computes XX and YY for NOVA and VAMOS.
C     !DASH
      save
C     !DASH
      real*8 D, DI, F, OD1, ONE, SXX, SYY, W, XMI, XR, XX, YY, ZERO,
     $       ZM11
      integer ITAU, IW, J, KODE, NLM
      logical DMPI
      character TIT*30
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external DIVIDE, MOTOR, HALT, GIKWE, HI, BYE
C
      dimension W(*), IW(*)
C
C               XMI(NLM,NLM), XR(NLM), DI(NLM)
      dimension XMI(NLM,*),   XR(*),   DI(*)
C
      call HI ('TRUE')
C     !BEG
      call DIVIDE  (ONE, DI(1), OD1)
C---- Get XX
      F   = ONE
      SXX = ZERO
      do 100 J = 2,NLM
        F = -F
        SXX = SXX+F*XMI(1,J)*(DI(J)*OD1)
  100 continue
      XX = ZM11+SXX
C
C---- Get inverse
      write (TIT,101) ITAU
  101 format('Matrix for NOVA, depth =',I3)
C
      call MOTOR   (XMI, NLM, TIT, W, IW, KODE)
      if(KODE.eq.0) then
        write (MSSLIN(1),102)
  102   format('No Plan-B available.')
        call HALT  ('TRUE', 1)
      end if
C
C---- Compute YY
      SYY = ZERO
      do 103 J = 1,NLM
        SYY = SYY+XMI(1,J)*XR(J)
  103 continue
      YY = SYY*(D*OD1)
C
      if(DMPI) then
        call GIKWE (XMI, NLM, DI, D, SXX, XX, SYY, YY)
      end if
C     !END
      call BYE ('TRUE')
C
      return
      end
