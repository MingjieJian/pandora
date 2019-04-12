      subroutine HYGRO
     $(NL,AIJ,P,XNU,OSF)
C
C     Rudolf Loeser, 1992 Mar 13
C---- Sets up oscillator strengths.
C     !DASH
      save
C     !DASH
      real*8 AIJ, OSF, P, XNU, ZERO
      integer IL, IU, IUL, NL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external INDXUL, SLEEPY, HI, BYE
C
C               AIJ(NL,NL), OSF(MUL), P(NSL), XNU(NSL)
      dimension AIJ(NL,*),  OSF(*),   P(*),   XNU(*)
C
      call HI ('HYGRO')
C     !BEG
      do 101 IU = 2,NL
        do 100 IL = 1,(IU-1)
          call INDXUL   (IU,IL,IUL)
          if(AIJ(IU,IL).gt.ZERO) then
            call SLEEPY (AIJ(IU,IL),P(IU),XNU(IU),P(IL),XNU(IL),
     $                   OSF(IUL))
          else
            OSF(IUL) = ZERO
          end if
  100   continue
  101 continue
C     !END
      call BYE ('HYGRO')
C
      return
      end
