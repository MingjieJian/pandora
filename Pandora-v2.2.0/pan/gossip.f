      subroutine GOSSIP
     $(NLM,XM,XR)
C
C     Rudolf Loeser, 1968 Sep 25
C---- More additional debugging printout for calculation of
C     statistical equilibrium equations.
C     !DASH
      save
C     !DASH
      real*8 XM, XR
      integer LUEO, NLM
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, DARROUT, DVECOUT, HI, BYE
C
C               XM(NLM,NLM), XR(NLM)
      dimension XM(*),       XR(*)
C
      call HI ('GOSSIP')
C     !BEG
      call LINER   (2, LUEO)
      write (LUEO,100)
  100 format(' ','***** After NOVA/VAMOS-specific row and column ',
     $           'interchanges:')
C
      call DARROUT (LUEO, XM, NLM, NLM, 'XM')
      call DVECOUT (LUEO, XR, NLM     , 'XR')
C     !END
      call BYE ('GOSSIP')
C
      return
      end
