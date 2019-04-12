      subroutine WHISPER
     $(NL,Z,NLM,XM,XR,SM,SR,ZM11)
C
C     Rudolf Loeser, 1968 Sep 25
C---- Gives additional debugging printout for calculation of
C     statistical equilibrium equations.
C     !DASH
      save
C     !DASH
      real*8 SM, SR, XM, XR, Z, ZM11
      integer LUEO, NL, NLM
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
C               XM(NL-1,NL-1), XR(NL-1), SM(NL-1), Z(NL,NL)
      dimension XM(*),         XR(*),    SM(*),    Z(*)
C
      call HI ('WHISPER')
C     !BEG
      write (LUEO,100)
  100 format(' ','***** Results from GRASS: basic statistical ',
     $           'equilibrium terms ---')
C
      call DARROUT (LUEO, Z   , NL , NL , 'Z'   )
      call DARROUT (LUEO, XM  , NLM, NLM, 'XM'  )
      call DVECOUT (LUEO, XR  , NLM     , 'XR'  )
      call DVECOUT (LUEO, SM  , NLM     , 'SM'  )
      call DVECOUT (LUEO, SR  ,   1     , 'SR'  )
      call DVECOUT (LUEO, ZM11,   1     , 'ZM11')
C     !END
      call BYE ('WHISPER')
C
      return
      end
