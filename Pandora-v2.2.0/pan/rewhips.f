      subroutine REWHIPS
     $(NL,PIJ,CIJ,GMI,YBRIJ,SAIJ,YIJ,Z,NLM,XM,XR,SM,SR,ZM11)
C
C     Rudolf Loeser, 2003 Nov 19
C---- Gives additional debugging printout for calculation of
C     statistical equilibrium equations.
C     !DASH
      save
C     !DASH
      real*8 CIJ, GMI, PIJ, SAIJ, SM, SR, XM, XR, YBRIJ, YIJ, Z, ZM11
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
C               XM(NL-1,NL-1), XR(NL-1), SM(NL-1), YIJ(NL,NL), GMI(NL),
      dimension XM(*),         XR(*),    SM(*),    YIJ(*),     GMI(*),
C
C               YBRIJ(NL,NL), CIJ(NL,NL), PIJ(NL,NL), SAIJ(NL,NL),
     $          YBRIJ(*),     CIJ(*),     PIJ(*),     SAIJ(*),
C
C               Z(NL,NL)
     $          Z(*)
C
      call HI ('REWHIPS')
C     !BEG
      call LINER   (2, LUEO)
      write (LUEO,100)
  100 format(' ','***** Input and results of GLASS/VAMOS: basic ',
     $           'statistical equilibrium terms ---')
C
      call DARROUT (LUEO, CIJ   , NL , NL , 'CIJ'   )
      call DARROUT (LUEO, PIJ   , NL , NL , 'PIJ'   )
      call DARROUT (LUEO, YBRIJ , NL , NL , 'JBARIJ')
      call DARROUT (LUEO, SAIJ  , NL , NL , 'SAIJ'  )
      call DARROUT (LUEO, YIJ   , NL , NL , 'YIJ'   )
      call DVECOUT (LUEO, GMI   , NL      , 'GMI'   )
C
      call DARROUT (LUEO, Z     , NL , NL , 'Z'     )
      call DARROUT (LUEO, XM    , NLM, NLM, 'XM'    )
      call DVECOUT (LUEO, XR    , NLM     , 'XR'    )
      call DVECOUT (LUEO, SM    , NLM     , 'SM'    )
      call DVECOUT (LUEO, SR    ,   1     , 'SR'    )
      call DVECOUT (LUEO, ZM11  ,   1     , 'ZM11'  )
C     !END
      call BYE ('REWHIPS')
C
      return
      end
