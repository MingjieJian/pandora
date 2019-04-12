      subroutine GIKWE
     $(XM,NLM,DI,D,SXX,XX,SYY,YY)
C
C     Rudolf Loeser, 1987 Oct 23
C---- Prints, for TRUE.
C     !DASH
      save
C     !DASH
      real*8 D, DI, SXX, SYY, XM, XX, YY
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
C               XM(NLM,NLM), DI(NLM)
      dimension XM(*),       DI(*)
C
      call HI ('GIKWE')
C     !BEG
      call LINER   (2, LUEO)
C
      write (LUEO,100)
  100 format(' ','Remaining NOVA/VAMOS-specific results:')
C
      call DARROUT (LUEO, XM  , NLM, NLM, 'XM-inverse'                )
      call DVECOUT (LUEO, DI  , NLM     , 'Scaled determinants DI'    )
      call DVECOUT (LUEO, SXX ,   1     , 'Sum for XX'                )
      call DVECOUT (LUEO, XX  ,   1     , 'XX'                        )
      call DVECOUT (LUEO, D   ,   1     , 'Scaled determinant D'      )
      call DVECOUT (LUEO, SYY ,   1     , 'Sum for YY'                )
      call DVECOUT (LUEO, YY  ,   1     , 'YY'                        )
C     !END
      call BYE ('GIKWE')
C
      return
      end
