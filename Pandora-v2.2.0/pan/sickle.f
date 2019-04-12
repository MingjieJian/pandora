      subroutine SICKLE
     $(N,SQS,STM,XNUM)
C
C     Rudolf Loeser, 1987 Jan 05
C---- Dumps, for "b from b-ratios" calculation.
C     (This is version 2 of SICKLE.)
C     !DASH
      save
C     !DASH
      real*8 SQS, STM, XNUM
      integer LUEO, N
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external VECOUT, HI, BYE
C
C               SQS(N), STM(N), XNUM(N)
      dimension SQS(*), STM(*), XNUM(*)
C
      call HI ('SICKLE')
C     !BEG
      call VECOUT (LUEO, SQS , N, 'SQS'      )
      call VECOUT (LUEO, STM , N, 'STM'      )
      call VECOUT (LUEO, XNUM, N, 'Numerator')
C     !END
      call BYE ('SICKLE')
C
      return
      end
