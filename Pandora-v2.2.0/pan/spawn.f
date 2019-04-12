      subroutine SPAWN
     $(N,KOOLS,XNE,HND,TE,SUM,SUMSM)
C
C     Rudolf Loeser, 1998 Jun 26
C---- Saves cooling rates data.
C     !DASH
      save
C     !DASH
      real*8 HND, SUM, SUMSM, TE, XNE
      integer KOOLS, LUMR, N
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(29),LUMR )
C     !DASH
      external BUNT, YARDEN, HI, BYE
C
C               SUMSM(N), SUM(N), XNE(N), HND(N), TE(N)
      dimension SUMSM(*), SUM(*), XNE(*), HND(*), TE(*)
C
      call HI ('SPAWN')
C     !BEG
      call YARDEN (LUMR,1,'COOLING RATES')
C
      write (LUMR,100) N,KOOLS
  100 format('N (',I4,' )  KOOLSUM (',I4,' ) > ')
C
      call BUNT   (LUMR,XNE  ,'NE')
      call BUNT   (LUMR,HND  ,'NH')
      call BUNT   (LUMR,TE   ,'TE')
      call BUNT   (LUMR,SUM  ,'SUM')
      call BUNT   (LUMR,SUMSM,'SUMSM')
C
      call YARDEN (LUMR,2,'COOLING RATES')
C     !END
      call BYE ('SPAWN')
C
      return
      end
