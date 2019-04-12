      subroutine SPROING
     $(N,IU,IL,AIJ,P,ALF,BETA,PE,FE,XND,XJBR,GMA,LU,TF,TG)
C
C     Rudolf Loeser, 2005 MAR 31
C---- Computes intermediare terms TF and TG for PRD.
C     (This is version 2 of SPROING.)
C     !DASH
      save
C     !DASH
      real*8 AIJ, ALF, BETA, EP1, EP2, FE, GMA, HA, OALF, OAUL, ONE, P,
     $       PE, PRAT, RNLU, TF, TFB, TG, TGB, XJBR, XND
      integer I, IL, IPRDD, IU, IUL, LU, N, NL
      logical PRINT
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 98),IPRDD)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external  LINER, INDXUL, DIVIDE, HI, BYE
      intrinsic mod
C
C               BETA(N,MUL), ALF(MUL), AIJ(NL,NL), XJBR(N), XND(N,NL),
      dimension BETA(N,*),   ALF(*),   AIJ(NL,*),  XJBR(*), XND(N,*),
C
C               P(NSL), PE(N), FE(N), GMA(N), TF(N), TG(N)
     $          P(*),   PE(*), FE(*), GMA(*), TF(*), TG(*)
C
      call HI ('SPROING')
C     !BEG
      call INDXUL   (IU, IL, IUL)
      call DIVIDE   (ONE, ALF(IUL), OALF)
      call DIVIDE   (P(IU), P(IL), PRAT)
      call DIVIDE   (ONE, AIJ(IU,IL), OAUL)
C
      PRINT = LU.gt.0
      if(PRINT) then
        call LINER  (2, LU)
        write (LU,100) OAUL,OALF,PRAT
  100   format(' ','OAUL =',1PE12.5,8X,'OALF =',E12.5,8X,
     $             'PRAT =',E12.5//
     $         ' ',14X,'RNLU',9X,'BETA',10X,'EP1',10X,'EP2',11X,'HA',
     $             10X,'TFB',11X,'TF',10X,'TGB',11X,'TG')
      end if
C
      do 102 I = 1,N
        call DIVIDE (XND(I,IL), XND(I,IU), RNLU)
        HA  = XJBR(I)*OALF
        EP1 = PE(I)*OAUL
        EP2 = FE(I)*OAUL
C
        TFB   = ONE+HA+EP1
        TF(I) = GMA(I)*TFB
        TGB   = (PRAT*RNLU*OALF)*(HA+EP2*BETA(I,IUL))
        TG(I) = GMA(I)*TGB
C
        if(PRINT) then
          if((IPRDD.eq.1).or.(mod(I,IPRDD).eq.1)) then
            write (LU,101) I,RNLU,BETA(I,IUL),EP1,EP2,HA,TFB,TF(I),TGB,
     $                       TG(I)
  101       format(' ',I5,1P9E13.5)
          end if
        end if
C
  102 continue
C     !END
      call BYE ('SPROING')
C
      return
      end
