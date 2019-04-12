      subroutine NOTAC
     $(IMAGE,JQ,JX,I,J,DEE,DEEL,LAB,PP)
C
C     Rudolf Loeser, 1998 Oct 30
C---- Enters a d-table into the plot, for ACTON.
C     Only nonzero values of d are significant.
C     !DASH
      save
C     !DASH
      real*8 DEE, DEEL, X, ZERO
      integer I, J, JQ, JX, K, KN, KP, LINC
      logical POINT
      character IMAGE*(*), LAB*3, P*1, PLUS*1, PN*1, PP*1
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(39),PLUS  )
C     !DASH
      external CNVCHRL, LINK, KPLOTC, HI, BYE
C
C               DEE(4,5,N), DEEL(4,5,N)
      dimension DEE(4,5,*), DEEL(4,5,*)
C
      data POINT /.false./
C     !EJECT
C
      call HI ('NOTAC')
C     !BEG
C---- Initialize positive/negative counters
      KN = 0
      KP = 0
C---- Get lower case of plot symbol PP, for negative values
      call CNVCHRL      (PP,PN)
C
C---- Enter values
      LINC = 1
      do 100 K = JQ,JX
        if(DEE(I,J,K).ne.ZERO) then
C
          if(DEE(I,J,K).gt.ZERO) then
C----       Positive value
            KP = KP+1
            P  = PP
          else
C----       Negative value
            KN = KN+1
            P  = PN
          end if
          X = K
C----     Enter line segment into plot
          call LINK     (IMAGE,X,DEEL(I,J,K),P,LINC)
          if(POINT) then
C----       Enter end-point of segment into plot
            call KPLOTC (IMAGE,X,DEEL(I,J,K),PLUS)
          end if
C
        end if
  100 continue
C
C---- Construct label to reflect the symbols actually used
      LAB = '   '
      if(KP.gt.0) then
        if(KN.gt.0) then
          LAB = PP//','//PN
        else
          LAB = PP//'  '
        end if
      else
        if(KN.gt.0) then
          LAB = PN//'  '
        end if
      end if
C     !END
      call BYE ('NOTAC')
C
      return
      end
