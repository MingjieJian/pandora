      subroutine FULUM
     $(TE,XNE,XNUU,XNUL,RHOMAX,DMPI)
C
C     Rudolf Loeser, 1991 Jan 30
C---- Computes RHOMAX, for ion broadening of Hydrogen lines.
C     !DASH
      save
C     !DASH
      real*8 C, CON61, RHOMAX, RM1, RM2, RTN, TE, XNE, XNUL, XNUU
      integer LUEO
      logical DMPI
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external  RIGEL, LINER, HI, BYE
      intrinsic min
C
      data C /6.9D0/
C
      call HI ('FULUM')
C     !BEG
      call RIGEL   (61, CON61)
      RTN = sqrt(TE/XNE)
      RM1 = C*RTN
      RM2 = CON61/(XNUU-XNUL)
      RHOMAX = min(RM1,RM2)
C
      if(DMPI) then
        call LINER (1, LUEO)
        write (LUEO,100) TE,XNE,RM1,XNUU,XNUL,RM2,RHOMAX
  100   format(' ',' TE =',1PE14.6,5X,' NE =',E14.6,5X,'RM1 =',E14.6/
     $         ' ','NUU =',  E14.6,5X,'NUL =',E14.6,5X,'RM2 =',E14.6,
     $             5X,'RHOMAX =',E14.6)
      end if
C     !END
      call BYE ('FULUM')
C
      return
      end
