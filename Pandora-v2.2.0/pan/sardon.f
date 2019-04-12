      subroutine SARDON
     $(IMAGE,X,Y,N,SYM,LOG)
C
C     Rudolf Loeser, 1978 Feb 02
C---- Enters points into emergent continuum graphs.
C     LOG=1 means: take log of Y, =0 means: no log.
C     !DASH
      save
C     !DASH
      real*8 X, Y, YI, ZERO
      integer I, LINC, LOG, N
      character IMAGE*(*), SYM*1
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external LINK, HI, BYE
C
C               X(N), Y(N)
      dimension X(*), Y(*)
C
      call HI ('SARDON')
C     !BEG
      LINC = 1
      do 100 I = 1,N
        if((X(I).gt.ZERO).and.(Y(I).gt.ZERO)) then
          if(LOG.eq.1) then
            YI = log10(Y(I))
          else
            YI = Y(I)
          end if
          call LINK     (IMAGE, X(I), YI, SYM, LINC)
        else
          LINC = 1
        end if
  100 continue
C     !END
      call BYE ('SARDON')
C
      return
      end
