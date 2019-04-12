      subroutine PACIJAN
     $(RNAME,DELTA,RCDNAM,INDLEN,I)
C
C     Rudolf Loeser, 1986 Jul 08
C---- Looks up RNAME in RCDNAM, which is SORTED IN ASCENDING ORDER.
C     Returns
C     with I = 0 if NOT FOUND,
C     otherwise
C     with I set such that RCDNAM(I) = RNAME.
C     !DASH
      save
C     !DASH
      real*8 DELTA, RCDNAM, RNAME
      integer I, INDLEN, K, LOOK, NOTE
C     !DASH
      external LOOKSD, HI, BYE
C
C               RCDNAM(INDLEN)
      dimension RCDNAM(*)
C
      call HI ('PACIJAN')
C     !BEG
      I = 0
      if(INDLEN.gt.0) then
        call LOOKSD (RCDNAM,INDLEN,DELTA,RNAME,K,NOTE,LOOK)
        if(LOOK.eq.1) then
          if(NOTE.eq.1) then
            I = K
          end if
        else if(LOOK.eq.2) then
          I = INDLEN
        end if
      end if
C     !END
      call BYE ('PACIJAN')
C
      return
      end
