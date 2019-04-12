      subroutine POCO
     $(RNAME,DELTA,RCDNAM,INDLEN,I)
C
C     Rudolf Loeser, 1986 Jul 08
C---- Looks up RNAME in RCDNAM, which is UNSORTED.
C     Returns
C     with I=0 if NOT FOUND,
C     otherwise
C     with I set such that RCDNAM(I) = RNAME.
C     !DASH
      save
C     !DASH
      real*8 DELTA, RCDNAM, RNAME
      integer I, INDLEN, K, LOOK
C     !DASH
      external LOOKUD, HI, BYE
C
C               RCDNAM(Indlen)
      dimension RCDNAM(*)
C
      call HI ('POCO')
C     !BEG
      I = 0
      if(INDLEN.gt.0) then
        call LOOKUD (RCDNAM,INDLEN,DELTA,RNAME,K,LOOK)
        if(LOOK.eq.1) then
          I = K
        end if
      end if
C     !END
      call BYE ('POCO')
C
      return
      end
