      subroutine NEGROS
     $(RNAME,RECORD,LENGTH, RCDNAM,DELTA,INDADR,INDLEN)
C
C     Rudolf Loeser, 1986 Jul 08
C---- "Rewrites" a scratch I/O record using a record index.
C     (See detailed remarks in "VISAYAS".)
C     (This is version 2 of NEGROS.)
C
C     RCDNAM must be in ascending sorted order.
C     !DASH
      save
C     !DASH
      real*8 DELTA, RCDNAM, RECORD, RNAME
      integer INDADR, INDLEN, KIND, LENGTH, LUEO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external PANGLAO, PACIJAN, BOHOL, BILIRAN, ABORT, LINER, MESHED,
     $         HI, BYE
C
C               RECORD(Length), RCDNAM(Indlim), INDADR(Indlim)
      dimension RECORD(*),      RCDNAM(*),      INDADR(*)
C
C
      call HI ('NEGROS')
C     !BEG
      call PANGLAO   (LENGTH,'NEGROS')
      call PACIJAN   (RNAME,DELTA,RCDNAM,INDLEN,KIND)
      if(KIND.gt.0) then
        call BOHOL   (RECORD,LENGTH,INDADR(KIND))
      else
        call MESHED  ('NEGROS',1)
        call BILIRAN (LUEO,RCDNAM,INDADR,INDLEN,0,RNAME,DELTA)
        call LINER   (2,LUEO)
        write (LUEO,100) RNAME
  100   format(' ','RNAME =',1PE24.16,', not found in index.')
        call ABORT
      end if
C     !END
      call BYE ('NEGROS')
C
      return
      end
